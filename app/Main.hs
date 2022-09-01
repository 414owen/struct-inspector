{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Applicative             (Alternative(..), (<|>))
import           Control.Applicative.MultiExcept
import           Control.Arrow                   (first)
import           Control.Exception               (bracket)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.UTF8            as BSU
import qualified Data.DList.NonEmpty             as DNE
import           Data.DList.NonEmpty             (NonEmptyDList)
import           Data.Foldable                   (traverse_)
import           Data.List                       (scanl', foldl', intercalate)
import           Data.List.NonEmpty              (NonEmpty(..))
import           Language.C
import           Language.C.Data.Ident
import           System.Directory                (getCurrentDirectory)
import           System.Environment
import           System.FilePath                 ((</>))
import           System.IO
import           System.Process
import qualified Text.PrettyPrint                as PP

import Debug.Trace

data Field
  = FieldNamed
    { name :: String
    , typeSpec :: CTypeSpec
    }
  | FieldAnonymousStruct
    { inner :: [Field]
    }
  | FieldNamedStruct
    { name :: String
    , inner :: [Field]
    }
  | FieldAnonymousUnion
    { inner :: [Field]
    }
  | FieldNamedUnion
    { name :: String
    , inner :: [Field]
    }
  deriving Show

data Struct
  = Struct
    { name :: String
    , fields :: [Field]
    }
  -- TODO
  | TypedefStruct
    { name :: String
    , fields :: [Field]
    }
  deriving Show

data QueryErr
  = FieldWithoutType String
  | NonStructFieldWithoutName
  deriving Show

type QM = MultiExcept QueryErr

data Handles
  = Handles
  { input  :: Handle
  , output :: Handle
  }

main :: IO ()
main = run $ Handles stdin stdout

cFile :: FilePath
cFile = ".struct.debug.c"

cPrelude :: B.ByteString
cPrelude = "#include <stdio.h>\n\n"
  <> "// End of prelude\n\n"

run :: Handles -> IO ()
run Handles{..} = do
  source <- hGetContents input
  cc <- getEnv "CC"
  dir <- getCurrentDirectory
  putStrLn $ "Using C compiler ($CC): " <> cc
  preprocessed <- readProcess "cc" ["-std=c11", "-E", "-"] source
  let ast = parseC (BSU.fromString preprocessed) $ initPos "stdin"
  case ast of
    Left err -> hPrint stderr err
    Right ast -> case runMultiExcept $ getStructsFromTranslUnit ast of
      Left errs -> traverse_ (hPrint stderr) errs
      Right structs -> do
        bracket (openFile cFile WriteMode) hClose $ \handle -> do
          B.hPut handle cPrelude
          hPutStrLn handle source
          hPutStrLn handle $ mkDebugger structs
        callCommand $ intercalate " "
          [ cc
          , cFile
          ]
        callProcess (dir </> "a.out") []

mkDebugger :: [Struct] -> String
mkDebugger structs
  =  "int main(void) {\n"
  <> concatMap debugStruct structs
  <> "}"

structName :: String
structName = "__s"

debugPrint :: Bool
debugPrint = False

printSizePaths :: Int -> String -> String -> String
printSizePaths n path nextPath
   =  printIndent n
   <> "    printf(\"// sizeof: %zu\\n\", sizeof(" <> path <> "));\n"
   <> printIndent n
   <> "    printf(\"// uses: %zu\\n\", ((size_t)&" <> nextPath <> ") - ((size_t)&" <> path <> "));\n"
   <> if not debugPrint then "" else db
  where
    db :: String
    db = printIndent n
      <> "    printf(\"// path: " <> path <> "\\n\");\n"
      <> printIndent n
      <> "    printf(\"// nextPath: " <> nextPath <> "\\n\");\n"

debugStruct :: Struct -> String
debugStruct (Struct s fields)
  =  "  {\n"
  <> "    struct " <> s <> " " <> structName <> "[2];\n"
  <> printSizePaths 0 path nextPath
  <> "    puts(\"struct " <> s <> " {\");\n"
  <> printSubs 0 path nextPath fields
  <> "    puts(\"};\\n\");\n"
  <> "  }\n"
  where
    path :: String
    path = structName <> "[0]"

    nextPath :: String
    nextPath = structName <> "[1]"

printIndent :: Int -> String
printIndent 0 = ""
printIndent n = "    printf(\"%" <> show n <> "s\", \"\");\n"

altList :: (Foldable f, Alternative a) => f (a b) -> a b
altList = foldl' (<|>) empty

altMap :: (Functor f, Foldable f, Alternative a) => (b -> a c) -> f b -> a c
altMap f = altList . fmap f

firstPath :: Field -> Maybe String
firstPath = \case
  FieldNamed{..} -> pure name
  FieldAnonymousStruct{..} -> altMap firstPath inner
  FieldNamedStruct{..} -> pure name

(<.>) :: String -> String -> String
(<.>) a b = a <> "." <> b

printSubs :: Int -> String -> String -> [Field] -> String
printSubs n path nextPath fields =
  concat $ zipWith (flip (debugField (n + 2) path)) fields nexts
  where
    nexts :: [String]
    nexts = reverse $ scanl' f nextPath $ reverse $ tail $ fieldFirsts

    f :: String -> Maybe String -> String
    f _ (Just a) = a
    f a Nothing = a

    fieldFirsts :: [Maybe String]
    fieldFirsts = fmap (path <.>) . firstPath <$> fields

debugField :: Int -> String -> String -> Field -> String
debugField n path nextPath = \case
  (FieldNamed name ts) ->
    let newPath = path <.> name
     in printSizePaths n newPath nextPath
       <> printIndent n
       <> "    printf(\"" <> PP.render (pretty ts) <> " " <> name <> ";\\n\");"
  FieldAnonymousStruct{..} ->
    printIndent n
    <> "    puts(\"struct {\");\n"
    <> printSubs n path nextPath inner
    <> "    puts(\"};\");\n"
  FieldNamedStruct{..} ->
    printIndent n
    <> "    puts(\"struct {\");\n"
    <> printSubs n (path <.> name) nextPath inner
    <> printIndent n
    <> "    puts(\"} " <> name <> ";\");\n"

getStructsFromTranslUnit :: CTranslUnit -> QM [Struct]
getStructsFromTranslUnit (CTranslUnit decls _) = concat <$> traverse getStructsFromExtDecl decls

getStructsFromExtDecl :: CExtDecl -> QM [Struct]
getStructsFromExtDecl (CDeclExt a) = getStructsFromDecl a
getStructsFromExtDecl _ = pure []

getStructsFromDecl :: CDeclaration NodeInfo -> QM [Struct]
getStructsFromDecl (CDecl a _ _) = concat <$> traverse getStructsFromDeclSpec a
getStructsFromDecl _ = pure []

getStructsFromDeclSpec :: CDeclarationSpecifier NodeInfo -> QM [Struct]
getStructsFromDeclSpec (CTypeSpec a) = getStructsFromCTypeSpec a
getStructsFromDeclSpec _ = pure []

getStructsFromCTypeSpec :: CTypeSpec -> QM [Struct]
getStructsFromCTypeSpec (CSUType su _) = getStructsFromCSU su
getStructsFromCTypeSpec _ = pure []

getStructsFromCSU :: CStructureUnion NodeInfo -> QM [Struct]
getStructsFromCSU (CStruct CStructTag (Just (Ident s _ _)) Nothing _ _) = pure $ [Struct s []]
getStructsFromCSU (CStruct CStructTag (Just (Ident s _ _)) (Just decls) _ _)
  = pure . Struct s <$> concat <$> traverse getFieldsFromDecl decls
getStructsFromCSU _ = pure []

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

getFieldsFromDecl :: CDeclaration NodeInfo -> QM [Field]
getFieldsFromDecl (CDecl specs trips _) =
  let names = concatMap (getFieldsFromDeclarators . fst3) trips
      typeNode = getTypeFromDeclSpecs specs
  in case typeNode of
    Nothing -> case names of
    -- I really hope this never happens
      [] -> throwError $ FieldWithoutType []
      x : xs -> throwErrors
        $ DNE.fromNonEmpty
        $ FieldWithoutType <$> (x :| xs)
    -- TODO test whether we need struct name here
    Just (CSUType (CStruct CStructTag _ Nothing _ _) _) -> pure $ case names of
      [] -> [FieldAnonymousStruct []]
      ns -> flip FieldNamedStruct [] <$> ns
    Just (CSUType (CStruct CStructTag _ (Just decls) _ _) _) -> do
      fields <- concat <$> traverse getFieldsFromDecl decls
      pure $ case names of
        [] -> [FieldAnonymousStruct fields]
        ns -> flip FieldNamedStruct fields <$> ns
    Just (CSUType (CStruct CUnionTag _ (Just decls) _ _) _) -> do
      fields <- concat <$> traverse getFieldsFromDecl decls
      pure $ case names of
        [] -> [FieldAnonymousStruct fields]
        ns -> flip FieldNamedStruct fields <$> ns
    Just t -> case names of
      [] -> trace (show t) $ throwError $ NonStructFieldWithoutName
      ns -> pure $ flip FieldNamed t <$> ns
getFieldsFromDecl _ = pure []

getTypeFromDeclSpecs :: [CDeclarationSpecifier NodeInfo] -> Maybe CTypeSpec
getTypeFromDeclSpecs = foldl' (<|>) Nothing . fmap getTypeFromDeclSpec

getTypeFromDeclSpec :: CDeclarationSpecifier NodeInfo -> Maybe CTypeSpec
getTypeFromDeclSpec (CTypeSpec a) = Just a
getTypeFromDeclSpec _ = Nothing

getFieldsFromDeclarators :: Maybe (CDeclarator NodeInfo) -> [String]
getFieldsFromDeclarators (Just (CDeclr (Just (Ident s _ _)) _ _ _ _)) = [s]
getFieldsFromDeclarators _ = []
