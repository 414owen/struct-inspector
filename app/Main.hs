{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<|>))
import Control.Applicative.MultiExcept
import Control.Arrow (first)
import qualified Data.ByteString as B
import qualified Data.DList.NonEmpty as DNE
import Data.DList.NonEmpty (NonEmptyDList)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Language.C
import Language.C.Data.Ident

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
  deriving Show

data Struct
  = Struct
  { name :: String
  , fields :: [Field]
  } deriving Show

data QueryErr
  = FieldWithoutType String
  | NonStructFieldWithoutName
  deriving Show

type QM = MultiExcept QueryErr

main :: IO ()
main = do
  input <- B.getContents
  let ast = parseC input $ initPos "stdin"
  case ast of
    Left err -> print err
    Right ast -> print $ getStructsFromTranslUnit ast

mkDebugger :: Struct -> String
mkDebugger (Struct s fields)
  =  "int main(void) {"
  <> "  printf(\"sizeof " <> s <> ": %zu\n\", sizeof(" <> s <> "));"
  <> "}"

getStructsFromTranslUnit :: CTranslUnit -> QM [Struct]
getStructsFromTranslUnit (CTranslUnit decls _) = concat <$> traverse getStructsFromExtDecl decls
getStructsFromTranslUnit _ = pure []

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
    Just t -> case names of
      [] -> throwError $ NonStructFieldWithoutName
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
