{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main (main) where

import           Control.Arrow (second)
import           Control.Applicative             (Alternative(..), (<|>), (<**>))
import           Control.Applicative.MultiExcept
import           Control.Exception               (bracket)
import           Control.Monad                   (unless)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.UTF8            as BSU
import qualified Data.DList.NonEmpty             as DNE
import           Data.Foldable                   (traverse_)
import           Data.List                       (scanl', foldl', intercalate)
import           Data.List.NonEmpty              (NonEmpty(..))
import           Data.Maybe                      (catMaybes, isJust)
import           Language.C
import           Language.C.Data.Ident
import           Options.Applicative             (Parser)
import qualified Options.Applicative             as O
import           System.Directory                (getCurrentDirectory)
import           System.Environment
import           System.FilePath                 ((</>))
import           System.IO
import           System.Process
import qualified Text.PrettyPrint                as PP

data Options
  = Options
  { sizeof           :: Bool
  , uses             :: Bool
  , padding          :: Bool
  , noColor          :: Bool
  , inFile           :: FilePath
  , nonZeroOnPadding :: Bool
  } deriving Show

data Handles
  = Handles
  { input  :: Handle
  , output :: Handle
  }

data Env
  = Env
  { options :: Options
  , handles :: Handles
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> O.switch
    ( O.long "sizeof"
    <> O.short 's'
    <> O.help "Output size of fields (excluding padding)")
  <*> O.switch
    ( O.long "uses"
    <> O.short 'u'
    <> O.help "Output bytes used by field (including padding)")
  <*> O.switch
    ( O.long "padding"
    <> O.short 'p'
    <> O.help "Output bytes of padding that follow a field")
  <*> O.switch
    ( O.long "no-color"
    <> O.short 'n'
    <> O.help "Don't output color")
  <*> O.strOption
    ( O.long "input"
    <> O.short 'i'
    <> O.metavar "FILE"
    <> O.help "Input file (default is stdin)"
    <> O.value "-"
    )
  <*> O.switch
    ( O.long "nonzero-on-padding"
    <> O.short 'z'
    )

getOptions :: IO Options
getOptions = O.execParser optInfo

optInfo :: O.ParserInfo Options
optInfo = O.info (optionsParser <**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Print structure padding and field size info"
  <> O.header "Struct Inspector" )

failWithHelp :: B.ByteString -> IO a
failWithHelp str = do
  B.putStr "Error: "
  B.putStr str
  B.putStr "\n\n"
  O.handleParseResult . O.Failure $ O.parserFailure pprefs optInfo (O.ShowHelpText Nothing) mempty

anyInfo :: Options -> Bool
anyInfo Options{..} = or debug
  where
    debug :: [Bool]
    debug = [padding, uses, sizeof]

pprefs :: O.ParserPrefs
pprefs = O.prefs mempty

environmentFromOptions :: Options -> IO Env
environmentFromOptions options@Options{inFile} = do
  unless (anyInfo options) $
    failWithHelp "Please specify at least one of '--padding', '--uses', or '--sizeof'"
  let output = stdout
  input <- case inFile of
    "-" -> pure stdin
    "" -> failWithHelp "Empty input file name"
    _ -> openFile inFile ReadMode
  let handles = Handles{..}
  pure $ Env{..}

createEnvironment :: IO Env
createEnvironment = getOptions >>= environmentFromOptions

data Field
  = FieldNamed
    { name :: String
    , declr :: CDeclr
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

main :: IO ()
main = createEnvironment >>= run

cFile :: FilePath
cFile = ".struct.debug.c"

ifThenElse :: Bool -> a -> a -> a
ifThenElse c a b = if c then a else b

cPrelude :: Env -> B.ByteString
cPrelude Env{options = Options{..}} = B.intercalate "\n"
  $ colors <>
  [ "#include <stdio.h>"
  , "// End of prelude\n\n"
  ]
  where
    colors :: [B.ByteString]
    colors
      = ("#define " <>)
      . (uncurry (<>))
      . second (" " <>)
      . (second $ ifThenElse noColor "\"\"")
      <$>
      [ ("RESET", "\"\\033[0m\"")
      , ("BLACK", "\"\\033[30m\"")
      , ("RED", "\"\\033[31m\"")
      , ("GREEN", "\"\\033[32m\"")
      , ("YELLOW", "\"\\033[33m\"")
      , ("BLUE", "\"\\033[34m\"")
      , ("MAGENTA", "\"\\033[35m\"")
      , ("CYAN", "\"\\033[36m\"")
      , ("WHITE", "\"\\033[37m\"")
      , ("BOLDBLACK", "\"\\033[1m\\033[30m\"")
      , ("BOLDRED", "\"\\033[1m\\033[31m\"")
      , ("BOLDGREEN", "\"\\033[1m\\033[32m\"")
      , ("BOLDYELLOW", "\"\\033[1m\\033[33m\"")
      , ("BOLDBLUE", "\"\\033[1m\\033[34m\"")
      , ("BOLDMAGENTA", "\"\\033[1m\\033[35m\"")
      , ("BOLDCYAN", "\"\\033[1m\\033[36m\"")
      , ("BOLDWHITE", "\"\\033[1m\\033[37m\"")
      ]

getEnvDef :: String -> String -> IO String
getEnvDef def env = flip fmap (lookupEnv env) $ \case
  Nothing -> def
  Just a -> a

run :: Env -> IO ()
run env@Env{handles = Handles{..}} = do
  source <- hGetContents input
  cc <- getEnv "CC"
  dir <- getCurrentDirectory
  -- putStrLn $ "Using C compiler ($CC): " <> cc
  cpath <- getEnvDef "" "CPATH"
  includePath <- getEnvDef "" "C_INCLUDE_PATH"
  preprocessed <- readProcess "cc" ["-std=c11", "-E", "-", "-I" <> cpath, "-I" <> includePath] source
  case parseC (BSU.fromString preprocessed) $ initPos "stdin" of
    Left err -> hPrint stderr err
    Right ast -> case runMultiExcept $ getStructsFromTranslUnit ast of
      Left errs -> traverse_ (hPrint stderr) errs
      Right structs -> do
        bracket (openFile cFile WriteMode) hClose $ \handle -> do
          B.hPut handle $ cPrelude env
          hPutStrLn handle source
          hPutStrLn handle $ mkDebugger env structs
        callCommand $ intercalate " "
          [ cc
          , "-O0"
          , "-s"
          , cFile
          ]
        callProcess (dir </> "a.out") []


mkDebugger :: Env -> [Struct] -> String
mkDebugger env@Env{options = Options{..}} structs
  =  "int main(void) {\n"
  <> strif nonZeroOnPadding "  int __exit_code = 0;\n"
  <> out
  <> concatMap (debugStruct env) structs
  <> strif nonZeroOnPadding "  return __exit_code;\n"
  <> "}"
  where
    out :: String
    out 
      =  strif (indirect env)
      $  "  FILE *__out = stdout;\n"
      <> "  char *__struct_out_string;\n"
      <> "  size_t __struct_out_size;\n"

structName :: String
structName = "__s"

debugPrint :: Bool
debugPrint = False

strif :: Bool -> String -> String
strif b s = if b then s else ""

indirect :: Env -> Bool
indirect Env{options = Options{..}} = padding

{- The best Haskell printf implementation isn't some type level
 - tomfoolery, it's to shell out to C. Thanks for coming to my
 - Ted Talk.
 -}
printf :: Env -> Int -> String -> [String] -> String
printf env n s args = replicate n ' ' <> start <> argsStr <> ");\n"
  where
    argsStr :: String
    argsStr = show s <> ", " <> intercalate ", " args

    start :: String
    start = if indirect env
      then "fprintf(__out, "
      else "printf("

puts :: Env -> Int -> String -> String
puts env n s = replicate n ' ' <> start <> show s <> ");\n"
  where
    start :: String
    start = if indirect env
      then "fprintf(__out, \"%s\\n\", "
      else "puts("

printSizePaths :: Env -> Int -> String -> String -> String
printSizePaths env@Env{options = Options{..}} n path nextPath
   =  "    {\n"
   <> "      size_t __size_of = sizeof(" <> path <> ");\n"
   <> "      size_t __uses    = ((size_t)&" <> nextPath <> ") - ((size_t)&" <> path <> ");\n"
   <> "      size_t __padding = __uses - __size_of;\n"
   <> strif padding "      __struct_total_padding += __padding;\n"
   <> exitPadding
   <> printSizeof
   <> printUses
   <> printPadding
   <> db
   <> "    }\n"
  where
    db :: String
    db = if debugPrint
      then printIndent env 4 n
        <> puts env 4 ("// path: " <> path <> "\n")
        <> printIndent env 4 n
        <> puts env 4 ("// nextPath: " <> nextPath <> "\n")
      else ""

    printSizeof :: String
    printSizeof
      =  strif sizeof
      $  printIndent env 6 n
      <> printf env 6 "// sizeof: %zu\n" ["__size_of"]

    printUses :: String
    printUses
      =  strif uses
      $  printIndent env 6 n
      <> printf env 6 "// uses: %zu\n" ["__uses"]

    printPadding :: String
    printPadding
      = strif padding
      $  printIndent env 6 n
      <> printf env 6 "// padding: %s%zu%s\n" ["__padding > 0 ? RED : GREEN", "__padding", "RESET"]

    exitPadding :: String
    exitPadding
      = strif nonZeroOnPadding
      $ "      if (__padding > 0) { __exit_code = 1; }\n"

debugStruct' :: Env -> Bool -> String -> [Field] -> String
debugStruct' env@Env{options = Options{..}} istypedef s fields
  =  "  {\n"
  <> strif padding "    size_t __struct_total_padding = 0;\n"
  <> out
  <> decl
  <> "    printf(\"// sizeof: %zu\\n\", sizeof(" <> path <> "));\n"
  <> start
  <> printSubs env 0 path nextPath fields
  <> end
  <> total
  <> strif (indirect env) "    putc(0, __out);\n"
  <> strif (indirect env) "    fclose(__out);\n"
  <> strif (indirect env) "    printf(\"%s\", __struct_out_string);\n"
  <> "    putc('\\n', stdout);\n"
  <> "  }\n"
  where
    out :: String
    out
      =  strif padding
      $  "    __out = open_memstream(&__struct_out_string, &__struct_out_size);\n"

    path :: String
    path = structName <> "[0]"

    nextPath :: String
    nextPath = structName <> "[1]"

    decl :: String
    decl = if istypedef
      then ("    " <> s <> " " <> structName <> "[2];\n")
      else ("    struct " <> s <> " " <> structName <> "[2];\n")

    start :: String
    start = puts env 4 $ if istypedef
      then "typedef struct {"
      else "struct " <> s <> " {"

    end :: String
    end = puts env 4 $ if istypedef
      then "} " <> s <> ";"
      else "};"

    total :: String
    total = strif padding
      $  "    printf(\"// total struct padding: %s%zu%s\\n\""
      <> ", __struct_total_padding > 0 ? RED : GREEN"
      <> ", __struct_total_padding"
      <> ", RESET);\n"

debugStruct :: Env -> Struct -> String
debugStruct env (Struct s fs) = debugStruct' env False s fs
debugStruct env (TypedefStruct s fs) = debugStruct' env True s fs

printIndent :: Env -> Int -> Int -> String
printIndent _ _ 0 = ""
printIndent env n m = replicate n ' ' <>
  if indirect env
  then "fprintf(__out, \"%" <> show m <> "s\", \"\");\n"
  else "printf(\"%" <> show m <> "s\", \"\");\n"

altList :: (Foldable f, Alternative a) => f (a b) -> a b
altList = foldl' (<|>) empty

altMap :: (Functor f, Foldable f, Alternative a) => (b -> a c) -> f b -> a c
altMap f = altList . fmap f

firstPath :: Field -> Maybe String
firstPath = \case
  FieldNamed{..} -> pure name
  FieldAnonymousStruct{..} -> altMap firstPath inner
  FieldAnonymousUnion{..} -> altMap firstPath inner
  FieldNamedStruct{..} -> pure name
  FieldNamedUnion{..} -> pure name

(<.>) :: String -> String -> String
(<.>) a b = a <> "." <> b

printSubs :: Env -> Int -> String -> String -> [Field] -> String
printSubs env n path nextPath fields =
  concat $ zipWith (flip (debugField env (n + 2) path)) fields nexts
  where
    nexts :: [String]
    nexts = reverse $ scanl' f nextPath $ reverse $ tail $ fieldFirsts

    f :: String -> Maybe String -> String
    f _ (Just a) = a
    f a Nothing = a

    fieldFirsts :: [Maybe String]
    fieldFirsts = fmap (path <.>) . firstPath <$> fields

printSubsUnion :: Env -> Int -> String -> String -> [Field] -> String
printSubsUnion env n path nextPath fields =
  concatMap (debugField env (n + 2) path nextPath) fields

debugField :: Env -> Int -> String -> String -> Field -> String
debugField env n path nextPath = \case
  (FieldNamed name declr ts) ->
    let newPath = path <.> name
     in printSizePaths env n newPath nextPath
       <> printIndent env 4 n
       <> puts env 4 (PP.render (pretty ts) <> " " <> PP.render (pretty declr) <> ";")
  FieldAnonymousStruct{..} ->
    printIndent env 4 n
    <> puts env 4 "struct {"
    <> printSubs env n path nextPath inner
    <> printIndent env 4 n
    <> puts env 4 "};"
  FieldNamedStruct{..} ->
    printIndent env 4 n
    <> puts env 4 "struct {"
    <> printSubs env n (path <.> name) nextPath inner
    <> printIndent env 4 n
    <> puts env 4 ("} " <> name <> ";")
  FieldAnonymousUnion{..} ->
    printIndent env 4 n
    <> puts env 4 "union {"
    <> printSubsUnion env n path nextPath inner
    <> printIndent env 4 n
    <> puts env 4 "};"
  FieldNamedUnion{..} ->
    printIndent env 4 n
    <> puts env 4 "union {"
    <> printSubsUnion env n (path <.> name) nextPath inner
    <> printIndent env 4 n
    <> puts env 4 ("} " <> name <> ";")

getStructsFromTranslUnit :: CTranslUnit -> QM [Struct]
getStructsFromTranslUnit (CTranslUnit decls _) = concat <$> traverse getStructsFromExtDecl decls

getStructsFromExtDecl :: CExtDecl -> QM [Struct]
getStructsFromExtDecl (CDeclExt a) = getStructsFromDecl a
getStructsFromExtDecl _ = pure []

getStructsFromDecl :: CDeclaration NodeInfo -> QM [Struct]
getStructsFromDecl (CDecl specs decs _) =
  concat <$> traverse (flip getStructsFromDeclSpec $ getTypedef specs decs) specs
getStructsFromDecl _ = pure []

getTypedef :: [CDeclSpec] -> [(Maybe CDeclr, b, c)] -> Maybe String
getTypedef specs decls
  | any isTd specs = altMap getName $ catMaybes $ fst3 <$> decls
  | otherwise = Nothing
  where
    isTd :: CDeclSpec -> Bool
    isTd (CStorageSpec (CTypedef _)) = True
    isTd _ = False

    getName :: CDeclr -> Maybe String
    getName (CDeclr (Just (Ident s _ _)) _ _ _ _) = Just s
    getName _ = Nothing

getStructsFromDeclSpec :: CDeclSpec -> Maybe String -> QM [Struct]
getStructsFromDeclSpec (CTypeSpec a) = getStructsFromCTypeSpec a
getStructsFromDeclSpec _ = const $ pure []

getStructsFromCTypeSpec :: CTypeSpec -> Maybe String -> QM [Struct]
getStructsFromCTypeSpec (CSUType su _) = getStructsFromCSU su
getStructsFromCTypeSpec _ = const $ pure []

hasParent :: NodeInfo -> Bool
hasParent = \case
  OnlyPos pos _ -> f pos
  NodeInfo pos _ _ -> f pos
  where
    f :: Position -> Bool
    f pos = isJust $ posParent pos

getStructsFromCSU :: CStructUnion -> Maybe String -> QM [Struct]
getStructsFromCSU (CStruct _ _ _ _ ni) _
  | hasParent ni = pure []
getStructsFromCSU (CStruct CStructTag _ Nothing _ _) (Just s) = pure $ [TypedefStruct s []]
getStructsFromCSU (CStruct CStructTag (Just (Ident s _ _)) Nothing _ _) Nothing = pure $ [Struct s []]
getStructsFromCSU (CStruct CStructTag (Just (Ident s _ _)) (Just decls) _ _) Nothing
  = pure . Struct s <$> concat <$> traverse getFieldsFromDecl decls
getStructsFromCSU (CStruct CStructTag _ (Just decls) _ _) (Just s)
  = pure . TypedefStruct s <$> concat <$> traverse getFieldsFromDecl decls
getStructsFromCSU _ _ = pure []

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

getFieldsFromDecl :: CDeclaration NodeInfo -> QM [Field]
getFieldsFromDecl (CDecl specs trips _) =
  let pairs = concatMap (getFieldsFromDeclarators . fst3) trips
      (names, _) = unzip pairs
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
    Just (CSUType (CStruct CUnionTag _ Nothing _ _) _) -> pure $ case names of
      [] -> [FieldAnonymousUnion []]
      ns -> flip FieldNamedUnion [] <$> ns
    Just (CSUType (CStruct CUnionTag _ (Just decls) _ _) _) -> do
      fields <- concat <$> traverse getFieldsFromDecl decls
      pure $ case names of
        [] -> [FieldAnonymousUnion fields]
        ns -> flip FieldNamedUnion fields <$> ns
    Just t -> case pairs of
      [] -> throwError $ NonStructFieldWithoutName
      ns -> pure $ uncurry FieldNamed <$> ns <*> pure t
getFieldsFromDecl _ = pure []

getTypeFromDeclSpecs :: [CDeclarationSpecifier NodeInfo] -> Maybe CTypeSpec
getTypeFromDeclSpecs = foldl' (<|>) Nothing . fmap getTypeFromDeclSpec

getTypeFromDeclSpec :: CDeclarationSpecifier NodeInfo -> Maybe CTypeSpec
getTypeFromDeclSpec (CTypeSpec a) = Just a
getTypeFromDeclSpec _ = Nothing

getFieldsFromDeclarators :: Maybe (CDeclarator NodeInfo) -> [(String, CDeclr)]
getFieldsFromDeclarators (Just declr@(CDeclr (Just (Ident s _ _)) _ _ _ _)) = [(s, declr)]
getFieldsFromDeclarators _ = []
