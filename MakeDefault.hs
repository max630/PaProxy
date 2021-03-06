#!/usr/bin/runghc
module MakeDefault(main) where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.List (nubBy, isPrefixOf, isSuffixOf, intersperse)
import Data.Maybe (mapMaybe)
import Language.C (parseCFile)
import Language.C.Syntax.AST (CTypeSpecifier(CCharType, CDoubleType, CFloatType, CIntType, CSUType, CTypeDef, CUnsigType, CVoidType),
                              CTypeQualifier(CConstQual),
                              CTranslationUnit(CTranslUnit),
                              CStructureUnion(CStruct),
                              CStructTag(CStructTag),
                              CStatement(CCompound, CExpr),
                              CFunctionDef(CFunDef),
                              CExternalDeclaration(CDeclExt, CFDefExt),
                              CExpression(CCall, CVar),
                              CDerivedDeclarator(CFunDeclr, CPtrDeclr),
                              CDeclarator(CDeclr),
                              CDeclarationSpecifier(CTypeQual, CTypeSpec),
                              CDeclaration(..),
                              CCompoundBlockItem(CBlockStmt))
import Language.C.System.GCC (newGCC)
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Data.Ident (Ident(Ident), internalIdent)
import Language.C.Data.Position (posFile, posRow, posColumn)
import Language.C.Parser (ParseError(ParseError))
import Language.C.Pretty (Pretty(pretty))
import System.Directory (removeFile, getDirectoryContents, getTemporaryDirectory)
import System.IO (IOMode(WriteMode), withBinaryFile, openBinaryTempFile, hPutStrLn, hClose)
import GHC.Exts (sortWith)

import qualified Data.Set as S

-- TODO:
-- + scan whole pulse directory
-- + make default implementation
-- + make it skip existing functions
-- + prepare compilable output
-- * check linkage, add more exports if necessary
-- * implement applications minimum
--   * simple loop
--   * real sound stuff
--   * threaded loop
--   * paplay
--   * skype
--   * parecord
-- * alsa output
-- * packaging
--   * side libs - simple, glib
--   * debian
--   * others (expect contribs)

data ReturnType = RTInt | RTPChar deriving (Eq, Ord, Show)

main = do
  definedFunctions <- S.fromList <$> getDefinedFunctions
  (includeFunctions, includes) <- processIncludes
  withBinaryFile "defaults.c" WriteMode (\defaults -> do
    printHeader defaults includes
    mapM_ (printFun defaults)
          (filter (not . (`S.member` definedFunctions) . declName)
                  includeFunctions))
  withBinaryFile "map-libpulse" WriteMode (\mapfile -> do
    hPutStrLn mapfile "PULSE_0 {"
    hPutStrLn mapfile "global:"
    mapM_ (\iname -> hPutStrLn mapfile (iname ++ ";"))
          (filter (`notElem` ["pa_encoding_from_string"]) $ map declName includeFunctions)
    hPutStrLn mapfile "pa_format_info_free2;"
    hPutStrLn mapfile "local:"
    hPutStrLn mapfile "*;"
    hPutStrLn mapfile "};")
  where
    printFun defaults decl@(CDecl spec1 [(Just (CDeclr (Just (Ident name _ _)) spec2 _ _ _), Nothing, Nothing)] _) = do
      implName <- case (spec1, spec2) of
          ([CTypeSpec (CVoidType _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_VOID"
          ([CTypeSpec (CIntType _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CUnsigType _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CFloatType _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CDoubleType _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "uint32_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "int64_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "size_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "pa_usec_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "pa_channel_position_mask_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_CHANNEL_POSITION_MASK"
          ([CTypeSpec (CTypeDef (Ident "pa_channel_position_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_CHANNEL_POSITION"
          ([CTypeSpec (CTypeDef (Ident "pa_context_state_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_CONTEXT_STATE"
          ([CTypeSpec (CTypeDef (Ident "pa_volume_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_VOLUME"
          ([CTypeSpec (CTypeDef (Ident "pa_encoding_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_ENCODING"
          ([CTypeSpec (CTypeDef (Ident "pa_prop_type_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_PROP_TYPE"
          ([CTypeSpec (CTypeDef (Ident "pa_operation_state_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_OPERATION_STATE"
          ([CTypeSpec (CTypeDef (Ident "pa_sample_format_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_SAMPLE_FORMAT"
          ([CTypeSpec (CTypeDef (Ident "pa_stream_state_t" _ _) _)], [CFunDeclr _ _ _]) -> return "PAP_DEFAULT_STREAM_STATE"
          ([CTypeSpec (CVoidType _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "PAP_DEFAULT_NULL"
          ([CTypeSpec (CCharType _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "PAP_DEFAULT_NULL"
          ([CTypeQual (CConstQual _), CTypeSpec (CCharType _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "PAP_DEFAULT_NULL"
          ([CTypeSpec (CTypeDef (Ident paTypeName _ _) _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) | "pa_" `isPrefixOf` paTypeName -> return "PAP_DEFAULT_NULL"
          ([CTypeQual (CConstQual _), CTypeSpec (CTypeDef (Ident paTypeName _ _) _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) | "pa_" `isPrefixOf` paTypeName -> return "PAP_DEFAULT_NULL"
          ([CTypeSpec (CSUType (CStruct CStructTag (Just (Ident "timeval" _ _)) _ _ _) _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "PAP_DEFAULT_NULL"
          _ -> fail ("Unrecognized: " ++ show (pretty decl) ++ "\n" ++ show (fmap (const ()) decl))
      hPutStrLn defaults $ show $ pretty $ fmap (const undefNode) $ tryFun2 name implName spec1u spec2u
      where
        spec1u = map (fmap (const ())) spec1
        spec2u = map (fmap (const ())) spec2

printHeader defaults includes = do
  mapM_ (\inc -> hPutStrLn defaults ("#include <pulse/" ++ inc ++ ">")) includes
  hPutStrLn defaults ""
  hPutStrLn defaults "#include \"default_macros.h\""
  hPutStrLn defaults ""

getDefinedFunctions = do
  sources <- filter (\fn -> isSuffixOf ".c" fn && fn /= "defaults.c") <$> getDirectoryContents "."
  concatMap (mapMaybe declMbFunName) <$> mapM readCFile sources

processIncludes = do
  includes <- filter (`notElem` [".", "..", "glib-mainloop.h"]) <$> getDirectoryContents "/usr/include/pulse"
  (flip (,) includes) <$> nubWith declName . sortWith declName . concat <$> mapM getFunctions includes

declName (CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _)] _) = name

getFunctions :: String -> IO [CDeclaration NodeInfo]
getFunctions file = do
  tempDir <- getTemporaryDirectory
  bracket
    (createTmpSource tempDir file)
    removeFile
    (\tmpSource -> do
      decls <- readCFile tmpSource
      return [ decl | (CDeclExt decl@(CDecl _ [(Just (CDeclr (Just (Ident name _ _)) (CFunDeclr _ _ _ : _) _ _ _), Nothing, Nothing)] _)) <- decls,
                      "pa_" `isPrefixOf` name && not ("pa_simple_" `isPrefixOf` name) ])

readCFile src = do
  res <- parseCFile (newGCC "gcc") Nothing [] src
  case res of
    Left (ParseError (errors, pos)) -> do
      putStrLn (posFile pos ++ ":" ++ show (posRow pos) ++ ":0: " ++ concat (intersperse "\n" errors))
      fail "Parsing failed"
    Right (CTranslUnit decls _) -> return decls

tuDecls (CTranslUnit decls _) = decls

declMbFunName (CFDefExt (CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ _ _)) = Just name
declMbFunName _ = Nothing

createTmpSource :: String -> String -> IO String
createTmpSource tempDir file = do
  -- FIXME: this leaves file in case of error
  (path, handle) <- openBinaryTempFile tempDir "pulse_2include_XXXXXXXX.c"
  hPutStrLn handle ("#include <pulse/" ++ file ++ ">")
  hClose handle
  return path

tryFun2 funName macroName spec1 spec2 =
  fmap (const undefNode) (CFunDef spec1
                                  (CDeclr (Just $ internalIdent funName) spec2 Nothing [] ())
                                  []
                                  (CCompound [] [CBlockStmt (CExpr (Just (CCall (CVar (internalIdent macroName) ()) [] ())) ())] ())
                                  ())

nubWith f l = nubBy (\v1 v2 -> f v1 == f v2) l

{-

CDeclExt (CDecl [CStorageSpec (CExtern (_)),CTypeSpec (CIntType (_))]
                [(Just (CDeclr (Just "getitimer")
                               [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef "__itimer_which_t" (_))]
                                                         [(Just (CDeclr (Just "__which") [] Nothing [] (_)),Nothing,Nothing)]
                                                         (_),
                                                   CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "itimerval") Nothing [] (_)) (_))]
                                                         [(Just (CDeclr (Just "__value") [CPtrDeclr [] (_)] Nothing [] (_)),Nothing,Nothing)]
                                                         (_)
                                                   ],
                                                  False))
                                          [] (_)]
                               Nothing
                               [CAttr "__nothrow__" [] (_)]
                               (_)),
                       Nothing,
                       Nothing)]
                (_))

CDeclExt (CDecl [CTypeQual (CConstQual (_)),CTypeSpec (CCharType (_))]
                [(Just (CDeclr (Just "pa_strerror")
                               [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType (_))]
                                                         [(Just (CDeclr (Just "error") [] Nothing [] (_)),
                                                           Nothing,
                                                           Nothing)]
                                                         (_)],
                                                  False))
                                          []
                                          (_),
                                CPtrDeclr [] (_)]
                               Nothing
                               []
                               (_)),
                  Nothing,
                  Nothing)]
                (_))

CTranslUnit [CFDefExt (CFunDef [CTypeSpec (CIntType ())]
                         (CDeclr (Just "foo_int") [CFunDeclr (Right ([],False)) [] ()] Nothing [] ())
                         []
                         (CCompound []
                                    [CBlockStmt (CReturn (Just (CUnary CMinOp (CVar "PA_ERR_NOTIMPLEMENTED" ()) ())) ())]
                                    ())
                         ()),
       CFDefExt (CFunDef [CTypeSpec (CCharType ())] (CDeclr (Just "foo_pchar") [CFunDeclr (Right ([],False)) [] (),CPtrDeclr [] ()] Nothing [] ()) [] (CCompound [] [CBlockStmt (CReturn (Just (CConst (CIntConst 0 ()))) ())] ()) ())] ()

CTranslUnit [CFDefExt (CFunDef [CTypeSpec (CIntType ())]
                               (CDeclr (Just "foo") [CFunDeclr (Right ([],False)) [] ()] Nothing [] ())
                               []
                               (CCompound [] [CBlockStmt (CExpr (Just (CCall (CVar "FOO_AAA" ()) [] ())) ())] ())
                               ())]
            ()

 -}
