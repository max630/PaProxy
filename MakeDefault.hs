#!/usr/bin/runghc
module MakeDefault(main) where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.List (nubBy, isPrefixOf)
import Language.C (parseCFile)
import Language.C.Syntax.AST (CTypeSpecifier(CCharType, CDoubleType, CFloatType, CIntType, CSUType, CTypeDef, CUnsigType, CVoidType),
                              CTypeQualifier(CConstQual),
                              CTranslationUnit(CTranslUnit),
                              CStructureUnion(CStruct),
                              CStructTag(CStructTag),
                              CStatement(CCompound, CExpr),
                              CFunctionDef(CFunDef),
                              CExternalDeclaration(CDeclExt),
                              CExpression(CCall, CVar),
                              CDerivedDeclarator(CFunDeclr, CPtrDeclr),
                              CDeclarator(CDeclr),
                              CDeclarationSpecifier(CTypeQual, CTypeSpec),
                              CDeclaration(..),
                              CCompoundBlockItem(CBlockStmt))
import Language.C.System.GCC (newGCC)
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Data.Ident (Ident(Ident), internalIdent)
import Language.C.Pretty (Pretty(pretty))
import System.Directory (removeFile, getDirectoryContents, getTemporaryDirectory)
import System.IO (openBinaryTempFile, hPutStrLn, hClose)
import GHC.Exts (sortWith)

-- TODO:
-- + scan whole pulse directory
-- + make default implementation
-- * make it skip existing functions
-- * check linkage, add more exports if necessary

data ReturnType = RTInt | RTPChar deriving (Eq, Ord, Show)

main = getIncludeFunctions >>= mapM_ printFun
  where
    printFun decl@(CDecl spec1 [(Just (CDeclr (Just (Ident name _ _)) spec2 _ _ _), Nothing, Nothing)] _) = do
      implName <- case (spec1, spec2) of
          ([CTypeSpec (CVoidType _)], [CFunDeclr _ _ _]) -> return "DEFAULT_VOID"
          ([CTypeSpec (CIntType _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CUnsigType _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CFloatType _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CDoubleType _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "uint32_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "int64_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "size_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "pa_usec_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ZERO"
          ([CTypeSpec (CTypeDef (Ident "pa_channel_position_mask_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_CHANNEL_POSITION_MASK"
          ([CTypeSpec (CTypeDef (Ident "pa_channel_position_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_CHANNEL_POSITION"
          ([CTypeSpec (CTypeDef (Ident "pa_context_state_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_CONTEXT_STATE"
          ([CTypeSpec (CTypeDef (Ident "pa_volume_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_VOLUME"
          ([CTypeSpec (CTypeDef (Ident "pa_encoding_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_ENCODING"
          ([CTypeSpec (CTypeDef (Ident "pa_prop_type_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_PROP_TYPE"
          ([CTypeSpec (CTypeDef (Ident "pa_operation_state_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_OPERATION_STATE"
          ([CTypeSpec (CTypeDef (Ident "pa_sample_format_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_SAMPLE_FORMAT"
          ([CTypeSpec (CTypeDef (Ident "pa_stream_state_t" _ _) _)], [CFunDeclr _ _ _]) -> return "DEFAULT_STREAM_STATE"
          ([CTypeSpec (CVoidType _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "DEFAULT_NULL"
          ([CTypeSpec (CCharType _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "DEFAULT_NULL"
          ([CTypeQual (CConstQual _), CTypeSpec (CCharType _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "DEFAULT_NULL"
          ([CTypeSpec (CTypeDef (Ident paTypeName _ _) _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) | "pa_" `isPrefixOf` paTypeName -> return "DEFAULT_NULL"
          ([CTypeQual (CConstQual _), CTypeSpec (CTypeDef (Ident paTypeName _ _) _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) | "pa_" `isPrefixOf` paTypeName -> return "DEFAULT_NULL"
          ([CTypeSpec (CSUType (CStruct CStructTag (Just (Ident "timeval" _ _)) _ _ _) _)], [CFunDeclr _ _ _, CPtrDeclr _ _]) -> return "DEFAULT_NULL"
          _ -> fail ("Unrecognized: " ++ show (pretty decl) ++ "\n" ++ show (fmap (const ()) decl))
      putStrLn $ show $ pretty $ fmap (const undefNode) $ tryFun2 name implName spec1u spec2u
      where
        spec1u = map (fmap (const ())) spec1
        spec2u = map (fmap (const ())) spec2

getIncludeFunctions = do
  includes <- filter (`notElem` [".", ".."]) <$> getDirectoryContents "/usr/include/pulse"
  nubWith declName . sortWith declName . concat <$> mapM getFunctions includes

declName (CDecl _ [(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _)] _) = name

getFunctions :: String -> IO [CDeclaration NodeInfo]
getFunctions file = do
  tempDir <- getTemporaryDirectory
  bracket
    (createTmpSource tempDir file)
    removeFile
    (\tmpSource -> do
      Right (CTranslUnit decls _) <- parseCFile (newGCC "gcc") Nothing ["-I/usr/include/pulse", "-I/usr/include/glib-2.0", "-I/usr/lib/i386-linux-gnu/glib-2.0/include"] tmpSource
      return [ decl | (CDeclExt decl@(CDecl _ [(Just (CDeclr (Just (Ident name _ _)) (CFunDeclr _ _ _ : _) _ _ _), Nothing, Nothing)] _)) <- decls,
                      "pa_" `isPrefixOf` name && not ("pa_simple_" `isPrefixOf` name) ])

tuDecls (CTranslUnit decls _) = decls

createTmpSource :: String -> String -> IO String
createTmpSource tempDir file = do
  -- FIXME: this leaves file in case of error
  (path, handle) <- openBinaryTempFile tempDir "pulse_2include_XXXXXXXX.c"
  hPutStrLn handle ("#include <" ++ file ++ ">")
  hClose handle
  return path

tryFun2 funName macroName spec1 spec2 =
  fmap (const undefNode) (CFunDef spec1
                                  (CDeclr (Just $ internalIdent funName) spec2 Nothing [] ())
                                  []
                                  (CCompound [] [CBlockStmt (CExpr (Just (CCall (CVar (internalIdent macroName) ()) [] ())) ())] ())
                                  ())

fromEitherM (Right r) = return r
fromEitherM (Left e) = fail (show e)

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
