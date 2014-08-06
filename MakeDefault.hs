#!/usr/bin/runghc
module MakeDefault where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Language.C (parseCFile)
import Language.C.Syntax.AST (CDeclarator(..),
                              CTranslationUnit(CTranslUnit),
                              CExternalDeclaration(CDeclExt),
                              CDerivedDeclarator(CFunDeclr),
                              CDeclaration(CDecl))
import Language.C.System.GCC (newGCC)
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Ident (Ident(Ident))
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO (openBinaryTempFile, hPutStrLn, hClose)

-- TODO:
-- * scan whole pulse directory
-- * make default implementation
--  * return type: int, char*, api*
-- * make it skip existing functions
-- * check linkage, add more exports if necessary

main = do
  functions <- concat <$> mapM getFunctions ["mainloop-api.h", "error.h"]
  mapM_ (\(CDeclr (Just name) _ _ _ _) -> print name) functions

getFunctions :: String -> IO [CDeclarator NodeInfo]
getFunctions file = do
  tempDir <- getTemporaryDirectory
  bracket
    (createTmpSource tempDir file)
    removeFile
    (\tmpSource -> do
      Right (CTranslUnit decls _) <- parseCFile (newGCC "gcc") Nothing ["-I/usr/include/pulse"] tmpSource
      return [ decl | (CDeclExt (CDecl _ [(Just decl@(CDeclr (Just (Ident name _ _)) (CFunDeclr _ _ _ : _) _ _ _), Nothing, Nothing)] _)) <- decls, case name of { ('p' : 'a' : '_' : _) -> True; _ -> False } ])

createTmpSource :: String -> String -> IO String
createTmpSource tempDir file = do
  -- FIXME: this leaves file in case of error
  (path, handle) <- openBinaryTempFile tempDir "pulse_2include_XXXXXXXX.c"
  hPutStrLn handle ("#include <" ++ file ++ ">")
  hClose handle
  return path

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


 -}
