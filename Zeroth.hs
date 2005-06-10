module Zeroth
    ( zeroth
    ) where

import Language.Haskell.Hsx
import System.Process        ( runInteractiveProcess, waitForProcess )
import System.IO             ( hPutStr, hClose, hGetContents, openTempFile )
import System.Directory      ( removeFile, getTemporaryDirectory )
import System.Exit           ( ExitCode (..) )
import Data.Maybe            ( mapMaybe )

zeroth :: FilePath -- ^ Path to GHC
       -> FilePath -- ^ Path to cpphs
       -> [String] -- ^ GHC options
       -> [String] -- ^ cpphs options
       -> String -- ^ Input data
       -> IO String
zeroth ghcPath cpphsPath ghcOpts cpphsOpts input
    = do thInput     <- preprocessCpphs cpphsPath (["--noline","-DHASTH"]++cpphsOpts) input
         zerothInput <- preprocessCpphs cpphsPath (["--noline"]++cpphsOpts) input
         thData <- case parseModule thInput of
                     ParseOk m@(HsModule _ _ _ _ decls) -> runTH ghcPath m ghcOpts (mapMaybe getTH decls)
                     e -> error (show e)
         zerothData <- case parseModule zerothInput of
                         ParseOk (HsModule loc m exports im decls) -> return (HsModule loc m exports im (filter delTH decls))
                         e -> error (show e)
         return (prettyPrint zerothData ++ "\n" ++ thData)
    where getTH (HsSpliceDecl _ s) = Just s
          getTH _ = Nothing
          delTH (HsSpliceDecl _ _) = False
          delTH _ = True

preprocessCpphs :: FilePath -- ^ Path to cpphs
                -> [String]
                -> String
                -> IO String
preprocessCpphs cpphs args input
    = do tmpDir <- getTemporaryDirectory
         (tmpPath,tmpHandle) <- openTempFile tmpDir "TH.cpphs.zeroth"
         hPutStr tmpHandle input
         hClose tmpHandle
         (inH,outH,_,pid) <- runInteractiveProcess cpphs (tmpPath:args) Nothing Nothing
         hClose inH
         output <- hGetContents outH
         --length output `seq` hClose outH
         eCode <- waitForProcess pid
         removeFile tmpPath
         case eCode of
           ExitFailure err -> error $ "Failed to run cpphs: " ++ show err
           ExitSuccess -> return output

runTH :: FilePath -- ^ Path to GHC
      -> HsModule 
      -> [String]
      -> [HsSplice]
      -> IO String
runTH ghcPath (HsModule src _ _ imports _) ghcOpts th
    = do tmpDir <- getTemporaryDirectory
         (tmpInPath,tmpInHandle) <- openTempFile tmpDir "TH.source.zeroth.hs"
         hPutStr tmpInHandle (prettyPrint realM)
         hClose tmpInHandle
         let args = [tmpInPath,"-e","main","-fth","-fglasgow-exts"]++ghcOpts
         --putStrLn $ "Module:\n" ++ prettyPrint realM
         --putStrLn $ "Running: " ++ unwords (ghcPath:args)
         (inH,outH,errH,pid) <- runInteractiveProcess ghcPath args Nothing Nothing
         hClose inH
         output <- hGetContents outH
         --putStrLn $ "TH Data:\n" ++ output
         length output `seq` hClose outH
         errMsg <- hGetContents errH
         length errMsg `seq` hClose errH
         eCode <- waitForProcess pid
         removeFile tmpInPath
         case eCode of
           ExitFailure err -> error (unwords (ghcPath:args) ++ ": failure: " ++ show err)
           ExitSuccess | not (null errMsg) -> error (unwords (ghcPath:args) ++ ": failure:\n" ++ errMsg)
                       | otherwise -> return output
    where realM = HsModule src (Module "Main") (Just [HsEVar (UnQual (HsIdent "main"))]) (thImport:imports) [mainFunc]
          thImport = HsImportDecl emptySrcLoc (Module "Language.Haskell.TH") False Nothing Nothing
          emptySrcLoc = SrcLoc "" 0 0
          mainFunc = HsPatBind emptySrcLoc (HsPVar (HsIdent "main"))
                     (HsUnGuardedRhs (HsDo [HsGenerator emptySrcLoc (HsPVar (HsIdent "decs"))
                                            (HsInfixApp (HsApp (HsVar (UnQual (HsIdent "fmap")))
                                                         (HsVar (UnQual (HsIdent "concat"))))
                                             (HsQVarOp (UnQual (HsSymbol "$")))
                                             (HsApp (HsVar (UnQual (HsIdent "sequence")))
                                              (HsList splices)))
                                           ,HsQualifier (HsApp (HsVar (UnQual (HsIdent "putStrLn")))
                                                         (HsParen (HsApp (HsVar (UnQual (HsIdent "pprint")))
                                                                   (HsVar (UnQual (HsIdent "decs"))))))])) (HsBDecls [])
          splices = flip map th $ HsApp (HsVar (UnQual (HsIdent "runQ"))) . HsParen . spliceToExp
          spliceToExp (HsParenSplice e) = e
          spliceToExp _ = error "TH: FIXME!"

{-

module Test where
$(test)
$(jalla)
svend = svend


-------------------------------

module Main where
import Language.Haskell.TH
main = do decs <- fmap concat $
                  sequence [runQ $(test)
                           ,runQ $(jalla)]
          print decs


-}
