module Zeroth
    ( zeroth
    ) where

import Language.Haskell.Hsx hiding ( comments )
import System.Process        ( runInteractiveProcess, waitForProcess )
import System.IO             ( hPutStr, hClose, hGetContents, openTempFile )
import System.Directory      ( removeFile, getTemporaryDirectory )
import System.Exit           ( ExitCode (..) )
import Data.Maybe            ( mapMaybe )

import Comments              ( parseComments, mixComments )

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
         return (prettyPrint zerothData ++ "\n" ++ unlines (mixComments comments thData))
    where getTH (HsSpliceDecl l s) = Just (l,s)
          getTH _ = Nothing
          delTH (HsSpliceDecl _ _) = False
          delTH _ = True
          comments = parseComments input

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
         length output `seq` hClose outH
         eCode <- waitForProcess pid
         removeFile tmpPath
         case eCode of
           ExitFailure err -> error $ "Failed to run cpphs: " ++ show err
           ExitSuccess -> return output

runTH :: FilePath -- ^ Path to GHC
      -> HsModule 
      -> [String]
      -> [(SrcLoc,HsSplice)]
      -> IO [(Int,String)]
runTH ghcPath (HsModule _ _ _ imports _) ghcOpts th
    = do tmpDir <- getTemporaryDirectory
         (tmpInPath,tmpInHandle) <- openTempFile tmpDir "TH.source.zeroth.hs"
         hPutStr tmpInHandle realM
         hClose tmpInHandle
         let args = [tmpInPath,"-e","main","-fth","-fglasgow-exts"]++ghcOpts
         --putStrLn $ "Module:\n" ++ realM
         --putStrLn $ "Running: " ++ unwords (ghcPath:args)
         (inH,outH,errH,pid) <- runInteractiveProcess ghcPath args Nothing Nothing
         hClose inH
         output <- hGetContents outH
         --putStrLn $ "TH Data:\n" ++ output
         length output `seq` hClose outH
         errMsg <- hGetContents errH
         length errMsg `seq` hClose errH
         eCode <- waitForProcess pid
         -- removeFile tmpInPath
         case eCode of
           ExitFailure err -> error (unwords (ghcPath:args) ++ ": failure: " ++ show err)
           ExitSuccess | not (null errMsg) -> error (unwords (ghcPath:args) ++ ": failure:\n" ++ errMsg)
                       | otherwise -> case reads output of
                                        [(ret,_)] -> return ret
                                        _         -> error $ "Failed to parse result:\n"++output
    where thImport = HsImportDecl emptySrcLoc (Module "Language.Haskell.TH") False Nothing Nothing
          emptySrcLoc = SrcLoc "" 0 0
          pp :: (Pretty a) => a -> String
          pp = prettyPrintWithMode (defaultMode{layout = PPInLine})
          realM = unlines $ ["module Main ( main ) where"]
                            ++ map pp (thImport:imports)
                            ++ ["main = do decls <- sequence $ " ++ pp (HsList splices)]
                            ++ ["          print $ map (\\(l,d) -> (l,pprint d)) (zip "++ pp (HsList lineNums) ++" decls)"]
          splices = flip map th $ \(_src,splice) -> HsApp (HsVar (UnQual (HsIdent "runQ"))) (HsParen (spliceToExp splice))
          lineNums = flip map th $ \(loc,_splice) -> HsLit (HsInt (fromIntegral (srcLine loc)))
          spliceToExp (HsParenSplice e) = e
          spliceToExp _ = error "TH: FIXME!"

{-

module Test where
$(test) -- line 2
$(jalla) -- line 3
svend = svend


-------------------------------

module Main where
import Language.Haskell.TH
main = do decs <- sequence [runQ test
                           ,runQ jalla]
          mapM_ (putStrLn.pprint) (zip decs [2,3])


-}
