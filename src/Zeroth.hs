module Zeroth
    ( zeroth
    ) where

import Language.Haskell.Exts hiding ( comments )
import System.Process        ( runInteractiveProcess, waitForProcess )
import System.IO             ( hPutStr, hClose, hGetContents, openTempFile, stdin )
import System.Directory      ( removeFile, getTemporaryDirectory )
import System.Exit           ( ExitCode (..) )
import Control.Monad         ( when )
import Data.Maybe            ( mapMaybe )

import Comments              ( parseComments, mixComments )

readFromFile :: FilePath -> IO String
readFromFile "-"  = hGetContents stdin
readFromFile path = readFile path

zeroth :: FilePath -- ^ Path to GHC
       -> FilePath -- ^ Path to cpphs
       -> [String] -- ^ GHC options
       -> [String] -- ^ cpphs options
       -> String   -- ^ Input filename, or "-" for stdin
       -> IO String
zeroth ghcPath cpphsPath ghcOpts cpphsOpts inputFile
    = do input       <- readFromFile inputFile
         tmpDir      <- getTemporaryDirectory
         (inputFile2, tmpHandle) <- case inputFile of
                                       "-" -> openTempFile tmpDir "TH.cpphs.zeroth"
                                       _   -> return (inputFile, undefined)
         when (inputFile == "-") $ hPutStr tmpHandle input >> hClose tmpHandle
         thInput     <- preprocessCpphs cpphsPath (["--noline","-DHASTH"]++cpphsOpts) inputFile2
         zerothInput <- preprocessCpphs cpphsPath (["--noline"]++cpphsOpts) inputFile2
         thData <- case parseModule thInput of
                     ParseOk m@(Module _ _ _ _ _ _ decls) -> runTH ghcPath m ghcOpts (mapMaybe getTH decls)
                     e -> error (show e)
         zerothData <- case parseModule zerothInput of
                         ParseOk (Module loc m opts mWarn exports im decls) -> return (Module loc m opts mWarn exports im (filter delTH decls))
                         e -> error (show e)
         when (inputFile == "-") $ removeFile inputFile2
         return (prettyPrint zerothData ++ "\n" ++ unlines (mixComments (parseComments input) thData))
    where getTH (SpliceDecl l s) = Just (l,s)
          getTH _ = Nothing
          delTH (SpliceDecl _ _) = False
          delTH _ = True

preprocessCpphs :: FilePath -- ^ Path to cpphs
                -> [String]
                -> String
                -> IO String
preprocessCpphs cpphs args inputFile
    = do (inH,outH,_,pid) <- runInteractiveProcess cpphs (inputFile:args) Nothing Nothing
         hClose inH
         output <- hGetContents outH
         length output `seq` hClose outH
         eCode <- waitForProcess pid
         case eCode of
           ExitFailure err -> error $ "Failed to run cpphs: " ++ show err
           ExitSuccess -> return output

runTH :: FilePath -- ^ Path to GHC
      -> Module 
      -> [String]
      -> [(SrcLoc,Splice)]
      -> IO [(Int,String)]
runTH ghcPath (Module _ _ _ _ _ imports _) ghcOpts th
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
    where thImport = ImportDecl emptySrcLoc (ModuleName "Language.Haskell.TH") False False Nothing Nothing
          emptySrcLoc = SrcLoc "" 0 0
          pp :: (Pretty a) => a -> String
          pp = prettyPrintWithMode (defaultMode{layout = PPInLine})
          realM = unlines $ ["module Main ( main ) where"]
                            ++ map pp (thImport:imports)
                            ++ ["main = do decls <- sequence $ " ++ pp (List splices)]
                            ++ ["          print $ map (\\(l,d) -> (l,pprint d)) (zip "++ pp (List lineNums) ++" decls)"]
          splices = flip map th $ \(_src,splice) -> App (Var (UnQual (Ident "runQ"))) (Paren (spliceToExp splice))
          lineNums = flip map th $ \(loc,_splice) -> Lit (Int (fromIntegral (srcLine loc)))
          spliceToExp (ParenSplice e) = e
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
