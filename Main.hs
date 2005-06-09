module Main where

import Zeroth
import Setup

import System.Environment
import System.IO
import Control.Exception

withFileHandleIn :: FilePath -> (Handle -> IO a) -> IO a
withFileHandleIn "-" action = action stdin
withFileHandleIn file action = bracket (openFile file ReadMode)
                                       (hClose)
                                       action

withFileHandleOut :: FilePath -> (Handle -> IO a) -> IO a
withFileHandleOut "-" action = action stdout
withFileHandleOut file action = bracket (openFile file WriteMode)
                                        (hClose)
                                        action



main :: IO ()
main = do args <- getArgs
          configFlags <- mkConfigFlags =<< parseArgs args
          withFileHandleIn (configInputFile configFlags) $ \inputHandle ->
           withFileHandleOut (configOutputFile configFlags) $ \outputHandle ->
            do input <- hGetContents inputHandle
               output <- zeroth (configGHCPath configFlags)
                                (configCpphsPath configFlags)
                                (configGHCArgs configFlags)
                                input
               hPutStrLn outputHandle output


