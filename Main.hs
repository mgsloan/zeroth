module Main ( main ) where

import Zeroth              ( zeroth )
import Setup               ( parseArgs, ConfigFlags (..), mkConfigFlags )

import System.Environment  ( getArgs )
import System.IO           ( hGetContents, stdin, stdout, hPutStr )

readFromFile :: FilePath -> IO String
readFromFile "-" = hGetContents stdin
readFromFile path = readFile path

writeToFile :: FilePath -> String -> IO ()
writeToFile "-" d = hPutStr stdout d
writeToFile path d = writeFile path d



main :: IO ()
main = do args <- getArgs
          configFlags <- mkConfigFlags =<< parseArgs args
          input <- readFromFile (configInputFile configFlags)
          output <- zeroth (configGHCPath configFlags)
                           (configCpphsPath configFlags)
                           (configGHCArgs configFlags)
                           (configCpphsArgs configFlags)
                           input
          writeToFile (configOutputFile configFlags) output


