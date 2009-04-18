module Main ( main ) where

import Zeroth              ( zeroth )
import Setup               ( parseArgs, ConfigFlags (..), mkConfigFlags )

import System.Environment  ( getArgs )
import System.IO           ( stdout, hPutStr )

writeToFile :: FilePath -> String -> IO ()
writeToFile "-" d = hPutStr stdout d
writeToFile path d = writeFile path d



main :: IO ()
main = do args <- getArgs
          configFlags <- mkConfigFlags =<< parseArgs args
          output <- zeroth (configGHCPath configFlags)
                           (configCpphsPath configFlags)
                           (configGHCArgs configFlags)
                           (configCpphsArgs configFlags)
                           (configWholeFile configFlags)
                           (configInputFile configFlags)
                           (configDropImport configFlags)
          writeToFile (configOutputFile configFlags) output


