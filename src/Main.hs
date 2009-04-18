module Main ( main ) where

import Language.Haskell.TH.ZeroTH.Config ( outputFile )
import Language.Haskell.TH.ZeroTH.GetOpt ( parseArgs, mkConfig )
import Language.Haskell.TH.ZeroTH        ( zeroTH )

import Control.Applicative ( (<*>) )
import System.Environment  ( getArgs )
import System.IO           ( stdout, hPutStr )

writeToFile :: FilePath -> String -> IO ()
writeToFile "-" d = hPutStr stdout d
writeToFile path d = writeFile path d

main :: IO ()
main = ((=<<) . writeToFile . outputFile <*> zeroTH) =<< mkConfig =<< parseArgs =<< getArgs
