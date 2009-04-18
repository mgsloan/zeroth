module ZerothHelper ( helper, idPrefix ) where

import Data.Generics.Schemes      ( listify )
import Data.Maybe                 ( fromJust, isJust )
import Language.Haskell.TH.Ppr    ( pprint )
import Language.Haskell.TH.Syntax ( Dec, nameModule, Q, runIO )
import System.IO                  ( hFlush, stdout )

import Comments                   ( Location )

idPrefix :: String
idPrefix = "ZEROTH OUTPUT: "

helper :: Q [Dec] -> Location -> Q [Dec]
helper splice loc = do
    decls <- splice
    runIO $ do putStrLn $ idPrefix ++ show ((loc, pprint =<< decls), map (fromJust . nameModule) $ listify (isJust . nameModule) decls)
               hFlush stdout
    return decls