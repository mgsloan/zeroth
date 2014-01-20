{-# LANGUAGE PatternGuards #-}
module Language.Haskell.TH.ZeroTH.Helper ( helper, idPrefix ) where

import Data.Generics
import Data.Maybe                 ( fromJust, isJust )
import System.IO                  ( hFlush, stdout )

import Language.Haskell.TH.ZeroTH.Comments ( Location )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Set as S

idPrefix :: String
idPrefix = "ZEROTH OUTPUT: "

helper :: Q [Dec] -> Location -> Q [Dec]
helper splice loc = do
    decls <- splice
    runIO $ do putStrLn $ idPrefix ++ show ((loc, unlines $ fmap pprint $ map unNameU decls),
                                map (fromJust . nameModule) $ listify (isJust . nameModule) decls)
               hFlush stdout
    return decls

-- | if a Dec binds a name that is a NameU, ghc generates a NameS
unNameU :: Dec -> Dec
unNameU x = everywhere (mkT $ \n -> if S.member n u
                    then renameU n
                    else n)
                x
    where u = S.fromList (topLevelBindrs x)

topLevelBindrs :: Dec -> [Name]
topLevelBindrs x = case x of
    FunD n _            -> [n]
    ValD p _ _          -> allVarPNameU p 
    DataD _ n _ _ _     -> [n]
    NewtypeD _ n _ _ _  -> [n]
    TySynD n _ _        -> [n]
    ClassD _ n _ _ _    -> [n] -- needed?
    SigD n _            -> [n]
    InfixD _ n          -> [n]
    FamilyD _ n _ _     -> [n]
    DataInstD _ n _ _ _ -> [n]
    NewtypeInstD _ n _ _ _ -> [n]
    TySynInstD n _ _    -> [n]
    PragmaD (InlineP n _ _ _)       -> [n]
    PragmaD (SpecialiseP n _ _ _)   -> [n]
    InstanceD {}    -> []
    ForeignD {}     -> []
    PragmaD {}      -> []

renameU :: Name -> Name
renameU (Name occ (NameU _)) = Name occ NameS
renameU x = x

allVarPNameU :: Data a => a -> [Name]
allVarPNameU = everything (++) (mkQ [] $ \e -> case e of
    VarP n | Name _ (NameU _) <- n -> [n]
    _ -> [])
