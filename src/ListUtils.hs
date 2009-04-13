module ListUtils where

import Data.List  ( find, intercalate, isPrefixOf, unfoldr )
import Data.Maybe ( isJust )

-- | Returns whether there exists an element in the list satisfying the given predicate 
contains :: (a -> Bool) -> [a] -> Bool
contains = (isJust .) . find

-- | Replace a substring with a replacement string throughout a list
replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll []     newSub = intercalate newSub . map return
replaceAll oldSub newSub = concat . unfoldr replace
    where
        replace list = do
            (h:t) <- return list
            return $ if oldSub `isPrefixOf` list then (newSub, drop len list) else ([h], t)
        len = length oldSub
