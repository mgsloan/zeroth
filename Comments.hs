module Comments where

type Comment = (Int,String)

mixComments :: [Comment] -> [(Int,String)] -> [String]
mixComments c [] = map (showComment.lines) (map snd c)
mixComments [] i = map snd i
mixComments c@((cl,cc):cs) i@((il,ic):is)
    | cl <= il = showComment (lines cc):mixComments cs i
    | otherwise = ic:mixComments c is

showComment :: [String] -> String
showComment [x] = '-':'-':' ':x
showComment c = "{- " ++ unlines c ++ " -}"

-- FIXME!
parseComments :: String -> [Comment]
parseComments input = parseComments' (lines input) 1

parseComments' :: [String] -> Int -> [Comment]
parseComments' [] _ = []
parseComments' (('-':'-':c):xs) l = (l,c):parseComments' xs (l+1)
parseComments' (_:xs) l = parseComments' xs (l+1)
