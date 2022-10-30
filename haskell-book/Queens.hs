module Queens (boardString, canAttack) where

endChar :: Int -> String
endChar 7 = "\n"
endChar _ = " "

nextPos :: Int -> Int -> (Int, Int)
nextPos a b = if b == 7 then (a, b + 1) else (a + 1, 0)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString (Just (a, b)) (Just (c, d)) = go 0 0
    where go n m 
            | n == 8           = ""
            | n == a && m == b = "W" ++ endChar m ++ (go nextN nextM)
            | n == c && m == d = "B" ++ endChar m ++ (go nextN nextM)
            | otherwise        = "_" ++ endChar m ++ (go nextN nextM)
            where (nextN, nextM) = nextPos n m
boardString Nothing Nothing = go 0 0
     where go n m 
            | n == 8           = ""
            | otherwise        = "_" ++ endChar m ++ (go nextN nextM)
            where (nextN, nextM) = nextPos n m


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (a, b) (c, d) = a == c || b == d || (abs $ a - b) == (abs $ c - d)
