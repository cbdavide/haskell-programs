
-- Problem #1
last' :: [a] -> a
last' [] = error "Empty list"
last' [x] = x
last' (_:xs) = last' xs


-- Problem #2
secondLast :: [a] -> a
secondLast [] = error "Empty list" 
secondLast [x] = error "One Element List"
secondLast xs = last' (init xs)


-- Problem #3
elementAt :: [a] -> Integer -> a
elementAt [] _ = error "Too few elements"
elementAt (x:_) 1 = x
elementAt (x: xs) n = elementAt xs (n - 1)


-- Problem #4
len :: [a] -> Integer
len [] = 0
len xs = 1 + len (tail xs)


-- Problem #5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x: xs) = reverse' xs ++ [x]


-- Problem #6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse' xs


-- Problem #7
data NestedList a = Element a  | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Element a) = [a]
flatten (List t) = flatten (head t) ++ flatten (List (tail t))


-- Problem #8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys) = if x == y then compress (x:ys)
                    else x : compress (y:ys)


-- Problem #9
toListOfLists :: [a] -> [[a]]
toListOfLists [] = [[]]
toListOfLists xs = [[x] | x <- xs] 

pack' :: (Eq a) => [[a]] -> [[a]]
pack' [[]] = [[]]
pack' [ys] = [ys]
pack' (x:y:ys) = if head x == head y then pack'((x++y):ys)
                 else x : pack' (y:ys)

pack :: (Eq a) => [a] -> [[a]]
pack = pack' . toListOfLists


-- Problem #10
reduce :: [[a]] -> [(Int, a)]
reduce xs = [(length x, head x) | x <- xs]

encode :: (Eq a) => [a] -> [(Int, a)]
encode = reduce . pack
