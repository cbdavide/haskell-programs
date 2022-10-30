
functionC x y = if (x > y) then x else y

functionC' x y =
    case (x > y) of
      True -> x
      False -> y

ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' n =
    case even n of
      True -> n + 2
      False -> n


nums x =
    case compare x 0 of
      LT -> -1
      GT -> 1
      EQ -> 0

avgGrade x
    | y >= 0.7 = 'C'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100


pal xs
    | xs == reverse xs = True
    | otherwise = False


numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main' :: IO ()
main' = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOnePF) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0))
  print ((negate . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)


tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10


tensDigit' x = ten
  where byTen = div x 10
        (_, ten) = divMod byTen 10


hunsD x = d2
  where d   = div x 10
        d1  = div d 10
        d2  = mod d1 10


foldBool :: a -> a -> Bool -> a
foldBool a b cond =
  case cond of
    True -> a
    False -> b


foldBool' :: a -> a -> Bool -> a
foldBool' a b cond
  | cond == True = a
  | otherwise    = b


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

main :: IO()
main = do
  print (roundTrip 4)
  print (id 4)
