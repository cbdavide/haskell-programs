import Data.Time
import Data.Bool

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 0
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

prependDbDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
prependDbDate (DbDate a) xs = a : xs
prependDbDate _ xs          = xs

prependDbNumber :: DatabaseItem -> [Integer] -> [Integer]
prependDbNumber (DbNumber a) xs = a : xs
prependDbNumber _ xs            = xs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr prependDbDate []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr prependDbNumber []

maxDbDate :: [DatabaseItem] -> UTCTime
maxDbDate = foldr max (UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)) . filterDbDate

sumDbNumbers :: [DatabaseItem] -> Integer
sumDbNumbers = foldr (+) 0 . filterDbNumber

averageDbNumbers :: [DatabaseItem] -> Double
averageDbNumbers xs = fromIntegral dbNumbersSum / fromIntegral (length dbNumbers) 
    where dbNumbers = filterDbNumber xs
          dbNumbersSum = foldr (+) 0 dbNumbers


stops = "pbtdkg"
vowels = "aeiou"

comb3 = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

comb3Filtered = filter (\(x, y, _) -> x == 'p') comb3

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = myAny (== e)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> bool b (a : b) (f a)) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- non-total functions just like foldr1
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\b a -> if f a b == GT then a else b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\b a -> if f a b == LT then b else a) x xs
