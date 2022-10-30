import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck


half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

testHalf :: IO ()
testHalf =
    quickCheck $ \x -> (halfIdentity x) == (x :: Float)


listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

testOrder :: IO ()
testOrder = quickCheck $ \x -> listOrdered . sort $ (x :: [Int])


plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

testPlusAssociative :: IO ()
testPlusAssociative = quickCheck plusAssociative

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

testPlusCommutative :: IO ()
testPlusCommutative  = quickCheck plusCommutative


multiAssociative :: Int -> Int -> Int -> Bool
multiAssociative x y z = x + (y + z) == (x + y) + z

testMultiAssociative :: IO ()
testMultiAssociative = quickCheck multiAssociative

multiCommutative :: Int -> Int -> Bool
multiCommutative x y = x + y == y + x

testMultiCommutative :: IO ()
testMultiCommutative  = quickCheck multiCommutative

nonZeroIntGen :: Gen Int
nonZeroIntGen = suchThat (arbitrary :: Gen Int) (/=0)

nonZeroIntTupleGen :: Gen (Int, Int)
nonZeroIntTupleGen = do
  a <- nonZeroIntGen 
  b <- nonZeroIntGen 
  return (a, b)

quotCheck :: Int -> Int -> Bool
quotCheck x y = (quot x y) * y + (rem x y) == x

testQuot :: IO ()
testQuot = quickCheck $ forAll nonZeroIntTupleGen (\(x,y) -> quotCheck x y)

divCheck :: Int -> Int -> Bool
divCheck x y = (div x y) * y + (mod x y) == x

testDiv :: IO ()
testDiv = quickCheck $ forAll nonZeroIntTupleGen (\(x, y) -> divCheck x y) 

powAssociativeCheck :: Int -> Int -> Int -> Bool
powAssociativeCheck x y z = x ^ (y ^ z) == (x ^ y) ^ z 

testAssociativePow :: IO ()
testAssociativePow = quickCheck powAssociativeCheck

powCommutativeCheck :: Int -> Int -> Bool
powCommutativeCheck x y = x ^ y == y ^ x

testCommutativePow :: IO ()
testCommutativePow = quickCheck powCommutativeCheck


reverseCheck :: [Int] -> Bool
reverseCheck x = (reverse . reverse $ x) == id x

testReverse :: IO ()
testReverse = quickCheck reverseCheck


-- don't remember how $ is called
theCheck :: Int -> Bool
theCheck a = (f $ a) == f a
  where f a = a + 1

testTheCheck :: IO ()
testTheCheck = quickCheck theCheck

compositionCheck :: Int -> Bool
compositionCheck x = (f . g $ x) ==  f (g x)
  where f a = a + 1
        g a = a * 2

testComposition :: IO ()
testComposition = quickCheck compositionCheck


consCheck :: [Int] -> Bool
consCheck xs = foldr (:) [] xs == (++) [] xs

testCons :: IO ()
testCons = quickCheck consCheck

tenCheck :: Int -> [Int] -> Bool
tenCheck n xs = length (take n xs) == n

testTen :: IO ()
testTen = quickCheck tenCheck

showCheck :: Int -> Bool
showCheck x = (read . show $ x) == x

testShow :: IO ()
testShow = quickCheck showCheck

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWordCheck x = 
  (capitalizeWord x == twice capitalizeWord x)
  && 
  (capitalizeWord x == fourTimes capitalizeWord x)

testCapitalizeWord :: IO ()
testCapitalizeWord = quickCheck capitalizeWordCheck 

sortCheck :: [Int] -> Bool
sortCheck x = 
  (sort x == twice sort x)
  &&
  (sort x == fourTimes sort x)

testSort:: IO ()
testSort = quickCheck sortCheck


data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen =
  oneof [return Fulse, return Frue]


biasFoolGen :: Gen Fool
biasFoolGen =
  frequency [(2, return Fulse), (1, return Frue)]
