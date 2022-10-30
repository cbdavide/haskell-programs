module Addition where

import Test.Hspec
import Test.QuickCheck


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


multiply :: (Eq a, Num a) => a -> a -> a
multiply _ 0 = 0
multiply x y = go x y
    where go a 1 = a
          go a b = a + go a (b - 1)


main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "5 multiplied 4 is 20" $ do
            multiply 5 4 `shouldBe` 20
        it "5 multiplied 1 is 5" $ do
            multiply 5 1 `shouldBe` 5
        it "5 multiplied 0 is 0" $ do
            multiply (5 :: Int) (0 :: Int) `shouldBe` (0 :: Int)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int) 


genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']
