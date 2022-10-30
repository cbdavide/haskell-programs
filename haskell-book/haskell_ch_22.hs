{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Char
import Data.Maybe

import Test.QuickCheck


boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tuppled ::  [Char] -> ([Char], [Char])
tuppled = (,) <$> cap <*> rev

tuppled' :: [Char] -> ([Char], [Char])
tuppled' = do
    a <- cap
    b <- rev
    return (a, b)

tuppled'' :: [Char] -> ([Char], [Char])
tuppled'' = cap >>= \a -> rev >>= \b -> return (a, b) 


compare' :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
compare' (a, b, c) (e, f, g) = a > e || (a == e && b > f) || (a == e && b == f && c >= g)

compare'' :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
compare'' (a, b, c) (e, f, g) = a > e || (a == e && b > f) || (a == e && b == f && c > g)

prop_compare ::  (Int, Int, Int) -> (Int, Int, Int) -> Bool
prop_compare xs ys = (compare'' xs ys) == (xs > ys) 

checkCompareProp  = quickCheck prop_compare

newtype Reader r a = Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where

    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r


instance Monad (Reader r) where

    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r)) r


x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(>3), (<8), even] 7

    print $ and . sequA $ 6
    print $ sequA $ fromMaybe 0 s'
    print $ bolt $ fromMaybe 0 ys
