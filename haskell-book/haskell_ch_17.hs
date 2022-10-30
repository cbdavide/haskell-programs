
import Data.List (elemIndex)
import Control.Applicative (liftA2, liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

maxed :: Maybe Int
maxed = max <$> x' <*> y'

xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x'' <*> y''

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity $ f a

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant a) <*> (Constant b) = Constant (mappend a b)


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor (List) where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
    Nil <> a = a
    a <> Nil = a
    (Cons x xs) <> ys = Cons x $ xs <> ys

instance (Monoid a) => Monoid (List a) where
    mempty = undefined

instance Applicative List where
    pure a = Cons a Nil 

    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> vals = (fmap f vals) <> (fs <*> vals)

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take 3000 l
              ys' = let (ZipList' l) = ys
                    in take 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Semigroup (ZipList' a) where
    (ZipList' a) <> (ZipList' b) = ZipList' $ a ++ b

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' [a]

    (ZipList' []) <*> _ = ZipList' []
    _ <*> (ZipList' []) = ZipList' []
    (ZipList' (f:[])) <*> (ZipList' a) = ZipList' $ fmap f a
    (ZipList' (f:fs)) <*> (ZipList' (x:[])) = (ZipList' [f x]) <> (ZipList' fs <*> pure x)
    (ZipList' (x:xs)) <*> (ZipList' (y:ys)) = (pure (x y)) <> (ZipList' xs <*> ZipList' ys)


testZipList :: IO ()
testZipList = do
    quickBatch $ applicative (ZipList' [("1", '2', '3')])


data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation' e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation' e) where
    pure = Success'

    (Failure' a) <*> (Success' _) = Failure' a
    (Success' _) <*> (Failure' a) = Failure' a
    (Success' f) <*> (Success' a) = Success' $ f a
    (Failure' a) <*> (Failure' b) = Failure' $ a <> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Failure' a, Success' b]

instance (Eq a, Eq b) => EqProp (Validation' a b) where
    (=-=) = eq


type ValidationType = Validation' String (String, String, String)

testValidation :: IO ()
testValidation = quickBatch $ applicative (Success' ("a", "b", "c") :: ValidationType)

pureList :: a -> [a]
pureList = pure

appList :: [(a -> b)] -> [a] -> [b]
appList =(<*>)

pureIO :: a -> IO a
pureIO = pure

appIO :: IO (a -> b) -> IO a -> IO b
appIO = (<*>)

pureTuple :: Monoid a => a -> (,) a a
pureTuple = pure

appTuple :: Monoid a => (,) a (a -> b) -> (a,a) -> (a, b)
appTuple = (<*>)

pureFunc :: Monoid a => a -> (->) a a
pureFunc = pure

appFunc :: Monoid a => (->) a (a -> b) -> (->) a a -> (->) a b
appFunc = (<*>)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a = Pair a a 

    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return $ Pair a a

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

testPair :: IO ()
testPair = quickBatch $ applicative (undefined :: Pair (String, String, String))

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure a = Two mempty a
    (Two a f) <*> (Two b c) = Two (a <> b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

testTwo :: IO ()
testTwo = quickBatch $ applicative (Two "a" ("a", "b", "c"))

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty

    (Three a b f) <*> (Three a' b' x) = Three (a <> a') (b <> b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

testThree :: IO ()
testThree = quickBatch $ applicative (Three "a" "b" ("a", "b", "c"))

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
    pure a = Three' mempty a a

    (Three' a f g) <*> (Three' a' x y) = Three' (a <> a') (f x) (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

testThree' :: IO ()
testThree'  = quickBatch $ applicative (Three' "a" ("1", "2", "3") ("4", "5", "6")) 


data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure a = Four mempty mempty mempty a

    (Four a b c f) <*> (Four a' b' c' x) = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

testFour :: IO ()
testFour = quickBatch $ applicative (Four "a" "b" "c" ("a", "b", "c"))

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b

    (Four' a b c f) <*> (Four' a' b' c' x) = Four' (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

testFour' :: IO ()
testFour' = quickBatch $ applicative (Four' "a" "b" "c" ("a", "b", "c"))

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations :: [a] -> [b] -> [c] -> [(a, b, c)]
combinations = liftA3 (,,) 

-- pure (,,) <*> stops <*> vowels <*> stops
-- it works because if first applies the function to the first element
-- of stops (this results in ('p',,)), then the second and so on
--
-- the resutl of this is:
-- [('p',,), ('b',,), ('t',,), ('d',,), ('k',,), ('g',,)]
--
-- Then it applies each function to each element of vowels the result of
-- this is a Tuple3 with the first two elements defined.... and the same
-- with the last list

