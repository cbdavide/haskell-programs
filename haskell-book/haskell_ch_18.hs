
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer] -> [(Integer,Integer)]
twiceWhenEven xs ys = do
    x <- xs
    y <- ys
    return (x, y)


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second

    (First b) <*> _ = First b
    _ <*> (First a) = First a
    (Second f) <*> (Second x) = Second $ f x

instance Monad (Sum a) where
    return = pure

    (>>=) (First a) _ = First a
    (>>=) (Second b) f = f b


instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [First a, Second b]


type SumType = Sum String (String, String, String)
type SumType2 = Sum Int (Int, Int, Int)

testSumMonad :: IO ()
testSumMonad = quickBatch $ monad (undefined :: SumType2)


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return _ = NopeDotJpg
    (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq

testNope :: IO ()
testNope = quickBatch $ monad (undefined :: Nope (Int, Int, Int))


data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
    fmap _ (PRight b) = PRight b
    fmap f (PLeft a) = PLeft $ f a

instance Applicative (BahEither b) where
    pure = PLeft

    (PRight a) <*> _ = PRight a
    _ <*> (PRight a) = PRight a
    (PLeft f) <*> (PLeft x) = PLeft $ f x

instance Monad (BahEither b) where
    return = pure

    (>>=) (PRight a) _ = PRight a
    (>>=) (PLeft b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [PRight a, PLeft b]

instance (Eq a, Eq b) => EqProp (BahEither a b) where
    (=-=) = eq

testBahEither :: IO ()
testBahEither = do
    quickBatch $ functor (undefined :: BahEither Int (Int, Int, Int))
    quickBatch $ applicative (undefined :: BahEither Int (Int, Int, Int))
    quickBatch $ monad (undefined :: BahEither Int (Int, Int, Int))

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

testIdentity :: IO ()
testIdentity = do
    quickBatch $ functor (undefined :: Identity (Int, Int, Int))
    quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
    quickBatch $ monad (undefined :: Identity (Int, Int, Int))

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor (List) where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
    Nil <> a = a
    a <> Nil = a
    (Cons x xs) <> ys = Cons x $ xs <> ys

instance Applicative List where
    pure a = Cons a Nil 

    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> vals = (fmap f vals) <> (fs <*> vals)

instance Monad List where
    return = pure

    (>>=) Nil _ = Nil
    (>>=) (Cons a b) f = f a <> (b >>= f)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        elements [Nil, Cons a Nil]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

testList :: IO ()
testList = do
    quickBatch $ functor (undefined :: List (Int, Int, Int))
    quickBatch $ applicative (undefined :: List (Int, Int, Int))
    quickBatch $ monad (undefined :: List (Int, Int, Int))

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x then [x*x, x*x] else []

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (++) (return <$> (f x)) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id 
