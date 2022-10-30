import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-# LANGUAGE FlexibleContexts #-}

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

checkIdentity :: IO ()
checkIdentity = quickBatch $ traversable (undefined :: Identity (Int, Int, Sum Int))

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
    traverse f (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

checkConstant :: IO ()
checkConstant = quickBatch $ traversable (undefined :: Constant Int (Int, Int, Sum Int))

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        elements [Nada, Yep a]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

checkOptional :: IO ()
checkOptional = quickBatch $ traversable (undefined :: Optional (Int, Int, Sum Int))

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
    foldMap f Nil = mempty
    foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
    traverse f Nil = pure Nil 
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        elements [Nil, Cons a Nil]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

checkList :: IO ()
checkList = quickBatch $ traversable (undefined :: List (Int, Int, Sum Int))

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

checkThree :: IO ()
checkThree = quickBatch $ traversable (undefined :: Three Int Int (Int, Int, Sum Int))

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b

instance Traversable (Two a) where
    traverse f (Two a b) = Two a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

checkTwo :: IO ()
checkTwo = quickBatch $ traversable (undefined :: Two Int (Int, Int, Sum Int))

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
    foldMap f (Big _ b c) = f b <> f c

instance Traversable (Big a) where
    traverse f (Big a b c) = Big a <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Big a b c

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

checkBig :: IO ()
checkBig = quickBatch $ traversable (undefined :: Big Int (Int, Int, Sum Int))


data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
    traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq

checkBigger :: IO ()
checkBigger = quickBatch $ traversable (undefined :: Bigger Int (Int, Int, Sum Int))

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S a b) = S (f <$> a) (f b)

instance  Foldable n => Foldable (S n) where
    foldMap f (S a b) = (foldMap f a) <> f b

instance Traversable n => Traversable (S n) where
    traverse f (S t a) = S <$> (traverse f t) <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
    (=-=) = eq

checkS :: IO ()
checkS = quickBatch $ traversable (undefined :: S [] (Int, Int, Sum Int))

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node p a q) = Node (fmap f p) (f a) (fmap f q)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node p a q) = (foldMap f p) <> f a <> (foldMap f q)

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node p a q) = Node <$> (traverse f p) <*> f a <*> (traverse f q)

treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
    xs <- treeGen
    ys <- treeGen
    a <- arbitrary
    elements [Leaf a, Empty, Node xs a ys]

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = treeGen

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

checkTree :: IO ()
checkTree = quickBatch $ traversable (undefined :: Tree (Int, Int, Sum Int))
