import Data.Bool
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a ->t a -> Bool
elem' a = getAny . foldMap (Any . (== a))

aux :: Ord a => (a -> a -> a) -> a -> Maybe a -> Maybe a
aux _ a Nothing = Just a
aux f b (Just a) = Just $ f a b

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\a b -> aux min a b) Nothing

maximum':: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\a b -> aux max a b) Nothing

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = getSum . foldMap (const (Sum 1))

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (\x -> [x])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (\a b -> f a `mappend` b) mempty

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' _ a b) = f a <> f b

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' _ b c d) = f b <> f c <> f d

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> bool mempty (pure x) (f x))
