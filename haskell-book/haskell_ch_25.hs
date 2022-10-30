{-# LANGUAGE InstanceSigs #-}

import Data.Foldable

import Control.Applicative

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) 
  => Applicative (Compose f g) where

    pure :: a -> Compose f g a
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (<*>) (Compose fgab) (Compose fga) = Compose $ ((<*>) <$> fgab) <*> fga


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap ab (Compose fga) = (foldMap . foldMap) ab fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id


data Deux a b = Deux a b

instance Bifunctor Deux where

  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux a c) = Deux (f a) (g c)

data Const' a b = Const' a

instance Bifunctor Const' where

  first :: (a -> b) -> Const' a c -> Const' b c
  first f (Const' a) = Const' (f a)

  second :: (b -> c) -> Const' a b -> Const' a c
  second _ (Const' a) = Const' a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' a) = Left' $ f a
  bimap _ g (Right' b) = Right' $ g b


newtype IdentityT f a =
  IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa


instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa


instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
