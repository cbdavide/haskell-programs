{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Data.Either

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT(..))


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where

    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where

    pure :: a -> EitherT e m a
    pure a = EitherT $ (pure . pure) a

    -- (<*>) :: f (a -> b) -> f a -> f b
    -- <*> is lifted over emab to transform: m (Either e (a -> b))
    -- into m (Either e a -> Either e b), and then we can use
    -- apply for the outer structure
    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT emab) <*> (EitherT ema) = EitherT $ (<*>) <$> emab <*> ema

instance Monad m => Monad (EitherT e m) where
    
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    -- (EitherT ema) >>= f = EitherT $ ema >>= either (return . Left) (runEitherT . f)
    (EitherT ema) >>= f = EitherT $ do

        v <- ema

        case v of
          (Left e) -> return $ Left e
          (Right a) -> runEitherT (f a)


swapEither :: Either e a -> Either a e
swapEither (Left a) = Right a 
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT x) = EitherT $ swapEither <$> x

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
-- eitherT l r (EitherT amb) = join $ either l r <$> amb
-- amb :: m x -> (x -> m c)
-- either l r :: x -> m c; l :: (a -> mc); r :: (b -> m c)
eitherT l r (EitherT amb) = amb >>= either l r


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where

    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where

    pure :: a -> ReaderT r m a
    pure a = ReaderT $ (pure . pure) a

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma

instance Monad m => Monad (ReaderT r m) where

    return :: a -> ReaderT r m a
    return = pure

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= f = ReaderT $ \r -> do
        -- rma :: r -> m a
        a <- rma r

        -- f :: a -> ReaderT r m b
        -- runReaderT :: Reader r m b -> r -> m b
        -- runReaderT (f a) r :: m b
        runReaderT (f a) r

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where

    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sma) = StateT $ \s -> fmap (\(a, s) -> (f a, s)) (sma s)

instance (Monad m) => Applicative (StateT s m) where

    pure :: a -> StateT s m a
    pure a = StateT $ \s -> pure (a, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT smab) <*> (StateT sma) = StateT $ \s -> do
        (f, t) <- smab s
        (a, p) <- sma t
        return (f a, p)

    -- The hard to read oneliner
    -- (StateT smab) <*> (StateT sma) = StateT $
    --     \s -> (smab s) >>= \(f, t) -> fmap (\(a, p) -> (f a, p)) (sma t)

instance (Monad m) => Monad (StateT s m) where

    return :: a -> StateT s m a
    return a = StateT $ \s -> return (a, s)

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= f = StateT $ \s -> do
        -- sma :: s -> m (a, s)
        (a, t) <- sma s
        -- f a :: StateT s m b
        -- runStateT (f a) :: s -> m b
        runStateT (f a) t


embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap  :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

instance MonadTrans (EitherT r) where

    lift :: Monad m => m a -> EitherT r m a
    lift = EitherT . liftM Right


instance MonadTrans (StateT s) where

    lift :: Monad m => m a -> StateT s m a
    lift ma = StateT $ \s -> do
       a <- ma
       return (a, s)  -- :: m (a, s)


class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO
