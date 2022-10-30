module App where

import Control.Monad.Reader(ReaderT, runReaderT)

import Types

type App a = ReaderT Config IO a

runApp :: App a -> Config -> IO a
runApp app conf = runReaderT app conf
