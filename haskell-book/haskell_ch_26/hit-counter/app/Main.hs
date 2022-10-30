{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

import Control.Monad.IO.Class

type MapTI = M.Map Text Integer

data Config = 
    Config {
      counts :: IORef MapTI
    , prefix :: Text
    }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> MapTI -> MapTI
bumpBoomp k m = M.insertWith (+) k 1 m

gets :: (Config -> b) -> ReaderT Config IO b
gets f = ask >>= return . f

getKeyCounts :: Text -> IORef MapTI -> IO Integer
getKeyCounts key counter = do
    map <- readIORef counter
    return $ M.findWithDefault 0 key map

-- modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyConfig :: Text -> Config -> IO ()
modifyConfig key cfg = modifyIORef (counts cfg) $ bumpBoomp key

modify :: Text -> ReaderT Config IO ()
modify key = ask >>= liftIO . modifyConfig key

app :: Scotty ()
app =
    get "/:key" $ do
        unprefixed <- param "key"
        prefix <- lift $ gets prefix

        let key' = mappend prefix unprefixed

        lift $ modify key'

        counter <- lift $ gets counts
        updated <- liftIO $ getKeyCounts key' counter

        liftIO $ putStrLn $ show key' ++ " " ++ show updated 

        html $
            mconcat [ "<h1>Success! Count was: "
                    , TL.pack $ show updated
                    , "</h1>"
                    ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config { counts = counter, prefix = TL.pack prefixArg}
        -- runR :: ReaderT Config IO Response ->  IO Response
        runR m = runReaderT m config

    scottyT 3000 runR app
