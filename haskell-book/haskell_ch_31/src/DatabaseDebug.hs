{-# LANGUAGE OverloadedStrings #-}

module DatabaseDebug where

import Control.Exception
import Data.Text
import System.IO
import Database.SQLite.Simple (SQLError(..))

import Database
import Types

import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as T


getTextLine :: Text -> IO Text
getTextLine msg = do 
    putStr $ unpack msg
    hFlush stdout
    strip . pack  <$> getLine

handleInsertError :: SQLError -> IO ()
handleInsertError err = putStrLn . unpack $ T.concat 
    ["User couldn't be created: ", "\n\t", pack . show $ sqlError err, " - ", sqlErrorDetails err]


main :: IO ()
main = do
    user <- User 0 
        <$> getTextLine "Username: "
        <*> getTextLine "Shell: "
        <*> getTextLine "Home Dir: "
        <*> getTextLine "Real Name: "
        <*> getTextLine "Phone: "

    connection <- SQLite.open "finger.db"
    insertUser connection user `catch` handleInsertError 
