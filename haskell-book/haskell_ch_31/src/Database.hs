{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database where

import Control.Exception (throwIO)
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple.Types (Null(..), Query, Only(..))
import Data.Text (Text)
import Text.RawString.QQ

import Exceptions
import Types

import qualified Database.SQLite.Simple as SQLite


createUsersTable :: Query
createUsersTable  = [r|
CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     username TEXT UNIQUE,
     shell TEXT,
     homeDirectory TEXT,
     realName TEXT,
     phone TEXT)
|]

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

updateUserQuery :: Query
updateUserQuery = [r|
UPDATE users 
SET shell = ?, homeDirectory = ?, realName = ?, phone = ?
WHERE username = ?;
|]

allUsersQuery :: Query
allUsersQuery = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username' = do
    results <- SQLite.query conn getUserQuery (Only username')

    case results of
      [] -> return Nothing
      [user] -> return $ Just user
      _ -> throwIO DuplicateData

insertUser :: Connection -> User -> IO ()
insertUser conn User{..} = do
    SQLite.execute conn insertUserQuery userRow
  where
    userRow :: UserRow
    userRow = (Null, username, shell, homeDirectory, realName, phone)

updateUser :: Connection -> User -> IO ()
updateUser conn User{..} = do
    SQLite.execute conn updateUserQuery userRow
  where
    userRow = (shell, homeDirectory, realName, phone, username)


getAllUsers :: Connection -> IO [User]
getAllUsers conn = SQLite.query_ conn allUsersQuery

createDatabase :: IO ()
createDatabase = do
    conn <- SQLite.open "finger.db"
    SQLite.execute_ conn createUsersTable

    SQLite.close conn
