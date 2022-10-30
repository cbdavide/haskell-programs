module Types (Config(..), User(..), UserRow) where

import Data.Text
import Database.SQLite.Simple.Types (Null)
import Database.SQLite.Simple (Connection, field, fromRow, FromRow, toRow, ToRow)
import Network.Socket (Socket)


data Config = Config { dbConnection :: Connection, clientSocket :: Socket }

data User =
    User {
          userId :: Integer
        , username :: Text
        , shell :: Text
        , homeDirectory :: Text
        , realName :: Text
        , phone :: Text

    } deriving (Eq, Show)


instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field


instance ToRow User where
    toRow (User id_ usrname shll dir name phn) =
        toRow (id_, usrname, shll, dir, name, phn)


type UserRow = (Null, Text, Text, Text, Text, Text)
