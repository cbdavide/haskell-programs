{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad (forever)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple (Connection, SQLError(..))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Say (sayString)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import qualified Network.Socket as S

import App
import Database
import Lib
import Types


formatUser :: User -> ByteString
formatUser (User _ usrname shll homeDir name _) = BS.concat
    ["Login: ", e usrname, "\t\t\t\t",
     "Name: ", e name, "\n",
     "Directory: ", e homeDir, "\t\t",
     "Shell: ", e shll, "\n"]
    where e = encodeUtf8


returnUsers :: App ()
returnUsers = do
    soc <- asks clientSocket
    dbConn <- asks dbConnection

    users <- liftIO $ getAllUsers dbConn

    let usernames = map username users
        newlineSeparated = T.concat $ intersperse "\n" usernames

    liftIO $ sendAll soc (encodeUtf8 newlineSeparated)


returnUser :: Text -> App ()
returnUser uname = do
    soc <- asks clientSocket
    dbConn <- asks dbConnection

    maybeUser <- liftIO $ getUser dbConn (T.strip uname)

    case maybeUser of
      Nothing ->
        sayString $ "Couldn't find matching user for username: " ++ show (T.strip uname)
      Just user -> liftIO $ sendAll soc (formatUser user)


createUser :: User -> App ()
createUser user = do
    soc <- asks clientSocket
    dbConn <- asks dbConnection

    liftIO $ insertUser' dbConn soc `catch` handleInsertError soc

  where
    insertUser' dbConn soc = do
        insertUser dbConn user
        liftIO $ sendAll soc (formatUser user)

    handleInsertError :: Socket -> SQLError -> IO () 
    handleInsertError soc err = do
        liftIO $ sendAll soc (BS.concat [
              "User couldn't be created: "
            , encodeUtf8 . T.pack . show . sqlError $ err
            , encodeUtf8 . sqlErrorDetails $ err
            , "\n"])


handleCreateUserRequest :: App ()
handleCreateUserRequest = do
    soc <- asks clientSocket
    msg <- liftIO $ recv soc 1024

    let decodedInput = decodeUtf8 msg
        user = unmarshalUser decodedInput

    case user of
      Just u -> createUser u
      Nothing -> do
          sayString $ "Invalid request: " ++ (T.unpack decodedInput)
          liftIO $ sendAll soc (BS.concat ["Wrong user information: ", msg])


userCreationServer :: Socket -> Connection -> IO ()
userCreationServer sock conn = forever $ do
    (soc, _) <- accept sock
    sayString "Got user request connection, handling query"
    let conf = Config { clientSocket = soc, dbConnection = conn }
    runApp handleCreateUserRequest conf
    close soc


handleQuery :: App ()
handleQuery = do
    soc <- asks clientSocket
    msg <- liftIO $ recv soc 1024

    case msg of
      "\r\n" -> returnUsers
      name -> returnUser (decodeUtf8 name)


handleQueries :: Socket -> Connection -> IO ()
handleQueries sock conn = forever $ do
    (soc, _) <- accept sock
    sayString "Got connection, handling query"

    let conf = Config { clientSocket = soc, dbConnection = conn }

    runApp handleQuery conf

    close soc


createAddrInfo :: String -> IO AddrInfo
createAddrInfo port = do
    infos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    return . head $ infos


createSocket :: String -> IO Socket
createSocket port = do
    addrInfo <- createAddrInfo port
    sock <- socket (addrFamily addrInfo) Stream defaultProtocol

    bind sock (addrAddress addrInfo)

    listen sock 1 -- only one connection open at a time
    
    return sock


main :: IO ()
main =  withSocketsDo $ do
    dbConn <- SQLite.open "finger.db"

    fingerdSocket <- createSocket "79"
    userSocket <- createSocket "8079"

    fingerdThread <- async $ handleQueries fingerdSocket dbConn
    usersThread <- async $ userCreationServer userSocket dbConn

    _ <- waitBoth fingerdThread usersThread

    sayString "Closing connections..."

    S.close fingerdSocket
    S.close userSocket

    SQLite.close dbConn
