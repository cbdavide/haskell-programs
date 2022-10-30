{-# LANGUAGE OverloadedStrings #-}

module Debug (main) where

import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)


logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
    -- accept :: Socket -> IO (Socket, SockAddr)
    putStrLn "waiting for connection..."
    (soc, _) <- accept sock
    putStrLn "connection acquired"
    printAndKickback soc
    -- close :: Socket -> IO ()
    close soc

    where 
        printAndKickback :: Socket -> IO ()
        printAndKickback conn = do
            -- recv :: Socket -> Int -> IO ByteString
            msg <- recv conn 1024
            print msg
            -- sendAll :: Socket -> ByteString -> IO ()
            sendAll conn "mmmm...."
            sendAll conn msg

-- withSocketDo :: IO a -> IO a
main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting server..."
    -- getAddrInfo :: Maybe AddrInfo Maybe HostName Maybe ServiceName -> IO [AddrInfo]
    addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "79")
    let serveraddr = head addrinfos :: AddrInfo
    -- socket :: FamilyType -> SocketType -> ProtocolNumber -> IO Socket
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    -- bind :: Socket -> SockAddr -> IO ()
    bind sock (addrAddress serveraddr)
    -- listen :: Socket -> Int -> IO ()
    -- second argument is the maximum number of queued connections
    listen sock 1
    logAndEcho sock
    -- close :: Socket -> IO ()
    close sock
