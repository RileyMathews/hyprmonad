module HyprLib.Socket where

import System.Environment (getEnv)
import Network.Socket
import qualified Data.ByteString.Char8 as BS
import Network.Socket.ByteString (recv, sendAll)

getHyprSocketPath :: IO FilePath
getHyprSocketPath = do
    instanceSig <- getEnv "HYPRLAND_INSTANCE_SIGNATURE"
    return $ "/run/user/1000/hypr/" ++ instanceSig ++ "/.socket.sock"

getHyprSocket :: FilePath -> IO Socket
getHyprSocket filePath = do
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix filePath)
    return sock

sendHyprCommand :: Socket -> String -> IO BS.ByteString
sendHyprCommand sock command = do
    sendAll sock (BS.pack command)
    response <- getHyprResponse sock
    return response

getHyprResponse :: Socket -> IO BS.ByteString
getHyprResponse = recvResponseWithAccum BS.empty
    where
        bufSize = 4096

        recvResponseWithAccum :: BS.ByteString -> Socket -> IO BS.ByteString
        recvResponseWithAccum accum sock = do
            chunk <- recv sock bufSize
            if BS.null chunk
                then pure accum
                else recvResponseWithAccum (BS.append accum chunk) sock

debugHyprResponse :: BS.ByteString -> IO ()
debugHyprResponse text = BS.putStrLn text
