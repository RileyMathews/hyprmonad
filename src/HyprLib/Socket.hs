module HyprLib.Socket (sendHyprCommand) where

import System.Environment (getEnv)
import Network.Socket
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.Socket.ByteString (recv, sendAll)

getHyprSocketPath :: IO FilePath
getHyprSocketPath = do
    instanceSig <- getEnv "HYPRLAND_INSTANCE_SIGNATURE"
    return $ "/run/user/1000/hypr/" ++ instanceSig ++ "/.socket.sock"

getHyprSocket :: IO Socket
getHyprSocket = do
    sock <- socket AF_UNIX Stream defaultProtocol
    filePath <- getHyprSocketPath
    connect sock (SockAddrUnix filePath)
    return sock

sendHyprCommand :: String -> IO BS.ByteString
sendHyprCommand command = do
    sock <- getHyprSocket
    sendAll sock (BSC.pack command)
    response <- getHyprResponse sock
    close sock
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

