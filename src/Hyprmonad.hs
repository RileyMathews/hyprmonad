module Hyprmonad
    ( app
    ) where

import HyprLib.Socket
import HyprLib.Models
import Data.Aeson
import System.Environment
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

getProfilePath :: String -> IO FilePath
getProfilePath profileName = do
    userHome <- getEnv "HOME"
    let dataDirectory = userHome <> "/.local/share/hyprmonad/"
    -- create dataDirectory if it does not exist
    createDirectoryIfMissing True dataDirectory
    return $ dataDirectory <> profileName <> ".json"
    

app :: IO ()
app = do
    args <- getArgs
    case args of
        ["list"] -> print "listing profiles"
        ["save", profile] -> saveProfile profile
        ["load", profile] -> print $ "loading " <> profile
        _ -> print "unknown command given"
    print args
    sock <- getHyprSocket
    resp <- sendHyprCommand sock "j/monitors"
    let mMonitors = decodeStrict resp :: Maybe [Monitor]
    print mMonitors

encodeStrict :: ToJSON a => a -> BS.ByteString
encodeStrict = BL.toStrict . encode

saveProfile :: String -> IO ()
saveProfile profileName = do
    sock <- getHyprSocket
    mMonitors <- (decodeStrict :: BS.ByteString -> Maybe [Monitor]) <$> sendHyprCommand sock "j/monitors"
    case mMonitors of
        Nothing -> print "error fetching monitor configuration"
        Just monitors -> do
            profilePath <- getProfilePath profileName
            -- save json encoded monitors to the file path
            BS.writeFile profilePath $ encodeStrict monitors
    pure ()
