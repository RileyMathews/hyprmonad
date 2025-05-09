module Hyprmonad
    ( app
    ) where

import HyprLib.Socket
import HyprLib.Models
import Control.Monad
import Data.Aeson
import System.Environment
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath
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
        ["list"] -> listProfiles
        ["save", profile] -> saveProfile profile
        ["load", profile] -> loadProfile profile
        _ -> print "unknown command given"

encodeStrict :: ToJSON a => a -> BS.ByteString
encodeStrict = BL.toStrict . encode

saveProfile :: String -> IO ()
saveProfile profileName = do
    monitors <- getConnectedMonitors
    profilePath <- getProfilePath profileName
    -- save json encoded monitors to the file path
    BS.writeFile profilePath $ encodeStrict monitors
    pure ()

listProfiles :: IO ()
listProfiles = do
    -- list all profiles in the data directory with the .json stripped
    userHome <- getEnv "HOME"
    let dataDirectory = userHome <> "/.local/share/hyprmonad/"
    files <- listDirectory dataDirectory
    let stripped = takeBaseName <$> files
    mapM_ putStrLn stripped

-- nlistProfiles :: IO ()
-- nlistProfiles =
--     getEnv "HOME" >>= listDirectory . (<> "/.local/share/hyprmonad/") >>= (mapM_ putStrLn . map takeBaseName)

dispatchKeywordCommand :: Monitor -> IO ()
dispatchKeywordCommand monitor = do
    let command = keywordRestoreCommand monitor
    _ <- sendHyprCommand command 
    pure ()

dispatchDisableCommand :: Monitor -> IO ()
dispatchDisableCommand monitor = do
    let command = keywordDisableCommand monitor
    _ <- sendHyprCommand command
    pure ()

-- ndispatchDisableCommand :: Monitor -> IO ()
-- ndispatchDisableCommand monitor = sendHyprCommand (keywordDisableCommand monitor) >> pure ()

getConnectedMonitors :: IO [Monitor]
getConnectedMonitors = do
    mMonitors <- (decodeStrict :: BS.ByteString -> Maybe [Monitor]) <$> sendHyprCommand "j/monitors"
    case mMonitors of
        Nothing -> error "Failed to fetch monitors"
        Just monitors -> pure monitors

loadProfile :: String -> IO ()
loadProfile profileName = do
    fileContents <- getProfilePath profileName >>= BS.readFile
    let mMonitors = decodeStrict fileContents :: Maybe [Monitor]
    case mMonitors of
        Nothing -> putStrLn "Failed to fetch monitors"
        Just profileMonitors -> do
            connectedMonitors <- getConnectedMonitors
            let missingMonitors = filter (\m -> not $ elem m profileMonitors) connectedMonitors
            forM_ profileMonitors dispatchKeywordCommand
            forM_ missingMonitors dispatchDisableCommand
    pure ()

