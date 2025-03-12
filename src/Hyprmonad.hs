module Hyprmonad
    ( app
    ) where

import HyprLib.Socket

app :: IO ()
app = do
    sockPath <- getHyprSocketPath
    sock <- getHyprSocket sockPath
    resp <- sendHyprCommand sock "j/monitors"
    debugHyprResponse resp
