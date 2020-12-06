{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
(
  CKeybinding(key, value, name)
, CConfig(terminal, normalBorderColor, focusedBorderColor, borderWidth, dmenu, dmenuArgs, keybindings)
, decodeConfig
, defaultConfig
,
) where
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)

data CKeybinding = CKeybinding {
  key :: String
, name :: String
, value :: String
} deriving (Show, Eq)

data CConfig = CConfig {
  terminal :: CKeybinding
, normalBorderColor :: String
, focusedBorderColor :: String
, borderWidth :: Int
, dmenu :: String
, dmenuArgs :: [String]
, keybindings :: [CKeybinding]
} deriving (Show, Eq)

defaultConfig :: CConfig
defaultConfig = CConfig {
  terminal = CKeybinding {
    value = "xterm"
  , key = "M-S-<Return>"
  , name = "Launch terminal"
  }
, normalBorderColor = "#555555"
, focusedBorderColor = "#1abc9c"
, borderWidth = 2
, dmenu = "dmenu"
, dmenuArgs = []
, keybindings = []
}

instance FromJSON CKeybinding where
  parseJSON = withObject "keybinding" $ \o -> do
    key <- o .: "key"
    name <- o .: "name"
    value <- o .: "value"
    return CKeybinding{..}

instance FromJSON CConfig where
  parseJSON =
    let
      CConfig{
        terminal = defTerminal
      , normalBorderColor = defNormalBorderColor
      , focusedBorderColor = defFocusedBorderColor
      , borderWidth = defBorderWidth
      , dmenu = defDmenu
      , dmenuArgs = defDmenuArgs
      , keybindings = defKeybindings
      } = defaultConfig
    in
      withObject "config" $ \o -> do
        terminal <- o .:? "terminal" .!= defTerminal
        normalBorderColor <- o .:? "normalBorderColor" .!= defNormalBorderColor
        focusedBorderColor <- o .:? "focusedBorderColor" .!= defFocusedBorderColor
        borderWidth <- o .:? "borderWidth" .!= defBorderWidth
        dmenu <- o .:? "dmenu" .!= defDmenu
        dmenuArgs <- o .:? "dmenuArgs" .!= defDmenuArgs
        keybindings <- o .:? "keybindings" .!= defKeybindings
        return CConfig{..}

decodeConfig = fromMaybe defaultConfig . decode
