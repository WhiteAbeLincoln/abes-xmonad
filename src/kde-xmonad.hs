{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Control.Monad (filterM)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Graphics.X11.ExtraTypes.XF86

import XMonad hiding ((|||))

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isInProperty)
import qualified XMonad.StackSet as W

import XMonad.Actions.SpawnOn (spawnOn)

import XMonad.Util.EZConfig (removeKeys, additionalMouseBindings, additionalKeys, mkNamedKeymap)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.NamedActions
import qualified XMonad.Util.Dmenu as DM

-- {{{ Functions
--
dmenu = DM.menuArgs "rofi" ["-dmenu", "-i"]
-- }}}

-- {{{ Programs
--
myScreensaver :: [Char]
myScreensaver = ""
toggleScreensaver :: [Char]
toggleScreensaver = ""
myTerminal = "konsole"
--- }}}

-- {{{ Workspaces
--
workspaceMap :: [(String,String)]
workspaceMap = [("term", "\xf120"), ("web", "\xf269"), ("editor", "\xf121"), ("games", "\xf1b6"), ("media", "\xf04b")]
-- workspaceMap = [("term", "term"), ("web", "web"), ("editor", "editor"), ("games", "games"), ("media", "media")]

getWorkspace :: String -> String
getWorkspace = fromMaybe "9" . flip lookup workspaceMap

myWorkspaces :: [String]
myWorkspaces = foldr (\x acc -> snd x:acc) [] workspaceMap ++ map show [(length workspaceMap + 1)..9]
-- }}}

-- {{{ Window Rules
-- use xprop | grep WM_CLASS to find the name of a program
--
myManageHook :: Query (Endo WindowSet)
myManageHook = (composeAll . concat $
    [ [ className =? c --> doShift (getWorkspace "term")   | c <- myTerms   ]
    , [ className =? c --> doShift (getWorkspace "web")    | c <- myWebs    ]
    , [ className =? c --> doShift (getWorkspace "editor") | c <- myEditors ]
    , [ className =? c --> doShift (getWorkspace "games")  | c <- myGames   ]
    , [ className =? c --> doShift (getWorkspace "media")  | c <- myMedia   ]
    , [ className =? c --> doFloat                         | c <- myFloat   ]
    , [ className =? "krunner" --> doIgnore ]
    , [ isFullscreen   --> myDoFullFloat ]
    , [ wantsFloat <||> isNotification --> doFloat ]
    ]) <+> manageDocks
  where myDoFullFloat = doF W.focusDown <+> doFullFloat
        wantsFloat = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STAYS_ON_TOP" <||> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE" <||> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_BELOW"
        isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"
        myTerms = ["Termite", "xterm", "konsole"]
        myWebs = ["Google-chrome", "Vivaldi-stable", "Firefox", "Firefox Developer Edition", "Chromium"]
        myEditors = ["jetbrains-idea", "jetbrains-pycharm", "Code", "Emacs", "GVim"]
        myGames = ["MultiMC5", "Steam"]
        myMedia = ["Spotify", "Google Play Music Desktop Player"]
        myFloat = []
-- }}}

-- {{{ Colors and Borders
--
myNormalBorderColor :: String
myNormalBorderColor = "#555555"

myFocusedBorderColor :: String
myFocusedBorderColor = "#dab3af"

myBorderWidth :: Dimension
myBorderWidth = 2
-- }}}

-- {{{ Layouts
--
jumpLayout :: X ()
jumpLayout = do
  r <- dmenu $ layoutNames
  sendMessage $ JumpToLayout r
  where layoutNames = ["bsp", "tall", "mirror-tall", "full"]

data MyToggles
    = GAPPED deriving (Read, Show, Eq, Typeable)

instance Transformer MyToggles Window where
    transform GAPPED x k = k (smartSpacingWithEdge 10 x) (const x)

myLayout = mkToggle (NOBORDERS ?? FULL ?? EOT) . mkToggle1 GAPPED $ layouts
    where
        layouts = bsp ||| tall ||| mirrorTall ||| full
        bsp  = renamed [Replace "bsp"] emptyBSP
        tall = renamed [Replace "tall"] $ Tall nmaster delta ratio
        mirrorTall = renamed [Replace "mirror-tall"] $ Mirror tall
        full = renamed [Replace "full"] (noBorders $ Full)

        -- default tiling algorithm partitions the screen into 2 panes
        -- default number of windows in the master pane
        nmaster = 1

        -- default proportion of the screen occupied by master pane
        ratio = 2/3

        -- percent of screen to increment by when resizing panes
        delta = 1/100
-- }}}

-- Key Bindings {{{
modm :: KeyMask
modm = mod4Mask -- changes mod key to super

altMask :: KeyMask
altMask = mod1Mask

myKeys c =
  mkSM "Custom" "" myCustomKeys
  ++ mkSM "Toggles" "M-S-t" myToggles
  ++ mkSM "BSP" "M-b" myBSP
  -- ++ mkSM "Fn" "" myMediaKeys
  where
    mkSM t [] l = (subtitle t:) $ mkNamedKeymap c $ l
    mkSM t s l  = (subtitle t:) $ mkNamedKeymap c $ fmap (\(k, a) -> (s++" "++k, a)) l
    myCustomKeys =
      [ ("M-S-<Return>", addName "Launch terminal" $ spawn $ XMonad.terminal c)
      , ("M-p",     addName "Launch rofi"        $ spawn "rofi -show run")
      , ("M-S-p",   addName "Launch programs"    $ spawn "j4-dmenu-desktop --dmenu='rofi -dmenu'")
      , ("M-<Tab>", addName "Window switcher"    $ spawn "rofi -show window")
      , ("M-S-q",   addName "Leave"              $ spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1")
      , ("M-i",     addName "Select layout"      $ jumpLayout)
    --  , ("M-o",     addName "Switch themes"      $ spawn "~/bin/themer")
    --  , ("M-C-l",   addName "Lock screen"        $ spawn myScreensaver)
    --  , ("M-C-c",   addName "Toggle screensaver" $ spawn toggleScreensaver)
    --  , ("<Print>", spawn' "scrot -e 'mv $f ~/Pictures/Screenshots'")
      ]
    myToggles =
      [ ("g", addName "Toggle gaps"        $ sendMessage $ Toggle GAPPED)
      , ("z", addName "Toggle full"        $ sendMessage $ Toggle FULL)
      ]
    myBSP =
      [ ("l",     addName "Expand Right"     $ sendMessage $ ExpandTowards R)
      , ("h",     addName "Expand Left"      $ sendMessage $ ExpandTowards L)
      , ("j",     addName "Expand Down"      $ sendMessage $ ExpandTowards D)
      , ("k",     addName "Expand Up"        $ sendMessage $ ExpandTowards U)
      , ("C-l",   addName "Shrink Right"     $ sendMessage $ ShrinkFrom R)
      , ("C-h",   addName "Shrink Left"      $ sendMessage $ ShrinkFrom L)
      , ("C-j",   addName "Shrink Down"      $ sendMessage $ ShrinkFrom D)
      , ("C-k",   addName "Shrink Up"        $ sendMessage $ ShrinkFrom U)
      , ("<Enter>",   addName "Swap"         $ sendMessage $ Swap)
      , ("r",         addName "Rotate"       $ sendMessage $ Rotate)
      , ("p",         addName "Focus Parent" $ sendMessage $ FocusParent)
      , ("n",         addName "Select Node"  $ sendMessage $ SelectNode)
      , ("m",         addName "Move Node"    $ sendMessage $ MoveNode)
      ]
   -- myMediaKeys =
   --   [ ("<XF86AudioLowerVolume>",   spawn' "ponymix -N decrease 2")
   --   , ("<XF86AudioRaiseVolume>",  spawn' "ponymix -N increase 2")
   --   , ("<XF86AudioMute>",         spawn' "ponymix -N toggle")
   --   , ("<XF86AudioPlay>",         spawn' "playerctl play-pause")
   --   , ("<XF86AudioNext>",         spawn' "playerctl next")
   --   , ("<XF86AudioPrev>",         spawn' "playerctl previous")
   --   , ("<XF86MonBrightnessUp>",   spawn' "xbacklight -inc 10")
   --   , ("<XF86MonBrightnessDown>", spawn' "xbacklight -dec 10")
   --   ]
-- }}}

main = do
  xmonad
   $ ewmh
   $ docks
   $ addDescrKeys ((modm .|. shiftMask, xK_slash), xMessage) myKeys
   $ def
      { terminal           = myTerminal
      , modMask            = modm
      , workspaces         = myWorkspaces
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , borderWidth        = myBorderWidth
      , manageHook         = myManageHook
      , layoutHook         = avoidStruts $ smartBorders $ myLayout
      , handleEventHook    = fullscreenEventHook <+> handleEventHook def
      }
      `removeKeys`
      [(modm .|. shiftMask, xK_slash)]

-- vim: foldmethod=marker foldlevel=0
