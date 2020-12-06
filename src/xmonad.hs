{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (liftM, filterM, when, join)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Endo(..))
import Graphics.X11.ExtraTypes.XF86

import XMonad hiding ((|||))

import XMonad.Layout.NoBorders (smartBorders, noBorders, hasBorder)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.MultiToggle
    ( (??), mkToggle, EOT(EOT), Toggle(Toggle) )
import XMonad.Layout.MultiToggle.Instances

import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageDocks (docks, manageDocks, avoidStruts, ToggleStruts(..), Direction2D(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isInProperty, isKDETrayWindow)
import qualified XMonad.StackSet as W

import XMonad.Actions.SpawnOn (spawnOn)

import XMonad.Util.EZConfig (removeKeys, additionalMouseBindings, additionalKeys, mkNamedKeymap)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.NamedActions
import qualified XMonad.Util.Dmenu as DM
import Config (decodeConfig, CConfig)
import qualified Config as C
import EnvConfig (getConfigText)

-- Config {{{
--
configStr = $( getConfigText "CONFIG_FILE" "CONFIG_SRC" )
myConfig = decodeConfig configStr

dmenu = DM.menuArgs (C.dmenu myConfig) (C.dmenuArgs myConfig)
myTerminal = C.value . C.terminal $ myConfig
myNormalBorderColor = C.normalBorderColor $ myConfig
myFocusedBorderColor = C.focusedBorderColor $ myConfig
myBorderWidth :: Dimension
myBorderWidth = fromIntegral $ C.borderWidth $ myConfig
-- }}}

-- Workspaces {{{
--
-- workspaceMap = [("term", "\xf120"), ("web", "\xf269"), ("editor", "\xf121"), ("games", "\xf1b6"), ("media", "\xf04b")]
workspaceMap = [("term", "term"), ("web", "web"), ("editor", "editor"), ("games", "games"), ("media", "media")]

getWorkspace = fromMaybe "9" . flip lookup workspaceMap

myWorkspaces = foldr (\x acc -> snd x:acc) [] workspaceMap ++ map show [(length workspaceMap + 1)..9]
-- }}}

-- Window Rules {{{
-- use xprop | grep WM_CLASS to find the name of a program. className is the second elem of WM_CLASS(STRING)
myManageHook :: Query (Endo WindowSet)
myManageHook = (composeAll . concat $
    [ [ className =? c --> doShift (getWorkspace "term")   | c <- myTerms   ]
    , [ className =? c --> doShift (getWorkspace "web")    | c <- myWebs    ]
    , [ className =? c --> doShift (getWorkspace "editor") | c <- myEditors ]
    , [ className =? c --> doShift (getWorkspace "games")  | c <- myGames   ]
    , [ className =? c --> doShift (getWorkspace "media")  | c <- myMedia   ]
    , [ className =? c --> doFloat                         | c <- myFloat   ]
    , [ className =? c --> doIgnore <+> hasBorder False >> doFloat | c <- myIgnoreF ]
    , [ isFullscreen   --> myDoFullFloat ]
    , [ wantsFloat <||> isNotification <||> isOSD --> doFloat ]
    ]) <+> manageDocks
  where myDoFullFloat = doF W.focusDown <+> doFullFloat
        wantsFloat = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STAYS_ON_TOP" <||> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE" <||> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_BELOW"
        isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"
        isOSD = isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_ON_SCREEN_DISPLAY"
        myTerms = ["Termite", "xterm", "konsole"]
        myWebs = [ "Google-chrome"
                 , "Vivaldi-stable"
                 , "Firefox"
                 , "Firefox Developer Edition"
                 , "Chromium"
                 , "firefoxdeveloperedition"
                 ]
        myEditors = ["jetbrains-idea", "Code", "Emacs", "GVim"]
        myGames = ["MultiMC5", "Steam"]
        myMedia = ["Spotify", "spotify"]
        myFloat = ["stalonetray"]
        myIgnoreF = ["krunner", "plasmashell"]
-- }}}

-- Layouts {{{
--
jumpLayout :: X ()
jumpLayout = do
  r <- dmenu layoutNames
  sendMessage $ JumpToLayout r
  where layoutNames = ["bsp", "tall", "mirror-tall", "full", "focus"]

-- data MyToggles
--     = GAPPED deriving (Read, Show, Eq, Typeable)

-- instance Transformer MyToggles Window where
--     transform GAPPED x k = k (smartSpacingWithEdge 10 x) (const x)

myLayout = mkToggle (NOBORDERS ?? FULL ?? EOT) $ layouts
    where
      layouts = avoidStruts (bsp ||| tall ||| mirrorTall ||| full) ||| focus
      bsp  = renamed [Replace "bsp"] emptyBSP
      tall = renamed [Replace "tall"] $ Tall nmaster delta ratio
      mirrorTall = renamed [Replace "mirror-tall"] $ Mirror tall
      full = renamed [Replace "full"] (noBorders $ Full)
      focus = renamed [Replace "focus"] (noBorders $ Full)

      -- default tiling algorithm partitions the screen into 2 panes
      -- default number of windows in the master pane
      nmaster = 1

      -- default proportion of the screen occupied by master pane
      ratio = 2/3

      -- percent of screen to increment by when resizing panes
      delta = 1/100
-- }}}

-- Key Bindings {{{
--
modm :: KeyMask
modm = mod4Mask -- changes mod key to super

altMask :: KeyMask
altMask = mod1Mask

myKeys c =
  mkSM "Custom" "" (myCustomKeys ++ myConfigKeys ++ [myTerminalBinding])
  ++ mkSM "Toggles" "M-S-t" myToggles
  ++ mkSM "BSP" "M-b" myBSP
  -- ++ mkSM "Fn" "" myMediaKeys
  where
    mkSM t [] l = (subtitle t:) $ mkNamedKeymap c $ l
    mkSM t s l  = (subtitle t:) $ mkNamedKeymap c $ fmap (\(k, a) -> (s++" "++k, a)) l
    fromKeybinding k = (C.key k, addName (C.name k) $ spawn (C.value k))

    myConfigKeys = map fromKeybinding $ C.keybindings $ myConfig
    myTerminalBinding = let
      term = C.terminal myConfig
      in
        (C.key term, addName (C.name term) $ spawn $ XMonad.terminal c)
    myCustomKeys =
      [ ("M-i",     addName "Select layout"      $ jumpLayout) ]
    myToggles =
      [ -- ("g", addName "Toggle gaps"        $ sendMessage $ Toggle GAPPED)
        ("z", addName "Toggle full"        $ sendMessage $ Toggle FULL)
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
    --   [
    --     ("<XF86AudioLowerVolume>",   spawn' "ponymix -N decrease 2")
    --   , ("<XF86AudioRaiseVolume>",  spawn' "ponymix -N increase 2")
    --   , ("<XF86AudioMute>",         spawn' "ponymix -N toggle")
    --   , ("<XF86AudioPlay>",         spawn' "playerctl play-pause")
    --   , ("<XF86AudioNext>",         spawn' "playerctl next")
    --   , ("<XF86AudioPrev>",         spawn' "playerctl previous")
    --   , ("<XF86MonBrightnessUp>",   spawn' "xbacklight -inc 10")
    --   , ("<XF86MonBrightnessDown>", spawn' "xbacklight -dec 10")
    --   ]
-- }}}

-- Startup {{{
--
-- Fix for firefox not going fullscreen
-- Credit https://gist.github.com/sgf-dma/a609f855bbacf1a0292e660c32a5a04e
-- https://github.com/cstrahan/dotfiles/blob/b28910dca345f5030969828fae1fdbb9982e0042/.xmonad/xmonad.hs#L64-L82

-- the sxiv app (and maybe others) believes that fullscreen is not supported,
-- so this fixes that.
-- see: https://mail.haskell.org/pipermail/xmonad/2017-March/015224.html
-- and: https://github.com/xmonad/xmonad-contrib/pull/109
addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r               <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a               <- getAtom "ATOM"
  liftIO $ do
    sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

startup :: X()
startup = do
  addEWMHFullscreen
  -- autostart using XDG specification
  -- spawnOnce "/bin/sh -c 'dex -a >/dev/null'"
  spawnOn (getWorkspace "term") myTerminal

-- }}}

main :: IO()
main = do
  xmonad
    $ addDescrKeys ((modm .|. shiftMask, xK_slash), xMessage) myKeys
    $ docks
    $ ewmh def
      { terminal           = myTerminal
      , modMask            = modm
      , workspaces         = myWorkspaces
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , borderWidth        = myBorderWidth
      , manageHook         = myManageHook
      , layoutHook         = smartBorders $ myLayout
      , startupHook        = startup
      , handleEventHook    = fullscreenEventHook <+> handleEventHook def
      }
      `removeKeys`
      [(modm .|. shiftMask, xK_slash)]

-- vim: foldmethod=marker foldlevel=0
