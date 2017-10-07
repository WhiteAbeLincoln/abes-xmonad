{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Control.Monad (filterM)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Graphics.X11.ExtraTypes.XF86

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Actions.Navigation2D (navigation2D, windowGo, windowSwap)
import XMonad.Actions.FloatSnap
import XMonad.Actions.Submap (submap)
import XMonad.Actions.Commands (defaultCommands, runCommand)
import XMonad.Util.NamedActions

import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(..))
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Util.EZConfig (removeKeys, additionalMouseBindings, additionalKeys, mkNamedKeymap)
import XMonad.Util.SpawnOnce (spawnOnce)
import qualified XMonad.Util.Dmenu as DM
import XMonad.Util.Run (hPutStrLn, spawnPipe)

-----------------------------------------------------------------------------
-- Functions
--
-- strip :: (Eq a, Foldable t) => t a -> [a] -> [a]
-- strip ys = filter $ not . (`elem` ys)
dmenu = DM.menuArgs "rofi" ["-dmenu", "-i"]

-----------------------------------------------------------------------------
-- Customized programs
--
myScreensaver :: [Char]
myScreensaver = ""
toggleScreensaver :: [Char]
toggleScreensaver = ""
myTerminal :: [Char]
myTerminal = "termite -e /usr/bin/tmux"

------------------------------------------------------------------------------
-- Workspaces
--
workspaceMap :: [(String,String)]
workspaceMap = [("term", "\xf120"), ("web", "\xf269"), ("editor", "\xf121"), ("games", "\xf1b6"), ("media", "\xf04b")]

getWorkspace :: String -> String
getWorkspace = fromMaybe "9" . flip lookup workspaceMap

myWorkspaces :: [String]
myWorkspaces = foldr (\x acc -> snd x:acc) [] workspaceMap ++ map show [(length workspaceMap + 1)..9]

------------------------------------------------------------------------------
-- Window Rules
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
    , [ isFullscreen   --> myDoFullFloat]
    ]) <+> manageDocks
  where myDoFullFloat = doF W.focusDown <+> doFullFloat
        myTerms = ["Termite", "xterm"]
        myWebs = ["Google-chrome", "Vivaldi-stable", "Firefox"]
        myEditors = ["jetbrains-idea", "jetbrains-pycharm", "Code", "Emacs", "GVim"]
        myGames = ["MultiMC5", "Steam"]
        myMedia = ["Spotify", "spotify", "Google Play Music Desktop Player"]
        myFloat = ["stalonetray"]

--------------------------------------------------------------------------------
-- Colors and Borders
--
myNormalBorderColor :: String
myNormalBorderColor = "#555555"

myFocusedBorderColor :: String
myFocusedBorderColor = "#dab3af"

myBorderWidth :: Dimension
myBorderWidth = 2

-------------------------------------------------------------------------------
-- Key Bindings
--
modm :: KeyMask
modm = mod4Mask -- changes mod key to super

altMask :: KeyMask
altMask = mod1Mask

------------------------------------------------------------------------------
-- Layouts
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
        full = renamed [Replace "full"] Full

        -- default tiling algorithm partitions the screen into 2 panes
        -- default number of windows in the master pane
        nmaster = 1

        -- default proportion of the screen occupied by master pane
        ratio = 2/3

        -- percent of screen to increment by when resizing panes
        delta = 1/100

-------------------------------------------------
-- Key Bindings
myKeys c =
  mkSM "Custom" "" myCustomKeys
  ++ mkSM "Toggles" "M-S-t" myToggles
  ++ mkSM "BSP" "M-b" myBSP
  ++ mkSM "Fn" "" myMediaKeys
  ++ mkSM "FloatSnap" "M-f" myFloatSnap
  where
    mkSM t [] l = (subtitle t:) $ mkNamedKeymap c $ l
    mkSM t s l  = (subtitle t:) $ mkNamedKeymap c $ fmap (\(k, a) -> (s++" "++k, a)) l
    myCustomKeys =
      [ ("M-S-<Return>", addName "Launch terminal" $ spawn $ XMonad.terminal c)
      , ("M-p",     addName "Launch rofi"        $ spawn "rofi -show run")
      , ("M-S-p",   addName "Launch programs"    $ spawn "j4-dmenu-desktop --dmenu='rofi -dmenu'")
      , ("M-<Tab>", addName "Window switcher"    $ spawn "rofi -show window")
      , ("M-i",     addName "Select layout"      $ jumpLayout)
      , ("M-o",     addName "Switch themes"      $ spawn "~/bin/themer")
      , ("M-C-l",   addName "Lock screen"        $ spawn myScreensaver)
      , ("M-C-c",   addName "Toggle screensaver" $ spawn toggleScreensaver)
      , ("<Print>", spawn' "scrot -e 'mv $f ~/Pictures/Screenshots'")
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
    myMediaKeys =
      [ ("<XF86AudioLowerVolume>",   spawn' "ponymix -N decrease 2")
      , ("<XF86AudioRaiseVolume>",  spawn' "ponymix -N increase 2")
      , ("<XF86AudioMute>",         spawn' "ponymix -N toggle")
      , ("<XF86AudioPlay>",         spawn' "playerctl play-pause")
      , ("<XF86AudioNext>",         spawn' "playerctl next")
      , ("<XF86AudioPrev>",         spawn' "playerctl previous")
      , ("<XF86MonBrightnessUp>",   spawn' "xbacklight -inc 10")
      , ("<XF86MonBrightnessDown>", spawn' "xbacklight -dec 10")
      ]
    myFloatSnap = 
       [ ("h",   addName "Left"         $ withFocused $ snapMove L Nothing)
       , ("l",   addName "Right"        $ withFocused $ snapMove R Nothing)
       , ("k",   addName "Up"           $ withFocused $ snapMove U Nothing)
       , ("j",   addName "Down"         $ withFocused $ snapMove D Nothing)
       , ("C-l", addName "Shrink Right" $ withFocused $ snapShrink R Nothing)
       , ("S-l", addName "Grow Right"   $ withFocused $ snapGrow R Nothing)
       , ("C-j", addName "Shrink Down"  $ withFocused $ snapShrink D Nothing)
       , ("S-j", addName "Grow Down"    $ withFocused $ snapGrow D Nothing)
       ]


startup :: X()
startup = do
    spawnOn (getWorkspace "term") myTerminal

toggleFloatingWindow :: String -> X()
toggleFloatingWindow name = do
  ss <- gets windowset
  let ws = W.index ss
  matchedWindows <- filterM (runQuery (className =? name)) ws
  mapM_ hide matchedWindows

main :: IO()
main = do
  xmonad
    $ ewmh
    $ docks
    -- $ navigation2D def (xK_w, xK_a, xK_s, xK_d) [(mod1Mask, windowGo), (mod1Mask .|. shiftMask, windowSwap)] False
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
      , startupHook        = startup
      , handleEventHook    = fullscreenEventHook <+> handleEventHook def
      }
      `removeKeys`
      [(modm .|. shiftMask, xK_slash)]
      `additionalMouseBindings`
      [
        -- FloatSnap
        ((modm,               button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 50) (Just 50) w)))
      , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)))
      , ((modm,               button3), (\w -> focus w >> mouseResizeWindow w >> afterDrag (snapMagicResize [R,D] (Just 50) (Just 50) w)))
      ]
