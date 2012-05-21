-- Martin Santos XMonad condig file
--  * Depends on XMonad 0.9


import XMonad
import XMonad.Core
import XMonad.Operations
import XMonad.ManageHook
import XMonad.Config.Desktop (desktopLayoutModifiers)

import XMonad.Layout
import XMonad.Layout.Roledex
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Master
import XMonad.Layout.Gaps
import XMonad.Layout.Reflect
import XMonad.Layout.DwmStyle

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes

import XMonad.Actions.WindowBringer
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Input
import XMonad.Prompt.AppLauncher as AL

import XMonad.Config.Gnome

import qualified Data.Map as M
import Data.Bits
import Data.Ratio ((%))
import System.IO (hPutStrLn)
import Graphics.X11.Xlib
import System.Exit


myWorkspaces = ["im", "www", "downloads", "music", "1", "2", "3", "4"]

myTerminal = "gnome-terminal"

myModMask = mod4Mask

myLayoutHook = smartBorders $ avoidStruts ( onWorkspace "im" (withIM (1/8) (Title "Buddy List") (Grid ||| tiled ||| tabs)) $
                             onWorkspace "www" tabs
                             ( tiled ||| tabs ||| full ||| Grid ||| mtiled ||| threecol )
                            ) where
                                tabs     = noBorders $ desktopLayoutModifiers $ tabbed shrinkText (theme deiflTheme)
                                full     = noBorders Full
                                tiled    = ResizableTall 1 (3/100) (60/100) []
                                mtiled   = Mirror tiled
                                threecol = ThreeCol 1 (3/100) (1/2)

-- Additional keybindings
mykeys c@(XConfig {modMask = modm}) = M.fromList $
                                    [ ((modm,               xK_h     ), sendMessage Shrink)
                                    , ((modm,               xK_l     ), sendMessage Expand)
                                    , ((modm,               xK_u     ), sendMessage MirrorExpand)
                                    , ((modm,               xK_i     ), sendMessage MirrorShrink)

                                    , ((modm,               xK_c     ), kill1)
                                    , ((modm .|. shiftMask, xK_c     ), kill) -- Close the focused window

                                    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- Quit xmonad
                                    , ((modm              , xK_q     ), restart "xmonad" True)     -- Restart xmonad
                                    , ((modm              , xK_s     ), spawnSelected defaultGSConfig ["pidgin", "firefox",
                                                                                                       "google-chrome", "gedit",
                                                                                                       "rhythmbox"])
                                    -- *** Open file or folder
                                    , ((modm              , xK_f     ), runOrRaisePrompt defaultXPConfig)
                                    -- *** Rhythmbox controls
                                    , ((modm,               xK_p     ), spawn "rhythmbox-client --play-pause")
                                    -- *** Misc focus
                                    , ((modm .|. shiftMask, xK_u     ), focusUrgent)
                                    , ((modm              , xK_z     ), toggleWS) -- Switch to previews focused workspace
                                    -- *** Modifying the window order
                                    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

                                    -- Commands
                                    , ((modm,               xK_Return), spawn myTerminal)
                                    , ((modm,               xK_x     ), spawn myTerminal)
                                    ]
                                    ++
                                    -- mod-[1..9] @@ Switch to workspace N
                                    -- mod-shift-[1..9] @@ Move client to workspace N
                                    -- mod-control-shift-[1..9] @@ Copy client to workspace N
                                    [((m .|. modm, k), windows $ f i)
                                     | (i, k) <- zip (workspaces c) ([ xK_F1 .. xK_F4 ] ++ [ xK_1 .. xK_4])
                                     , (f, m) <- [(W.view, 0),
                                                  (W.shift, shiftMask),
                                                  (copy, shiftMask .|. controlMask)]]
                                    ++
                                    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
                                    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
                                    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
                                        | (key, sc) <- zip [xK_w, xK_e] [0..]
                                        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myManageHooks = composeAll . concat $
    [ [manageDocks]
    , [ className =? "Firefox" --> doShift "www" ]
    , [ className =? "Google-chrome" --> doShift "www" ]
    , [ className =? "Pidgin" --> doShift "im" ]
    , [ className =? "Rhythmbox" --> doShift "music" ]
    , [ title =? "JDownloader" --> doShift "downloads" ]
    , [ className =? "Gimp" --> doCenterFloat ]
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> doFullFloat ]
    ]

-- *** Main
main = do
       xmonad $ withUrgencyHook NoUrgencyHook
              $ ewmh gnomeConfig   { manageHook         = manageHook gnomeConfig <+> myManageHooks
                                   , layoutHook         = myLayoutHook
                                   , workspaces         = myWorkspaces
                                   , terminal           = myTerminal
                                   , modMask            = myModMask
                                   , borderWidth        = 1
                                   , normalBorderColor  = "#FFFFFF"
                                   , focusedBorderColor = "#FF1600"
                                   , keys = \c -> mykeys c `M.union` keys gnomeConfig c
                                   }
