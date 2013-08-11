{-# LANGUAGE ViewPatterns #-}

import XMonad hiding ((|||))

import System.Exit

import XMonad.Actions.DwmPromote

import XMonad.Hooks.ManageDocks

import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Master
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

import XMonad.Hooks.ManageHelpers

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

myLayout =
      avoidStrutsOn [] $ smartBorders $ minimize $ maximize $ boringWindows $
        mkToggle (FULL ?? EOT) $ mkToggle (single MIRROR)
        (    ResizableTall 1 (3/100) (1/2) []
         ||| mastered (3/100) (1/3) tall
         ||| mastered (3/100) (1/3) (Mirror tall))

tall = Tall 1 (3/100) (1/2)

myTerminal =  "urxvt -fn \"xft:terminus-8\" +sb"

myBorderWidth   = 2
myModMask       = mod4Mask

myNumlockMask   = mod2Mask

myWorkspaces    = map show [1..9]

myNormalBorderColor = "#102233"
myFocusedBorderColor  = "#1892f8"

dmenu = "dmenu_run -fn \"-*-terminus-*-*-*-*-*-*-*-*-*-*-*-*\""
     ++ " -nb \"#000\" -nf \"#ccc\" -sb \"#333\" -sf \"#66e\" -l 6 -b"


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList keylist
  where
    just       = (,) 0
    shift      = (,) shiftMask
    super      = (,) modMask
    shiftSuper = (,) (modMask .|. shiftMask)
    ctrlSuper  = (,) (modMask .|. controlMask)

    keylist =

        -- Spawn programs
      [ (super xK_r, spawn "urxvt -fn \"xft:dejavu sans mono-9\" -rv +sb")
      , (super xK_c, spawn "urxvt -fn \"xft:dejavu sans mono-9\" +sb")
      , (super xK_f, spawn "firefox")
      , (super xK_u, spawn "urxvt -fn \"xft:dejavu sans mono-11:autohint=true\" +sb")
      , (shiftSuper xK_u, spawn "urxvt -fn \"xft:dejavu sans mono-11:autohint=true\" +sb -rv")
      , (ctrlSuper xK_space, spawn "togglepad")

        -- Take screenshot
      , (super xK_Print, spawn "scrot")

        -- Dmenu
      , (super xK_g, spawn dmenu)
      , (super xK_x, spawn "xlock")

        -- Kill window
      , (shiftSuper xK_d, kill)

        -- Rotate through the available layout algorithms
      , (super xK_space, sendMessage NextLayout)

        --  Reset the layouts on the current workspace to default
      , (shiftSuper xK_space, setLayout $ XMonad.layoutHook conf)

        -- Shrink and expand the windows on the non-master area
      , (super xK_v, sequence_ (take 6 $ cycle [sendMessage MirrorShrink,sendMessage ShrinkSlave]))
      , (super xK_w, sequence_ (take 6 $ cycle [sendMessage MirrorExpand,sendMessage ExpandSlave]))

        -- Move focus
      , (super xK_Tab, focusDown)
      , (super xK_n,   focusDown)
      , (super xK_t,   focusUp)

        -- Swap the focused window and the master window, and focus master (dwmpromote)
      , (super xK_Return, dwmpromote)

        -- Swap the windows
      , (shiftSuper xK_n, windows W.swapDown)
      , (shiftSuper xK_t, windows W.swapUp)

        -- Resize the master area
      , (super xK_h, sendMessage Shrink)
      , (super xK_s, sendMessage Expand)

          -- Toggle zoom (full) and mirror, and minimise
      , (super xK_z,      sendMessage $ Toggle FULL)
      , (shiftSuper xK_z, withFocused (sendMessage . maximizeRestore))
      , (ctrlSuper xK_m,  sendMessage $ Toggle MIRROR)
      , (super xK_m,      withFocused minimizeWindow)
      , (shiftSuper xK_m, sendMessage RestoreNextMinimizedWin)

        -- Push window back into tiling
      , (shiftSuper xK_b, withFocused $ windows . W.sink)

        -- [De]Increment the number of windows in the master area
      , (super xK_comma,  sendMessage (IncMasterN 1))
      , (super xK_period, sendMessage (IncMasterN (-1)))

        -- Quit xmonad
      , (shiftSuper xK_q, io (exitWith ExitSuccess))

        -- Restart xmonad
      , (super xK_q,       restart "xmonad" True)

      ]

      ++

      let mute = spawn "amixer sset Master toggle"
          volume s = spawn ("amixer set Master " ++ s) in

      [ -- Media keys : alsamixer
        (just xF86XK_AudioMute,         mute)
      , (just xF86XK_AudioRaiseVolume,  volume "10%+")
      , (just xF86XK_AudioLowerVolume,  volume "10%-")
      , (shift xF86XK_AudioRaiseVolume, volume "2%+")
      , (shift xF86XK_AudioLowerVolume, volume "2%-")

        -- Media keys : mpd
      , (just xF86XK_AudioPlay,        spawn "mpc toggle")
      , (just xF86XK_AudioStop,        spawn "mpc stop")
      , (just xF86XK_AudioPrev,        spawn "mpc prev")
      , (just xF86XK_AudioNext,        spawn "mpc next")

      ]

      ++

     -- mod-[1..9], Switch to workspace N
     -- mod-shift-[1..9], Move client to workspace N
     [ ((m .|. modMask, k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

     ++
     -- mod-{up,dn}, Switch to physical/Xinerama screens 1, 2
     -- mod-shift-{up,dn}, Move client to screen 1, 2
     [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_Up, xK_Down] [0..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse myMouseBindings
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- Shrink and expand slaves (in non master area) with super + scroll wheel
    , ((modMask, button4), const (sequence_ [sendMessage MirrorExpand,sendMessage ExpandSlave]))
    , ((modMask, button5), const (sequence_ [sendMessage MirrorShrink,sendMessage ShrinkSlave]))
    ]


------------------------------------------------------------------------
-- Window rules:
myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , resource  =? "kicker"         --> doIgnore
    ]

------------------------------------------------------------------------
-- Run xmonad
main = xmonad defaults


defaults = defaultConfig {
    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    layoutHook         = myLayout,
    manageHook         = myManageHook
  }
