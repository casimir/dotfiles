{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad hiding ((|||))

import System.Exit

import XMonad.Actions.DwmPromote

import XMonad.Hooks.ManageDocks

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Master
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Gaps
-- import XMonad.Layout.ThreeColumns
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Actions.Navigation2D
import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.ManageHelpers

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Ratio
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

import System.Process
import System.IO.Unsafe
import Data.IORef

import Data.Time.Clock

import Control.Monad

import System.Directory

myLayout = smartBorders $ mkToggle (FULL ?? EOT) $ borderResize emptyBSP

tall = Tall 1 (3/100) (1/2)

myTerminal =  "urxvt -fn \"xft:terminus-8\" +sb"

myBorderWidth   = 2
myModMask       = mod4Mask

myNumlockMask   = mod2Mask

myWorkspaces    = map show [1..9]

yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

myNormalBorderColor = "#073642"
myFocusedBorderColor = green

dmenu = "dmenu_run -fn iosevka-18:weight=50"
     ++ " -nb \"#000\" -nf \"#ccc\" -sb \"#333\" -sf \"#66e\" -l 6 -b"


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList (moreKeys ++ keylist)
  where
    just       = (,) 0
    shift      = (,) shiftMask
    super      = (,) modMask
    shiftSuper = (,) (modMask .|. shiftMask)
    ctrlSuper  = (,) (modMask .|. controlMask)
    ctrlMask   = controlMask
    altMask    = mod1Mask

    -- BSP
    moreKeys =
        [ ((modMask,               xK_m     ), sendMessage $ ExpandTowards L)
        , ((modMask,               xK_w     ), sendMessage $ ExpandTowards D)
        , ((modMask,               xK_v     ), sendMessage $ ExpandTowards U)
        , ((modMask,               xK_z     ), sendMessage $ ExpandTowards R)
        , ((modMask .|. shiftMask, xK_m     ), sendMessage $ ShrinkFrom L)
        , ((modMask .|. shiftMask, xK_w     ), sendMessage $ ShrinkFrom D)
        , ((modMask .|. shiftMask, xK_v     ), sendMessage $ ShrinkFrom U)
        , ((modMask .|. shiftMask, xK_z     ), sendMessage $ ShrinkFrom R)
        , ((modMask,               xK_r     ), sendMessage Rotate)
        , ((modMask,               xK_slash ), sendMessage RotateR)
        , ((modMask .|. shiftMask, xK_slash ), sendMessage RotateL)
        , ((modMask .|. shiftMask, xK_r     ), sendMessage Swap)
        , ((modMask,               xK_u     ), sendMessage FocusParent)
        , ((modMask,               xK_l     ), sendMessage MoveNode)
        , ((modMask .|. shiftMask, xK_l     ), sendMessage SelectNode)
        , ((modMask,               xK_space ), sendMessage Balance)
        , ((modMask .|. shiftMask, xK_space ), sendMessage Equalize)
        ]

    keylist =

        -- Spawn programs
      [ (super xK_c, spawn "urxvt")
      , (shiftSuper xK_c, spawn "urxvt -rv")
      , (super xK_minus, spawn "urxvt")
      , (shiftSuper xK_minus, spawn "urxvt -rv")
      , (shiftSuper xK_f, spawn "vimb")
      , (super xK_f, spawn "firefox")

        -- Take screenshot
      , (super xK_Print, spawn "scrot")

        -- Dmenu
      , (super xK_g, spawn dmenu)

        -- Lock
      , (super xK_x, spawn "xlock")

        -- Kill window
      , (shiftSuper xK_d, kill)

        --Move focus
      , (super xK_Tab, windows W.focusDown)

        -- Toggle fullscreen
      , (super xK_0,      sendMessage (Toggle FULL))

        -- Back to tiling
      , (super xK_b,      withFocused $ windows . W.sink)

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
     [ ((m .|. modMask, k), cond (windows $ f i))
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m, cond) <- [(W.greedyView, 0, condWait i), (W.shift, shiftMask, id)]]

     ++
     -- mod-{up,dn}, Switch to physical/Xinerama screens 1, 2
     -- mod-shift-{up,dn}, Move client to screen 1, 2
     [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_Up, xK_Down] [0..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

condWait :: String -> X () -> X ()
condWait "9" m = waitNine (m >> waitLock)
condWait _ m = m

{-# NOINLINE wait_ref #-}
wait_ref :: IORef (Maybe UTCTime)
wait_ref = unsafePerformIO (newIORef Nothing)

waitLock :: X ()
waitLock = liftIO $ writeIORef wait_ref . Just =<< getCurrentTime

waitNine :: X () -> X ()
waitNine m =
  do (b,diff) <- liftIO $ do
        m <- readIORef wait_ref
        now <- getCurrentTime
        case m of
            Nothing     -> return (True,"")
            Just before ->
                do let diff = diffUTCTime now before
                   return (diff > 600,show diff)
     if b then m else osd_cat diff

osd_cat :: String -> X ()
osd_cat s =
    do liftIO (writeFile "/tmp/osd" s)
       spawn "osd_cat /tmp/osd --age=1 --pos=bottom --lines=1"

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
main = xmonad
  $ navigation2DP def
      ("n","h","t","s")
      [("M-", windowGo),
       ("M-S-", windowSwap),
       ("M-C-", \x b -> windowSwap x b >> windowGo (opp x) b)]
      False
  $ defaults
  where
  opp U = D
  opp D = U
  opp R = L
  opp L = R


defaults = ewmh $ def {
    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = handleEventHook def <+> fullscreenEventHook
  }


