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

myLayout =
      -- gaps [(L,400),(R,400)] $
      avoidStrutsOn [] $ smartBorders $ minimize $
        mkToggle (FULL ?? EOT) $ mkToggle (single MIRROR)
        (    ResizableTall 1 (3/100) (1/2) []
        ||| ThreeColMid 1 (3/100) (1/2) True
        ||| mastered (3/100) (1/3) tall
        ||| mastered (3/100) (1/3) (Mirror tall)
        )

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


mplayer :: X () -> X () -> X ()
mplayer m n =
    do b <- liftIO $ doesFileExist "/home/dan/.mplayer-in"
       if b then m else n

mcmd :: String -> X ()
mcmd cmd = (liftIO $ writeFile "/home/dan/.mplayer-in" (cmd ++ "\n"))

mread :: X String
mread = liftIO (fmap (concat . take 1 . reverse . lines) $ readFile "/home/dan/.mplayer-out")

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList keylist
  where
    just       = (,) 0
    shift      = (,) shiftMask
    super      = (,) modMask
    shiftSuper = (,) (modMask .|. shiftMask)
    ctrlSuper  = (,) (modMask .|. controlMask)

    keylist =

        -- Spawn programs
      [ (shiftSuper xK_r, spawn "urxvt -fn \"xft:dejavu sans mono-9:hintstyle=hintnone\" -rv +sb")
      , (shiftSuper xK_c, spawn "urxvt -fn \"xft:dejavu sans mono-9:hintstyle=hintnone\" +sb")
      , (super xK_r, spawn "urxvt -fn \"xft:inconsolata-14\" +sb -rv")
      , (super xK_c, spawn "urxvt -fn \"xft:inconsolata-14\" +sb")
      , (super xK_minus, spawn "urxvt -fn \"xft:inconsolata-24\" +sb")
      , (shiftSuper xK_minus, spawn "urxvt -fn \"xft:inconsolata-24\" +sb -rv")
      , (shiftSuper xK_f, spawn "vimb")
      , (super xK_f, spawn "firefox")
      -- , (super xK_u, spawn "urxvt -fn \"xft:dejavu sans mono-11:autohint=true\" +sb")
      -- , (shiftSuper xK_u, spawn "urxvt -fn \"xft:dejavu sans mono-11:autohint=true\" +sb -rv")
      -- , (ctrlSuper xK_space, spawn "togglepad")

        -- Take screenshot
      , (super xK_Print, spawn "scrot")

        -- Dmenu
      , (super xK_g, spawn dmenu)
      , (super xK_x, spawn "xlock")

        -- Kill window
      , (shiftSuper xK_d, kill)

      , (shift xK_F11, mplayer (mcmd "step_property time_pos -1") (return ()))
      , (shift xK_F12, mplayer (mcmd "step_property time_pos +1") (return ()))
      , (just xK_F10, mplayer (mcmd "pause") (return ()))
      , (just xK_F11, mplayer (spawn "bash /home/dan/code/sub/get_time_pos.sh") (return ()))
      , (just xK_F12, mplayer (spawn "bash /home/dan/code/sub/set_time_pos.sh") (return ()))

        -- Rotate through the available layout algorithms
      , (super xK_space,      sendMessage NextLayout)

        --  Reset the layouts on the current workspace to default
      , (shiftSuper xK_space, setLayout $ XMonad.layoutHook conf)

        -- Shrink and expand the windows on the non-master area
      , (super xK_v, sequence_ (take 6 $ cycle [sendMessage MirrorShrink,sendMessage ShrinkSlave]))
      , (super xK_w, sequence_ (take 6 $ cycle [sendMessage MirrorExpand,sendMessage ExpandSlave]))

        -- Move focus
      , (super xK_Tab, windows W.focusDown)
      , (super xK_n,   windows W.focusDown)
      , (super xK_t,   windows W.focusUp)

        -- Swap the focused window and the master window, and focus master (dwmpromote)
      , (super xK_Return, dwmpromote)

        -- Swap the windows
      , (shiftSuper xK_n, windows W.swapDown)
      , (shiftSuper xK_t, windows W.swapUp)

        -- Resize the master area
      , (super xK_h, sendMessage Shrink)
      , (super xK_s, sendMessage Expand)

        -- Toggle zoom (full) and mirror
      , (super xK_z,      sendMessage $ Toggle FULL)
      , (super xK_m,      sendMessage $ Toggle MIRROR)

        -- Back to tiling
      , (super xK_b,      withFocused $ windows . W.sink)

        -- Boring window
      , (shiftSuper xK_b,      withFocused minimizeWindow)
      --, (shiftSuper xK_b, sendMessage RestoreNextMinimizedWin)

        -- [De]Increment the number of windows in the master area
      , (super xK_comma,  sendMessage (IncMasterN 1))
      , (super xK_period, sendMessage (IncMasterN (-1)))

        -- Quit xmonad
      , (shiftSuper xK_q, io (exitWith ExitSuccess))

        -- Restart xmonad
      , (super xK_q,       restart "xmonad" True)

      , (super xK_l,       waitLock)

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
main = xmonad defaults


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


-- My layout!!

-- | Arguments are nmaster, delta, fraction
data ThreeCol a = ThreeColMid { threeColNMaster :: !Int, threeColDelta :: !Rational, threeColFrac :: !Rational, stable :: Bool }
                | ThreeCol    { threeColNMaster :: !Int, threeColDelta :: !Rational, threeColFrac :: !Rational, stable :: Bool }
    deriving (Show,Read)

instance LayoutClass ThreeCol a where
    pureLayout (ThreeCol n _ f stable) r    = doL stable False n f r
    pureLayout (ThreeColMid n _ f stable) r = doL stable True n f r
    handleMessage l m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = l { threeColFrac = max (-0.5) $ f-d }
                  resize Expand = l { threeColFrac = min 1 $ f+d }
                  incmastern (IncMasterN x) = l { threeColNMaster = max 0 (n+x) }
                  n = threeColNMaster l
                  d = threeColDelta l
                  f = threeColFrac l
    description _ = "ThreeCol"

doL :: Bool -> Bool-> Int-> Rational-> Rectangle-> W.Stack a-> [(a, Rectangle)]
doL stable m n f r = ap zip (tile3 stable m f r n . length) . W.integrate

-- | tile3.  Compute window positions using 3 panes
tile3 :: Bool -> Bool -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile3 stable middle f r nmaster n
    | not stable && (n <= nmaster || nmaster == 0) = splitVertically n r
    | not stable && (n <= nmaster+1) = splitVertically nmaster s1 ++ splitVertically (n-nmaster) s2
    | otherwise = splitVertically nmaster r1 ++ splitVertically nslave1 r2 ++ splitVertically nslave2 r3
        where (r1, r2, r3) = split3HorizontallyBy middle (if f<0 then 1+2*f else f) r
              (s1, s2) = splitHorizontallyBy (if f<0 then 1+f else f) r
              nslave = (n - nmaster)
              nslave1 = ceiling (nslave % 2)
              nslave2 = (n - nmaster - nslave1)

split3HorizontallyBy :: Bool -> Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy middle f (Rectangle sx sy sw sh) =
    if middle
    then ( Rectangle (sx + fromIntegral r3w) sy r1w sh
         , Rectangle (sx + fromIntegral r3w + fromIntegral r1w) sy r2w sh
         , Rectangle sx sy r3w sh )
    else ( Rectangle sx sy r1w sh
         , Rectangle (sx + fromIntegral r1w) sy r2w sh
         , Rectangle (sx + fromIntegral r1w + fromIntegral r2w) sy r3w sh )
        where r1w = ceiling $ fromIntegral sw * f
              r2w = ceiling ( (sw - r1w) % 2 )
              r3w = sw - r1w - r2w

