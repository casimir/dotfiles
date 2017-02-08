{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -i/home/dan/.xmonad/ -v #-}

import XMonad hiding ((|||))

import System.Exit

import XMonad.Hooks.DynamicLog

import XMonad.Util.Cursor

import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import BinarySpacePartition
import XMonad.Layout.BorderResize

import XMonad.Hooks.ServerMode

import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowBringer

import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.ManageHelpers

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified XMonad.Core     as W
import qualified Data.Map        as M

import System.IO.Unsafe
import Data.IORef
import Data.Maybe

import Data.Time.Clock

myLayout = smartBorders $ mkToggle1 NBFULL (borderResize emptyBSP)

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

base03 = "#1c1c1c"
base02 = "#303030"
-- base03 = "#002b36"
-- base02 = "#073642"
base01 = "#586e75"
base00 = "#657b83"
base0  = "#839496"
base1  = "#93a1a1"
base2  = "#eee8d5"
base3  = "#fdf6e3"

myNormalBorderColor  = base02 -- "#073642"
myFocusedBorderColor = cyan -- green

dmenu :: String
dmenu = "dmenu_run -fn \"Iosevka Slab-22:weight=50\" -l 6 -b"
     ++ concat [ " -" ++ opt ++ " " ++ show color
               | (opt,color) <-
                     [ ("nb", base02)
                     , ("nf", base1)
                     , ("sb", cyan)
                     , ("sf", base03)
                     ]]

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
        , ((modMask,               xK_p     ), sendMessage MoveNode)
        , ((modMask,               xK_d     ), sendMessage SelectNode)
        , ((modMask,               xK_space ), sendMessage Balance)
        , ((modMask .|. shiftMask, xK_space ), sendMessage Equalize)
        ]

    keylist =

        -- Spawn programs
      [ (super      xK_c,     spawn "urxvt")
      , (shiftSuper xK_c,     spawn "urxvt -rv")
      , (super      xK_minus, spawn "urxvt")
      , (shiftSuper xK_minus, spawn "urxvt -rv")
      , (shiftSuper xK_f,     spawn "vimb")
      , (super      xK_f,     spawn "qutebrowser")

      , ((altMask, xK_F1),    sendMessage (setAxisCrumbMessage Horizontal) >> sendMessage Swap)
      , ((altMask, xK_F2),    sendMessage (setAxisCrumbMessage Vertical) >> sendMessage Swap)

      -- , (super xK_d, do withFocused minimizeWindow
      --                   windows W.focusDown)
      -- , (super xK_p, sendMessage RestoreNextMinimizedWin)

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
      , (super xK_0,      sendMessage (Toggle NBFULL))

        -- Back to tiling
      , (super xK_b,      withFocused $ windows . W.sink)

      --, (super xK_i, bringMenu)
      --, (super xK_u, gotoMenu)

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
     [ ((m .|. modMask, k),
           do cond (windows $ f i)
              setDefaultCursor cursor)
     | (i, k, cursor) <- zip3 (XMonad.workspaces conf) [xK_1 .. xK_9] cursors
     , (f, m, cond) <- [(W.view, 0, condWait i), (W.shift, shiftMask, id)]]

     ++

     -- mod-o,       Switch to other screen
     -- mod-shift-o, Move client to other screen
     [ ((m .|. modMask, xK_o),
           windows $ \ ss -> case W.visible ss of
                               other:_ -> f (W.tag (W.workspace other)) ss
                               []      -> ss)
     | (m, f) <- [(0,         W.view)
                 ,(shiftMask, W.shift)]
     ]

     ++

     -- keep focus on this screen, but switch workspaces between it and the other
     [ (super xK_e, windows $ \ ss ->
         let rot (x:y:zs) = y:x:zs
             rot zs       = zs
             swap_ws x y  = x { W.workspace = W.workspace y }
             me:other     = rot (W.current ss : W.visible ss)
         in  ss { W.current = swap_ws (W.current ss) me
                , W.visible = zipWith swap_ws (W.visible ss) other
                })
     ]



cursors = cycle [ xC_dot, xC_heart, xC_plus ]

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
main =
  do xmconf <- statusBar "xmobar" xmobarConf toggleStrutsKey navconf
     xmonad xmconf
         -- (xmconf { layoutHook = mkToggle1 NBFULL (layoutHook xmconf) } )

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

xmobarConf =
  def
    { ppCurrent = xmobarColor yellow "" . wrap "[" "]"
    , ppVisible = wrap "(" ")"
    , ppHidden  = wrap " " " "
    , ppHiddenNoWindows  = wrap " " " " . const "-"
    , ppUrgent  = xmobarColor orange ""
    , ppWsSep   = ""
    , ppSep     = " "
    , ppOrder   = \ (ws:l:_:_) -> map (map nopar) $ case l of
                      "Full" -> [map oxford ws]
                      _      -> [ws]
    }
-- "<icon=/home/dan/blarg.xpm/>"n

oxford :: Char -> Char
oxford '[' = '⟦' -- urk, no oxford brackets in Iosevka :(
oxford ']' = '⟧'
oxford c   = c

nopar :: Char -> Char
nopar '(' = '['
nopar ')' = ']'
nopar c   = c

-- want to have a Navigation2D that moves inside the
-- BSP layout, namely that it takes the window
-- in the direction, splits it, and puts the current window
-- in the other split.
--
-- this makes a lot of sense with multiple workspaces

navconf
  = navigation2DP def
      ("n","h","t","s")
      [("M-", windowGo),
       ("M-S-", windowSwap),
       ("M-C-", \x b -> do sendMessage SelectNode
                           windowGo x b
                           sendMessage MoveNode
       )
       --("M-C-", windowToScreen)
       --("M-C-", \x b -> windowSwap x b >> windowGo (opp x) b)
       ]
      True
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
    startupHook        = do startupHook def
                            refresh
                            setDefaultCursor xC_dot,
    handleEventHook    = handleEventHook def <+> fullscreenEventHook <+>
                         serverModeEventHookCmd
  }


