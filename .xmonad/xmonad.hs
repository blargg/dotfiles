import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Actions.GridSelect
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowGo
import System.IO

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.ToggleLayouts

import JackStack

main = do
    xmonad myConfig

modm = mod1Mask

myLayout =  setOptions $ tiled ||| JackStack (3/4)
    where setOptions = workspaceDir "~" . smartBorders . avoidStruts . toggleLayouts Full
          tiled = Tall nmaster delta ratio
          nmaster = 1
          ratio = 1/2
          delta = 3/100

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9::Int]

myConfig = ewmh . addAllMyKeys $ myConfig'
    where myConfig' = def
                        { terminal = "urxvt"
                        , workspaces = myWorkspaces
                        , manageHook = manageDocks
                        , handleEventHook = handleEventHook def <+> fullscreenEventHook
                        , layoutHook = myLayout
                        , modMask = modm
                        , focusFollowsMouse = False
                        }

addAllMyKeys config = config `additionalKeys` keyMaskKeys `additionalKeysP` stringKeys
    where keyMaskKeys = myKeys ++ myWorkspaceKeys
          stringKeys = myMediaKeys

myKeys = [ ((modm .|. shiftMask, xK_l), spawn "~/bin/lock")
         , ((modm, xK_d), changeDir myXPConfig)
         , ((controlMask, xK_Print), spawn "~/bin/screenshot win")
         , ((0, xK_Print), spawn "~/bin/screenshot scr")
         , ((modm, xK_p), shellPrompt myXPConfig)
         , ((modm, xK_b), sendMessage ToggleStruts)
         , ((mod4Mask, xK_w), raiseBrowser)
         , ((modm, xK_f), sendMessage (Toggle "Full"))
         ]

myWorkspaceKeys =
    [((m .|. modm, k), windows $ f i)
    | (i,k) <- zip (myWorkspaces) [xK_1..]
    , (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
    ]
    ++
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.greedyView, controlMask)]]

myMediaKeys =
    [ ("<XF86AudioLowerVolume>", spawn "~/bin/volume down")
    , ("<XF86AudioRaiseVolume>", spawn "~/bin/volume up")
    , ("<XF86AudioMute>", spawn "~/bin/volume mute")
    ]

myXPConfig = def
                { font = "-*-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
                , bgColor = "black"
                , fgColor = "white"
                , bgHLight = "white"
                , fgHLight = "black"
                , borderColor = "white"
                , promptBorderWidth = 0
                , position = Top
                , height = 14
                }
