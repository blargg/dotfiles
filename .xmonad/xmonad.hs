import System.Directory (setCurrentDirectory, getHomeDirectory)
import System.IO

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
import XMonad.Util.Run
import XMonad.Util.Dzen ((>=>), dzenConfig, timeout, onCurr, xScreen)
import qualified XMonad.Util.Dzen as DZ

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Input

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts

import Data.Foldable
import System.IO.Error (tryIOError, isDoesNotExistError)

import JackStack
import qualified StackUtil as SU

main :: IO ()
main = xmonad xconfig

modm = modMask xconfig

layoutConfig =  setOptions $ tiled ||| JackStack (9/10)
    where setOptions = smartBorders . avoidStruts . toggleLayouts Full
          tiled = Tall nmaster delta ratio
          nmaster = 1
          ratio = 1/2
          delta = 3/100

xconfig = ewmh . addAllKeys $ xconfig'
    where xconfig' = def
                        { terminal = "urxvt"
                        , workspaces = map show [1..9::Int]
                        , manageHook = manageDocks
                        , handleEventHook = handleEventHook def <+> fullscreenEventHook
                        , layoutHook = layoutConfig
                        , modMask = mod1Mask
                        , focusFollowsMouse = False
                        , startupHook = message "XMonad"
                        }

term :: String
term = terminal xconfig


addAllKeys config = config
    `additionalKeys` hotKeys
    `additionalKeys` workspaceKeys
    `additionalKeys` (activityKeys =<< activities)
    `additionalKeysP` mediaKeys

hotKeys = [ ((modm .|. shiftMask, xK_l), safeSpawnProg "slock")
         , ((controlMask, xK_Print), spawn "~/bin/screenshot win")
         , ((0, xK_Print), spawn "~/bin/screenshot scr")
         , ((modm, xK_p), shellPrompt promptConfig)
         , ((modm, xK_b), sendMessage ToggleStruts)
         , ((mod4Mask, xK_w), raiseBrowser)
         , ((modm, xK_f), sendMessage (Toggle "Full"))
         , ((mod4Mask, xK_slash), doActivityPrompt)
         , ((mod4Mask .|. shiftMask, xK_slash), activityDirectoryPrompt)
         ]

workspaceKeys =
    [((m .|. modm, k), windows $ f i)
    | (i,k) <- zip (workspaces xconfig) [xK_1..]
    , (f,m) <- [(SU.mruView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
    ]
    ++
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(SU.mruView, controlMask)]]

-- list makes key shortcuts for an activity
activityKeys :: Activity -> [((KeyMask, KeySym), X ())]
activityKeys activity@Activity{keyCode=key}
  = [ ((mod4Mask, key), doAction activity)
    , ((mod4Mask .|. shiftMask, key), goDirectory activity)
    ]

mediaKeys =
    [ ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -2")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +2")
    , ("S-<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
    , ("S-<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
    ]

doActivityPrompt :: X ()
doActivityPrompt = activityPrompt ?+ doAction

activityDirectoryPrompt :: X ()
activityDirectoryPrompt = activityPrompt ?+ goDirectory

activityPrompt :: X (Maybe Activity)
activityPrompt = do
    actName <- inputPromptWithCompl promptConfig "Activity" (mkComplFunFromList' actNames)
    return $ actName >>= activityByName
    where actNames = name <$> activities

activityByName :: String -> Maybe Activity
activityByName actName = find ((== actName) . name) activities

doAction :: Activity -> X ()
doAction activity = goDirectory activity >> action activity

goDirectory :: Activity -> X ()
goDirectory = setUserDir . directory

data Activity = Activity
    { name :: String
    , keyCode :: KeySym
    , directory :: String
    , action :: X ()
    }

activities :: [Activity]
activities = [ Activity "home" xK_h "" (return ())
             , Activity "xmonad" xK_x ".xmonad" editXmonad
             , Activity "characterSheet" xK_c "dev/web/character_sheet" characterSheetAction
             , Activity "game" xK_g "dev/game/interstitial-flaw" spawnTerm
             ]

characterSheetAction :: X ()
characterSheetAction = do
    spawnTerm

editXmonad :: X ()
editXmonad = runEditor "xmonad.hs"

promptConfig :: XPConfig
promptConfig = def
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

-- Sets the current directory relative to the user home directory
setUserDir :: String -> X ()
setUserDir dir = do
    result <- io $ do
        homeDir <- getHomeDirectory
        tryIOError $ setCurrentDirectory (homeDir ++ "/" ++ dir)
    case result of
      Left err -> message $ errMsg err dir
      Right _ -> return ()
    where errMsg err dir = if isDoesNotExistError err
                              then dir ++ " does not exist."
                              else "Error moving to " ++ dir

spawnTerm :: X ()
spawnTerm = safeSpawnProg term

runEditor :: String -> X ()
runEditor filename = runInTerm "" (editor ++ " " ++ filename)
    where editor = "nvim"

nixShell :: String -> X ()
nixShell command = runInTerm "" ("nix-shell " ++ command)

message :: String -> X ()
message = dzenConfig (timeout 3 >=> onCurr xScreen >=> DZ.font fontCfg)
    where fontCfg = font promptConfig

