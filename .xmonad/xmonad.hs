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

main = do
    xmonad myConfig

modm = mod1Mask

myLayout =  setOptions $ tiled ||| JackStack (3/4)
    where setOptions = smartBorders . avoidStruts . toggleLayouts Full
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
                        , startupHook = message "XMonad"
                        }

term :: String
term = terminal myConfig


addAllMyKeys config = config `additionalKeys` keyMaskKeys `additionalKeysP` stringKeys
    where keyMaskKeys = myKeys ++ myWorkspaceKeys ++ (activityKeys =<< activities)
          stringKeys = myMediaKeys

myKeys = [ ((modm .|. shiftMask, xK_l), spawn "~/bin/lock")
         , ((controlMask, xK_Print), spawn "~/bin/screenshot win")
         , ((0, xK_Print), spawn "~/bin/screenshot scr")
         , ((modm, xK_p), shellPrompt myXPConfig)
         , ((modm, xK_b), sendMessage ToggleStruts)
         , ((mod4Mask, xK_w), raiseBrowser)
         , ((modm, xK_f), sendMessage (Toggle "Full"))
         , ((mod4Mask, xK_slash), doActivityPrompt)
         , ((mod4Mask .|. shiftMask, xK_slash), activityDirectoryPrompt)
         ]

doActivityPrompt :: X ()
doActivityPrompt = activityPrompt ?+ doAction

activityDirectoryPrompt :: X ()
activityDirectoryPrompt = activityPrompt ?+ goDirectory

activityPrompt :: X (Maybe Activity)
activityPrompt = do
    actName <- inputPromptWithCompl myXPConfig "Activity" (mkComplFunFromList' actNames)
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
activities = [ Activity "rustTest" xK_t "dev/langs/rust/hello_cargo" rustTutorial
             , Activity "home" xK_h "" (return ())
             , Activity "xmonad" xK_x ".xmonad" editXmonad
             , Activity "todoGraph"xK_t "dev/apps/TodoGraph" todoProject
             ]

-- list makes key shortcuts for an activity
activityKeys :: Activity -> [((KeyMask, KeySym), X ())]
activityKeys Activity{keyCode=key, directory=dir, action=act}
  = [ ((mod4Mask, key), setUserDir dir >> act)
    , ((mod4Mask .|. shiftMask, key), setUserDir dir)
    ]

rustTutorial :: X ()
rustTutorial = do
    spawnTerm
    runInTerm "" "nix-shell -p cargo"

editXmonad :: X ()
editXmonad = runEditor "xmonad.hs"

todoProject :: X ()
todoProject = do
    runEditor "src/Main.hs"
    nixShell "--run ghcid"

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
    where fontCfg = font myXPConfig

