import System.Directory (setCurrentDirectory, getHomeDirectory)
import System.IO

import XMonad
import XMonad.StackSet (StackSet, Workspace)
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
    spawn "~/.startup"
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

myKeys = [ ((modm .|. shiftMask, xK_l), safeSpawnProg "slock")
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
activities = [ Activity "home" xK_h "" (return ())
             , Activity "xmonad" xK_x ".xmonad" editXmonad
             , Activity "todoGraph" xK_t "dev/apps/TodoGraph" todoProject
             , Activity "rustTracer" xK_r "dev/graphics/rust-tracer" rustTracerAction
             , Activity "pijul" xK_p "dev/tools/pijul" pijulAction
             , Activity "characterSheet" xK_c "dev/web/character_sheet" characterSheetAction
             ]

pijulAction :: X ()
pijulAction = do
    spawnTerm
    runInTerm "" "nix-shell --command \"cwatch\""

characterSheetAction :: X ()
characterSheetAction = do
    spawnTerm

-- list makes key shortcuts for an activity
activityKeys :: Activity -> [((KeyMask, KeySym), X ())]
activityKeys activity@Activity{keyCode=key}
  = [ ((mod4Mask, key), doAction activity)
    , ((mod4Mask .|. shiftMask, key), goDirectory activity)
    ]

editXmonad :: X ()
editXmonad = runEditor "xmonad.hs"

todoProject :: X ()
todoProject = do
    runEditor "src/Main.hs"
    nixShell "--run ghcid"
    nixShell "--run \"hoogle server --local --port 8080\""

rustTracerAction :: X ()
rustTracerAction = do
    runInTerm "" "cargo watch --clear"
    spawnTerm

myWorkspaceKeys =
    [((m .|. modm, k), windows $ f i)
    | (i,k) <- zip (myWorkspaces) [xK_1..]
    , (f,m) <- [(mruView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
    ]
    ++
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(mruView, controlMask)]]

-- | When using multiple monitors, this will put the workspace for the given tag
-- on the primary screen. The screens will be filled with the most recently
-- viewed workspaces.
-- For example viewing workspaces in the following order: A B C A
-- will put A on screen 1, C on screen 2, B on screen 3
mruView :: (Eq i, Ord a) => i -> StackSet i l a s sd -> StackSet i l a s sd
mruView t st | (Just (w, ws), scs) <- first (extract $ (t ==) . W.tag) . toWSList $ st = fromWSList (w:ws) scs
               | otherwise = st
               where first f (x, y) = (f x, y)

toWSList :: StackSet i l a s sd -> ([Workspace i l a], [(s, sd)])
toWSList st = (workspaces, screenInfo)
    where
        workspaces = W.workspace (W.current st) : (fmap W.workspace (W.visible st) ++ W.hidden st)
        screenInfo = info (W.current st) : fmap info (W.visible st)
        info sc = (W.screen sc, W.screenDetail sc)

fromWSList :: Ord a => [Workspace i l a] -> [(s, sd)] -> StackSet i l a s sd
fromWSList (w1:ws) (sc1:scs) =
    let mkScreen = \w (sId, sDet) -> W.Screen w sId sDet
        cur = mkScreen w1 sc1
        (vis, hidden, _) = zipRemainder mkScreen ws scs
     in W.StackSet cur vis hidden mempty
fromWSList _ _ = error "fromWSList called without at least one workspace and one screen"

zipRemainder :: (a -> b -> c) -> [a] -> [b] -> ([c], [a], [b])
zipRemainder f (x:xs) (y:ys) = case zipRemainder f xs ys of
                             (cs, as, bs) -> (f x y : cs, as, bs)
zipRemainder _ [] ys = ([], [], ys)
zipRemainder _ xs _  = ([], xs, [])

extract :: (a -> Bool) -> [a] -> Maybe (a, [a])
extract f ls = case break f ls of
                 (left, found:right) -> Just (found, left ++ right)
                 _ -> Nothing

myMediaKeys =
    [ ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
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

