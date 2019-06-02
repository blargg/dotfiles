module StackUtil
    ( mruView
    ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import XMonad.StackSet (RationalRect, StackSet, Workspace)
import qualified XMonad.StackSet as W

data StackList i l a s sd =
    StackList
        { workspaces :: [Workspace i l a]
        , screenInfo :: [(s, sd)]
        , slFloating :: Map a RationalRect
        }

-- | When using multiple monitors, this will put the workspace for the given tag
-- on the primary screen. The screens will be filled with the most recently
-- viewed workspaces.
-- For example viewing workspaces in the following order: A B C A
-- will put A on screen 1, C on screen 2, B on screen 3
mruView :: Eq i => i -> StackSet i l a s sd -> StackSet i l a s sd
mruView t = fromWSList . mruView' t . toWSList

mruView' :: Eq i => i -> StackList i l a s sd -> StackList i l a s sd
mruView' t st =
    let modSt = do
            (firstWS, remainderWS) <- extract ((t ==) . W.tag) . workspaces $ st
            pure $ st{workspaces = (firstWS:remainderWS)}
    in fromMaybe st modSt

toWSList :: StackSet i l a s sd -> StackList i l a s sd
toWSList st = StackList { workspaces = ws, screenInfo = si, slFloating = fl}
    where
        ws = W.workspace (W.current st) : (fmap W.workspace (W.visible st) ++ W.hidden st)
        si = info (W.current st) : fmap info (W.visible st)
        fl = W.floating st
        info sc = (W.screen sc, W.screenDetail sc)

fromWSList :: StackList i l a s sd -> StackSet i l a s sd
fromWSList StackList{ workspaces=(w1:ws), screenInfo=(sc1:scs), slFloating=fl} =
    let mkScreen = \w (sId, sDet) -> W.Screen w sId sDet
        cur = mkScreen w1 sc1
        (vis, hidden, _) = zipRemainder mkScreen ws scs
     in W.StackSet cur vis hidden fl
fromWSList _ = error "fromWSList called without at least one workspace and one screen"

zipRemainder :: (a -> b -> c) -> [a] -> [b] -> ([c], [a], [b])
zipRemainder f (x:xs) (y:ys) = case zipRemainder f xs ys of
                             (cs, as, bs) -> (f x y : cs, as, bs)
zipRemainder _ [] ys = ([], [], ys)
zipRemainder _ xs _  = ([], xs, [])

extract :: (a -> Bool) -> [a] -> Maybe (a, [a])
extract f ls = case break f ls of
                 (left, found:right) -> Just (found, left ++ right)
                 _ -> Nothing

