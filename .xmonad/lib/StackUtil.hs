module StackUtil
    ( mruView
    ) where

import XMonad.StackSet (StackSet, Workspace)
import qualified XMonad.StackSet as W

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

