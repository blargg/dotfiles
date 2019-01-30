{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module JackStack (
    JackStack(JackStack)) where

import XMonad
import qualified XMonad.StackSet as W

data JackStack a = JackStack !Rational deriving ( Read, Show )

instance LayoutClass JackStack Window where
    pureLayout (JackStack r) (Rectangle sx sy sw sh) ws =
        if total == 0
           then [(W.focus ws, Rectangle sx sy sw sh)]
           else (focus:(reverse tops)) ++ bottoms
     where
        ups = W.up ws
        dns = W.down ws
        total = (length ups) + (length dns)
        delta = round $ (fromIntegral sw) * ((1 - r) / (fromIntegral total)) :: Int
        getPos i = Rectangle
            { rect_x = sx + (fromIntegral (delta * (fromIntegral i)))
            , rect_y = sy
            , rect_height = sh
            , rect_width = round $ r * (fromIntegral sw)
            }
        focus = ((W.focus ws), getPos (length ups))
        tops = zip (reverse ups) (map getPos [0..])
        bottoms = zip dns (map getPos[((length ups) + 1) ..])

    description _ = "JackStack"
