module Widget (Widget) where

import qualified Graphics.DrawingCombinators as Draw
import EventMap (EventMap)
import Sized (Sized)

type Widget k = Sized (Draw.Image (), Maybe (EventMap k))
