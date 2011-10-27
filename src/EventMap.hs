{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module EventMap where

import Graphics.UI.GLFW
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Monoid
import Control.Newtype
import Control.Newtype.TH

data ModState = ModState {
  modCtrl :: Bool,
  modMeta :: Bool,
  modAlt :: Bool,
  modShift :: Bool
  }
  deriving (Show, Eq, Ord)

noMods :: ModState
noMods = ModState False False False False

shift :: ModState
shift = noMods { modShift = True }

ctrl :: ModState
ctrl = noMods { modCtrl = True }

alt :: ModState
alt = noMods { modAlt = True }

instance Ord Key where
    compare a b = compare (show a) (show b)

-- TODO: Modifiers
data EventType = CharEventType | KeyEventType ModState Key
  deriving (Show, Eq, Ord)
data Event = CharEvent { fromCharEvent :: Char }
           | KeyEvent ModState Key
  deriving (Show, Eq, Ord)

eventTypeOf :: Event -> EventType
eventTypeOf (CharEvent _) = CharEventType
eventTypeOf (KeyEvent ms k) = KeyEventType ms k

newtype EventMap a = EventMap (Map EventType (Event -> a))
  deriving (Monoid)
$(mkNewTypes [''EventMap])

lookup :: Event -> EventMap a -> Maybe a
lookup event =
  fmap ($ event) .
  Map.lookup (eventTypeOf event) .
  unpack

singleton :: EventType -> (Event -> a) -> EventMap a
singleton eventType handler = pack $ Map.singleton eventType handler
