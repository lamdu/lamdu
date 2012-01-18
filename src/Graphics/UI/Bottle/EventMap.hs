{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap(
  EventMap, EventType(..), Event,
  module Graphics.UI.GLFW.ModState,
  module Graphics.UI.GLFW.Events,
  lookup, singleton, fromEventType,
  Key(..), charKey, delete)
where

import Control.Monad(msum)
import Control.Newtype(pack, unpack, over)
import Control.Newtype.TH(mkNewTypes)
import Data.Char(toUpper)
import Data.Map(Map)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Graphics.UI.GLFW (Key(..))
import Graphics.UI.GLFW.Events (GLFWEvent(..), KeyEvent(..), IsPress(..))
import Graphics.UI.GLFW.ModState (ModState(..), noMods, shift, ctrl, alt)
import Prelude hiding (lookup)
import qualified Data.Map as Map

charKey :: Char -> Key
charKey = CharKey . toUpper

data EventType = CharEventType | KeyEventType ModState Key
  deriving (Show, Eq, Ord)

isCharMods :: ModState -> Bool
isCharMods ModState { modCtrl = False, modAlt = False } = True
isCharMods _ = False

type Event = KeyEvent

eventTypesOf :: Event -> [EventType]
eventTypesOf (KeyEvent Release _ _ _) = []
eventTypesOf (KeyEvent Press ms mchar k) = KeyEventType ms k : charEventType
  where
   charEventType
     | isCharMods ms && isJust mchar = [CharEventType]
     | otherwise = []

newtype EventMap a = EventMap (Map EventType (Event -> a))
  deriving (Functor)
$(mkNewTypes [''EventMap])

instance Show (EventMap a) where
  show (EventMap m) = "EventMap (keys = " ++ show (Map.keys m) ++ ")"

filterByKey :: Ord k => (k -> Bool) -> Map k v -> Map k v
filterByKey p = Map.filterWithKey (const . p)

overrides :: EventMap a -> EventMap a -> EventMap a
EventMap x `overrides` EventMap y =
  EventMap $ x `mappend` filterByKey (not . inX) y
  where
    inX = any (`Map.member` x) . alternates
    alternates k@(KeyEventType ms (CharKey _))
      | isCharMods ms = [k, CharEventType]
      | otherwise = [k]
    -- TODO: Bug -- we don't have a good way of doing the reverse
    -- filtering (CharEventType alternates are ALL the KeyEventTypes)
    alternates k = [k]

instance Monoid (EventMap a) where
  mempty = EventMap mempty
  mappend = overrides

delete :: EventType -> EventMap a -> EventMap a
delete = over EventMap . Map.delete

lookup :: Event -> EventMap a -> Maybe a
lookup event eventMap =
  fmap ($ event) . msum $
  map (`Map.lookup` unpack eventMap) (eventTypesOf event)

singleton :: EventType -> (Event -> a) -> EventMap a
singleton eventType handler = pack $ Map.singleton eventType handler

fromEventType :: EventType -> a -> EventMap a
fromEventType eventType = singleton eventType . const
