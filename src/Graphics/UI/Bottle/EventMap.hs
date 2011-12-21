{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap(
  EventMap, ModState(..), EventType(..), Event(..),
  lookup, noMods, shift, ctrl, alt, singleton, fromEventType,
  Key(..), charKey, delete)
where

import Prelude hiding (lookup)
import Graphics.UI.GLFW (Key(..))
import qualified Data.Map as Map
import Data.Maybe(isJust)
import Data.Map(Map)
import Data.Monoid(Monoid(..))
import Data.Char(toUpper)
import Control.Monad(msum)
import Control.Newtype(pack, unpack, over)
import Control.Newtype.TH(mkNewTypes)

data ModState = ModState {
  modCtrl :: Bool,
  modMeta :: Bool,
  modAlt :: Bool,
  modShift :: Bool
  }
  deriving (Show, Eq, Ord)

charKey :: Char -> Key
charKey = CharKey . toUpper

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
data Event = KeyEvent {
  keyEventModState :: ModState,
  keyEventChar :: Maybe Char,
  keyEventKey :: Key }
  deriving (Show, Eq, Ord)

isCharMods :: ModState -> Bool
isCharMods ModState { modCtrl = False, modAlt = False, modMeta = False } = True
isCharMods _ = False

eventTypesOf :: Event -> [EventType]
eventTypesOf
  (KeyEvent ms mchar k) = KeyEventType ms k : charEventType
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
