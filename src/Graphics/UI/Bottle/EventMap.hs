{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap(
  EventMap, EventType(..), Event,
  module Graphics.UI.GLFW.ModState,
  module Graphics.UI.GLFW.Events,
  lookup, singleton, fromEventType, fromEventTypes,
  Key(..), charKey, delete, Doc, eventMapDocs)
where

import Control.Monad(msum)
import Control.Newtype(pack, op, over)
import Control.Newtype.TH(mkNewTypes)
import Data.Char(toLower, toUpper)
import Data.List(isPrefixOf)
import Data.Map(Map)
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Graphics.UI.GLFW (Key(..))
import Graphics.UI.GLFW.Events (GLFWEvent(..), KeyEvent(..), IsPress(..))
import Graphics.UI.GLFW.ModState (ModState(..), noMods, shift, ctrl, alt)
import Prelude hiding (lookup)
--import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map

charKey :: Char -> Key
charKey = CharKey . toUpper

data EventType = CharEventType | SpaceKeyEventType ModState | KeyEventType ModState Key
  deriving (Show, Eq, Ord)

prettyKey :: Key -> String
prettyKey (CharKey x) = [toLower x]
prettyKey k
  | "Key" `isPrefixOf` show k = drop 3 $ show k
  | otherwise = show k

prettyEventType :: EventType -> String
prettyEventType CharEventType = "Character"
prettyEventType (SpaceKeyEventType ms) = prettyModState ms ++ "Space"
prettyEventType (KeyEventType ms key) =
  prettyModState ms ++ prettyKey key

prettyModState :: ModState -> String
prettyModState ms = concat $
  ["Ctrl+" | modCtrl ms] ++
  ["Alt+" | modAlt ms] ++
  ["Shift+" | modShift ms]

isCharMods :: ModState -> Bool
isCharMods ModState { modCtrl = False, modAlt = False } = True
isCharMods _ = False

type Event = KeyEvent

eventTypesOf :: Event -> [EventType]
eventTypesOf (KeyEvent Release _ _ _) = []
eventTypesOf (KeyEvent Press ms mchar k)
  | isCharMods ms && mchar == Just ' ' = [SpaceKeyEventType ms]
  | otherwise = KeyEventType ms k : charEventType
  where
   charEventType
     | isCharMods ms && isJust mchar = [CharEventType]
     | otherwise = []

type Doc = String

data EventHandler a = EventHandler {
  ehDoc :: Doc,
  ehHandler :: Event -> a
  }
  deriving (Functor)
-- AtFieldTH.make ''EventHandler

newtype EventMap a = EventMap (Map EventType (EventHandler a))
  deriving (Functor)
mkNewTypes [''EventMap]

instance Show (EventMap a) where
  show (EventMap m) = "EventMap (keys = " ++ show (Map.keys m) ++ ")"

eventMapDocs :: EventMap a -> [(String, Doc)]
eventMapDocs =
  map f . Map.toList . op EventMap
  where
    f (eventType, eventHandler) =
      (prettyEventType eventType, ehDoc eventHandler)

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
  fmap (($ event) . ehHandler) . msum $
  map (`Map.lookup` op EventMap eventMap) (eventTypesOf event)

singleton :: EventType -> Doc -> (Event -> a) -> EventMap a
singleton eventType doc handler =
  pack . Map.singleton eventType $
  EventHandler {
    ehDoc = doc,
    ehHandler = handler
    }

fromEventType :: EventType -> Doc -> a -> EventMap a
fromEventType eventType doc = singleton eventType doc . const

fromEventTypes :: [EventType] -> Doc -> a -> EventMap a
fromEventTypes keys doc act =
  mconcat $ map (flip (`fromEventType` doc) act) keys
