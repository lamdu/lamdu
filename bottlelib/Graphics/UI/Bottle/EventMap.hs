{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap
  ( EventMap
  , EventType(..)
  , Event
  , module Graphics.UI.GLFW.ModState
  , module Graphics.UI.GLFW.Events
  , lookup
  , charEventMap, allCharsEventMap, simpleCharsEventMap
  , singleton, fromEventType, fromEventTypes
  , delete, filterChars
  , Key(..), charKey, Doc, eventMapDocs
  ) where

import Control.Arrow((***), (&&&))
import Control.Monad(mplus)
import Data.Char(toLower, toUpper)
import Data.List(isPrefixOf)
import Data.Map(Map)
import Data.Monoid(Monoid(..))
import Graphics.UI.GLFW (Key(..))
import Graphics.UI.GLFW.Events (GLFWEvent(..), KeyEvent(..), IsPress(..))
import Graphics.UI.GLFW.ModState (ModState(..), noMods, shift, ctrl, alt)
import Prelude hiding (lookup)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map

data IsShifted = Shifted | NotShifted
  deriving (Eq, Ord, Show, Read)

data EventType = KeyEventType ModState Key
  deriving (Show, Eq, Ord)

charOfKey :: Key -> Maybe Char
charOfKey key =
  case key of
  CharKey c      -> Just c
  KeySpace       -> Just ' '
  KeyPad0        -> Just '0'
  KeyPad1        -> Just '1'
  KeyPad2        -> Just '2'
  KeyPad3        -> Just '3'
  KeyPad4        -> Just '4'
  KeyPad5        -> Just '5'
  KeyPad6        -> Just '6'
  KeyPad7        -> Just '7'
  KeyPad8        -> Just '8'
  KeyPad9        -> Just '9'
  KeyPadDivide   -> Just '/'
  KeyPadMultiply -> Just '*'
  KeyPadSubtract -> Just '-'
  KeyPadAdd      -> Just '+'
  KeyPadDecimal  -> Just '.'
  KeyPadEqual    -> Just '='
  _              -> Nothing

charKey :: Char -> Key
charKey = CharKey . toUpper

type Event = KeyEvent

type Doc = String

data EventHandler a = EventHandler {
  ehDoc :: Doc,
  ehHandler :: ModState -> Key -> a
  } deriving (Functor)

-- CharHandlers always conflict with each other, but they may or may
-- not conflict with shifted/unshifted key events (but not with
-- alt'd/ctrl'd)
data CharHandler a = CharHandler
  { chInputDoc :: String
  , chDoc :: Doc
  , chHandler :: Char -> Maybe (IsShifted -> a)
  } deriving (Functor)
AtFieldTH.make ''CharHandler

data EventMap a = EventMap
  { emMap :: Map EventType (EventHandler a)
  , emCharHandler :: Maybe (CharHandler a)
  } deriving (Functor)
AtFieldTH.make ''EventMap

filterChars
  :: (Char -> Bool) -> EventMap a -> EventMap a
filterChars p =
  atEmCharHandler . fmap . atChHandler $
  \handler c -> if p c then handler c else Nothing

prettyKey :: Key -> String
prettyKey (CharKey x) = [toLower x]
prettyKey k
  | "Key" `isPrefixOf` show k = drop 3 $ show k
  | otherwise = show k

prettyEventType :: EventType -> String
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

isShifted :: ModState -> IsShifted
isShifted ModState { modShift = True } = Shifted
isShifted ModState { modShift = False } = NotShifted

eventTypeOf :: ModState -> Maybe Char -> Key -> EventType
eventTypeOf ms mchar k
  | isCharMods ms && mchar == Just ' ' = KeyEventType ms KeySpace
  | otherwise = KeyEventType ms k

instance Show (EventMap a) where
  show (EventMap m mc) =
    "EventMap (keys = " ++ show (Map.keys m) ++
    maybe "" showCharHandler mc ++ ")"
    where
      showCharHandler (CharHandler iDoc _ _) = ", handleChars " ++ iDoc

eventMapDocs :: EventMap a -> [(String, Doc)]
eventMapDocs (EventMap dict mCharHandler) =
  maybe [] ((:[]) . (chInputDoc &&& chDoc)) mCharHandler ++
  map (prettyEventType *** ehDoc) (Map.toList dict)

filterByKey :: Ord k => (k -> Bool) -> Map k v -> Map k v
filterByKey p = Map.filterWithKey (const . p)

overrides :: EventMap a -> EventMap a -> EventMap a
EventMap xMap xMCharHandler `overrides` EventMap yMap yMCharHandler =
  EventMap
  (xMap `mappend` filteredYMap)
  (xMCharHandler `mplus` yMCharHandler)
  where
    filteredYMap =
      maybe id (filterByKey . checkConflict) xMCharHandler yMap
    checkConflict charHandler (KeyEventType mods key)
      | isCharMods mods =
        maybe True (const False) $
        chHandler charHandler =<< charOfKey key
      | otherwise = True

instance Monoid (EventMap a) where
  mempty = EventMap mempty Nothing
  mappend = overrides

delete :: EventType -> EventMap a -> EventMap a
delete = atEmMap . Map.delete

lookup :: Event -> EventMap a -> Maybe a
lookup (KeyEvent Release _ _ _) _ = Nothing
lookup (KeyEvent Press ms mchar k) (EventMap dict mCharHandler) =
  lookupEvent `mplus` (lookupChar =<< mCharHandler)
  where
    lookupEvent =
      fmap (\eh -> ehHandler eh ms k) $
      eventTypeOf ms mchar k `Map.lookup` dict
    lookupChar (CharHandler _ _ handler)
      | isCharMods ms = fmap ($ isShifted ms) $ handler =<< mchar
      | otherwise = Nothing

-- low-level "smart constructor" in case we need to enforce
-- invariants:
charEventMap
  :: String -> Doc -> (Char -> Maybe (IsShifted -> a)) -> EventMap a
charEventMap = (fmap . fmap . fmap) (EventMap mempty . Just) CharHandler

allCharsEventMap
  :: String -> Doc -> (Char -> IsShifted -> a) -> EventMap a
allCharsEventMap iDoc oDoc f = charEventMap iDoc oDoc $ Just . f

simpleCharsEventMap
  :: String -> Doc -> (Char -> a) -> EventMap a
simpleCharsEventMap iDoc oDoc f =
  allCharsEventMap iDoc oDoc (const . f)

singleton :: EventType -> Doc -> (ModState -> Key -> a) -> EventMap a
singleton eventType doc handler =
  flip EventMap Nothing . Map.singleton eventType $
  EventHandler {
    ehDoc = doc,
    ehHandler = handler
    }

fromEventType :: EventType -> Doc -> a -> EventMap a
fromEventType eventType doc = singleton eventType doc . const . const

fromEventTypes :: [EventType] -> Doc -> a -> EventMap a
fromEventTypes keys doc act =
  mconcat $ map (flip (`fromEventType` doc) act) keys
