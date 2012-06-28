{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap
  ( KeyEvent(..), IsPress(..), ModKey(..)
  , ModState(..), noMods, shift, ctrl, alt
  , Key(..), Doc
  , EventMap, lookup, emTickHandlers
  , charEventMap, allChars, simpleChars
  , keyEventMap, keyPress, keyPresses
  , deleteKey, filterChars
  , charKey, eventMapDocs
  , tickHandler
  ) where

import Control.Arrow((***), (&&&))
import Control.Monad(mplus)
import Data.Char(toLower, toUpper)
import Data.List(isPrefixOf)
import Data.Map(Map)
import Data.Maybe(isNothing)
import Data.Monoid(Monoid(..))
import Graphics.UI.GLFW (Key(..))
import Graphics.UI.GLFW.Events (IsPress(..))
import Graphics.UI.GLFW.ModState (ModState(..), noMods, shift, ctrl, alt)
import Prelude hiding (lookup)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW.Events as Events

data ModKey = ModKey ModState Key
  deriving (Show, Eq, Ord)

data KeyEvent = KeyEvent Events.IsPress ModKey
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

type Doc = String

data DocHandler a = DocHandler {
  dhDoc :: Doc,
  dhHandler :: a
  } deriving (Functor)

data IsShifted = Shifted | NotShifted
  deriving (Eq, Ord, Show, Read)

-- CharHandlers always conflict with each other, but they may or may
-- not conflict with shifted/unshifted key events (but not with
-- alt'd/ctrl'd)
data CharHandler a = CharHandler
  { chInputDoc :: String
  , chDocHandler :: DocHandler (Char -> Maybe (IsShifted -> a))
  } deriving (Functor)

data EventMap a = EventMap
  { emKeyMap :: Map KeyEvent (DocHandler a)
  , emCharHandler :: Maybe (CharHandler a)
  , emTickHandlers :: [a]
  } deriving (Functor)

AtFieldTH.make ''DocHandler
AtFieldTH.make ''CharHandler
AtFieldTH.make ''EventMap

instance Monoid (EventMap a) where
  mempty = EventMap Map.empty Nothing []
  mappend = overrides

overrides :: EventMap a -> EventMap a -> EventMap a
EventMap xMap xMCharHandler xTicks `overrides` EventMap yMap yMCharHandler yTicks =
  EventMap
  (xMap `mappend` filteredYMap)
  (xMCharHandler `mplus` yMCharHandler)
  (xTicks ++ yTicks)
  where
    filteredYMap =
      maybe id (filterByKey . checkConflict) xMCharHandler yMap
    checkConflict charHandler (KeyEvent _ (ModKey mods key))
      | isCharMods mods =
        isNothing $
        dhHandler (chDocHandler charHandler) =<< charOfKey key
      | otherwise = True

filterChars
  :: (Char -> Bool) -> EventMap a -> EventMap a
filterChars p =
  atEmCharHandler . fmap . atChDocHandler . atDhHandler $
  \handler c -> if p c then handler c else Nothing

prettyKey :: Key -> String
prettyKey (CharKey x) = [toLower x]
prettyKey k
  | "Key" `isPrefixOf` show k = drop 3 $ show k
  | otherwise = show k

prettyModKey :: ModKey -> String
prettyModKey (ModKey ms key) = prettyModState ms ++ prettyKey key

prettyKeyEvent :: KeyEvent -> String
prettyKeyEvent (KeyEvent Press modKey) = prettyModKey modKey
prettyKeyEvent (KeyEvent Release modKey) =
  "Depress " ++ prettyModKey modKey

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

mkModKey :: ModState -> Key -> ModKey
mkModKey ms k
  | isCharMods ms && k == CharKey ' ' = ModKey ms KeySpace
  | otherwise = ModKey ms k

eventMapDocs :: EventMap a -> [(String, Doc)]
eventMapDocs (EventMap dict mCharHandler _) =
  maybe [] ((:[]) . (chInputDoc &&& dhDoc . chDocHandler)) mCharHandler ++
  map (prettyKeyEvent *** dhDoc) (Map.toList dict)

filterByKey :: Ord k => (k -> Bool) -> Map k v -> Map k v
filterByKey p = Map.filterWithKey (const . p)

deleteKey :: KeyEvent -> EventMap a -> EventMap a
deleteKey = atEmKeyMap . Map.delete

lookup :: Events.KeyEvent -> EventMap a -> Maybe a
lookup (Events.KeyEvent isPress ms mchar k) (EventMap dict mCharHandler _) =
  lookupEvent `mplus` (lookupChar isPress =<< mCharHandler)
  where
    modKey = mkModKey ms k
    lookupEvent =
      fmap dhHandler $
      KeyEvent isPress modKey `Map.lookup` dict
    lookupChar Press (CharHandler _ handler)
      | isCharMods ms = fmap ($ isShifted ms) $ dhHandler handler =<< mchar
      | otherwise = Nothing
    lookupChar _ _ = Nothing

-- low-level "smart constructor" in case we need to enforce
-- invariants:
charEventMap
  :: String -> Doc -> (Char -> Maybe (IsShifted -> a)) -> EventMap a
charEventMap iDoc oDoc handler =
  mempty
  { emCharHandler =
    Just $ CharHandler iDoc (DocHandler oDoc handler)
  }

allChars :: String -> Doc -> (Char -> IsShifted -> a) -> EventMap a
allChars iDoc oDoc f = charEventMap iDoc oDoc $ Just . f

simpleChars :: String -> Doc -> (Char -> a) -> EventMap a
simpleChars iDoc oDoc f =
  allChars iDoc oDoc (const . f)

keyEventMap :: KeyEvent -> Doc -> a -> EventMap a
keyEventMap eventType doc handler =
  mempty
  { emKeyMap =
    Map.singleton eventType
    DocHandler
    { dhDoc = doc
    , dhHandler = handler
    }
  }

keyPress :: ModKey -> Doc -> a -> EventMap a
keyPress =
  keyEventMap . KeyEvent Press

keyPresses :: [ModKey] -> Doc -> a -> EventMap a
keyPresses = mconcat . map keyPress

tickHandler :: a -> EventMap a
tickHandler x = mempty { emTickHandlers = [x] }
