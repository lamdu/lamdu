{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap
  ( KeyEvent(..), IsPress(..), ModKey(..)
  , ModState(..), noMods, shift, ctrl, alt
  , Key(..), Doc
  , EventMap, lookup
  , charEventMap, allChars, simpleChars
  , keyEventMap, keyPress, keyPresses
  , deleteKey, filterChars
  , charKey, eventMapDocs
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
  { emMap :: Map KeyEvent (DocHandler a)
  , emCharHandler :: Maybe (CharHandler a)
  } deriving (Functor)

AtFieldTH.make ''DocHandler
AtFieldTH.make ''CharHandler
AtFieldTH.make ''EventMap

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

instance Show (EventMap a) where
  show (EventMap m mc) =
    "EventMap (keys = " ++ show (Map.keys m) ++
    maybe "" showCharHandler mc ++ ")"
    where
      showCharHandler (CharHandler iDoc _) = ", handleChars " ++ iDoc

eventMapDocs :: EventMap a -> [(String, Doc)]
eventMapDocs (EventMap dict mCharHandler) =
  maybe [] ((:[]) . (chInputDoc &&& dhDoc . chDocHandler)) mCharHandler ++
  map (prettyKeyEvent *** dhDoc) (Map.toList dict)

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
    checkConflict charHandler (KeyEvent _ (ModKey mods key))
      | isCharMods mods =
        isNothing $
        dhHandler (chDocHandler charHandler) =<< charOfKey key
      | otherwise = True

instance Monoid (EventMap a) where
  mempty = EventMap mempty Nothing
  mappend = overrides

deleteKey :: KeyEvent -> EventMap a -> EventMap a
deleteKey = atEmMap . Map.delete

lookup :: Events.KeyEvent -> EventMap a -> Maybe a
lookup (Events.KeyEvent isPress ms mchar k) (EventMap dict mCharHandler) =
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
  EventMap mempty . Just $ CharHandler iDoc (DocHandler oDoc handler)

allChars :: String -> Doc -> (Char -> IsShifted -> a) -> EventMap a
allChars iDoc oDoc f = charEventMap iDoc oDoc $ Just . f

simpleChars :: String -> Doc -> (Char -> a) -> EventMap a
simpleChars iDoc oDoc f =
  allChars iDoc oDoc (const . f)

keyEventMap :: KeyEvent -> Doc -> a -> EventMap a
keyEventMap eventType doc handler =
  flip EventMap Nothing . Map.singleton eventType $
  DocHandler {
    dhDoc = doc,
    dhHandler = handler
    }

keyPress :: ModKey -> Doc -> a -> EventMap a
keyPress modKey doc =
  keyEventMap (KeyEvent Press modKey) doc

keyPresses :: [ModKey] -> Doc -> a -> EventMap a
keyPresses = mconcat . map keyPress
