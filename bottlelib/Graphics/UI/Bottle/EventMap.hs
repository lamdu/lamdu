{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap
  ( KeyEvent(..), IsPress(..), ModKey(..)
  , prettyModKey
  , ModState(..), noMods, shift, ctrl, alt
  , Key(..), InputDoc, Subtitle, Doc(..)
  , EventMap, lookup, emTickHandlers
  , charEventMap, allChars, simpleChars
  , charGroup, sCharGroup
  , keyEventMap, keyPress, keyPresses
  , deleteKey, deleteKeys
  , IsShifted(..)
  , filterSChars
  , anyShiftedChars
  , charKey, eventMapDocs
  , tickHandler
  , specialCharKey
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***), (&&&))
import Control.Lens.Operators
import Control.Monad (guard, mplus, msum)
import Data.Char (toLower, toUpper)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Maybe (isJust, listToMaybe, maybeToList)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Graphics.UI.GLFW (Key(..))
import Graphics.UI.GLFW.Events (IsPress(..))
import Graphics.UI.GLFW.ModState (ModState(..), noMods, shift, ctrl, alt)
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW.Events as Events

data ModKey = ModKey ModState Key
  deriving (Show, Eq, Ord)

data KeyEvent = KeyEvent Events.IsPress ModKey
  deriving (Show, Eq, Ord)

anyShiftedChars :: String -> [(Char, IsShifted)]
anyShiftedChars s = (,) <$> s <*> [Shifted, NotShifted]

specialCharKey :: Char -> Maybe Key
specialCharKey c =
  case c of
  ' ' -> Just KeySpace
  '0' -> Just KeyPad0
  '1' -> Just KeyPad1
  '2' -> Just KeyPad2
  '3' -> Just KeyPad3
  '4' -> Just KeyPad4
  '5' -> Just KeyPad5
  '6' -> Just KeyPad6
  '7' -> Just KeyPad7
  '8' -> Just KeyPad8
  '9' -> Just KeyPad9
  '/' -> Just KeyPadDivide
  '*' -> Just KeyPadMultiply
  '-' -> Just KeyPadSubtract
  '+' -> Just KeyPadAdd
  '.' -> Just KeyPadDecimal
  '=' -> Just KeyPadEqual
  _ -> Nothing

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

type Subtitle = String

newtype Doc = Doc
  { docStrs :: [Subtitle]
  } deriving (Eq, Ord)

data DocHandler a = DocHandler {
  _dhDoc :: Doc,
  _dhHandler :: a
  } deriving (Functor)

data IsShifted = Shifted | NotShifted
  deriving (Eq, Ord, Show, Read)

type InputDoc = String

-- AllCharsHandler always conflict with each other, but they may or may
-- not conflict with shifted/unshifted key events (but not with
-- alt'd/ctrl'd)
data AllCharsHandler a = AllCharsHandler
  { _chInputDoc :: InputDoc
  , _chDocHandler :: DocHandler (Char -> IsShifted -> Maybe a)
  } deriving (Functor)

data CharGroupHandler a = CharGroupHandler
  { _cgInputDoc :: InputDoc
  , _cgChars :: Set (Char, IsShifted)
  , _cgDocHandler :: DocHandler (Char -> IsShifted -> a)
  } deriving (Functor)

data EventMap a = EventMap
  { _emKeyMap :: Map KeyEvent (DocHandler a)
  , _emCharGroupHandlers :: [CharGroupHandler a]
  , _emCharGroupChars :: Set (Char, IsShifted)
  , _emAllCharsHandler :: Maybe (AllCharsHandler a)
  , _emTickHandlers :: [a]
  } deriving (Functor)

Lens.makeLenses ''DocHandler
Lens.makeLenses ''CharGroupHandler
Lens.makeLenses ''AllCharsHandler
Lens.makeLenses ''EventMap

instance Monoid (EventMap a) where
  mempty = EventMap Map.empty [] Set.empty Nothing []
  mappend = overrides

overrides :: EventMap a -> EventMap a -> EventMap a
overrides
  x@(EventMap xMap xCharGroups xChars xMAllChars xTicks)
  (EventMap yMap yCharGroups yChars yMAllChars yTicks) =
  EventMap
  (xMap `mappend` filteredYMap)
  (xCharGroups `mappend` filteredYCharGroups)
  (xChars `mappend` yChars)
  (xMAllChars `mplus` yMAllChars)
  (xTicks ++ yTicks)
  where
    filteredYMap = filterByKey (not . isKeyConflict) yMap
    isKeyConflict (KeyEvent _ (ModKey mods key))
      | isCharMods mods =
        maybe False (flip (isCharConflict x) (shiftedMods mods)) $ charOfKey key
      | otherwise = False
    filteredYCharGroups =
      filterCharGroups (fmap not . isCharConflict x) yCharGroups

filterCharGroups ::
  (Char -> IsShifted -> Bool) ->
  [CharGroupHandler a] ->
  [CharGroupHandler a]
filterCharGroups f =
  filter (not . Set.null . (^. cgChars)) .
  (Lens.traversed . cgChars %~ Set.filter (uncurry f))

isCharConflict :: EventMap a -> Char -> IsShifted -> Bool
isCharConflict eventMap char isShifted =
  Set.member (char, isShifted) (eventMap ^. emCharGroupChars) ||
  isJust
  (($ isShifted) . ($ char) . (^. chDocHandler . dhHandler) =<<
   eventMap ^. emAllCharsHandler)

filterSChars
  :: (Char -> IsShifted -> Bool) -> EventMap a -> EventMap a
filterSChars p =
  (emCharGroupHandlers %~ filterCharGroups p) .
  (emAllCharsHandler . Lens.traversed . chDocHandler . dhHandler %~ f)
  where
    f handler c isShifted = do
      guard $ p c isShifted
      handler c isShifted

prettyKey :: Key -> InputDoc
prettyKey (CharKey x) = [toLower x]
prettyKey k
  | "Key" `isPrefixOf` show k = drop 3 $ show k
  | otherwise = show k

prettyModKey :: ModKey -> InputDoc
prettyModKey (ModKey ms key) = prettyModState ms ++ prettyKey key

prettyKeyEvent :: KeyEvent -> InputDoc
prettyKeyEvent (KeyEvent Press modKey) = prettyModKey modKey
prettyKeyEvent (KeyEvent Release modKey) =
  "Depress " ++ prettyModKey modKey

prettyModState :: ModState -> InputDoc
prettyModState ms = concat $
  ["Ctrl+" | modCtrl ms] ++
  ["Alt+" | modAlt ms] ++
  ["Shift+" | modShift ms]

isCharMods :: ModState -> Bool
isCharMods ModState { modCtrl = False, modAlt = False } = True
isCharMods _ = False

shiftedMods :: ModState -> IsShifted
shiftedMods ModState { modShift = True } = Shifted
shiftedMods ModState { modShift = False } = NotShifted

mkModKey :: ModState -> Key -> ModKey
mkModKey ms k
  | isCharMods ms && k == CharKey ' ' = ModKey ms KeySpace
  | otherwise = ModKey ms k

eventMapDocs :: EventMap a -> [(InputDoc, Doc)]
eventMapDocs (EventMap dict charGroups _ mAllCharsHandler _) =
  concat
  [ map ((^. chInputDoc) &&& (^. chDocHandler . dhDoc)) $ maybeToList mAllCharsHandler
  , map ((^. cgInputDoc) &&& (^. cgDocHandler . dhDoc)) charGroups
  , map (prettyKeyEvent *** (^. dhDoc)) $ Map.toList dict
  ]

filterByKey :: Ord k => (k -> Bool) -> Map k v -> Map k v
filterByKey p = Map.filterWithKey (const . p)

deleteKey :: KeyEvent -> EventMap a -> EventMap a
deleteKey key = emKeyMap %~ Map.delete key

deleteKeys :: [KeyEvent] -> EventMap a -> EventMap a
deleteKeys = foldr ((.) . deleteKey) id

lookup :: Events.KeyEvent -> EventMap a -> Maybe a
lookup (Events.KeyEvent isPress ms mchar k) (EventMap dict charGroups _ mAllCharHandlers _) =
  msum
  [ (^. dhHandler) <$>
    KeyEvent isPress modKey `Map.lookup` dict
  , listToMaybe $ do
      Press <- return isPress
      char <- maybeToList mchar
      CharGroupHandler _ chars handler <- charGroups
      guard $ Set.member (char, isShifted) chars
      return $ (handler ^. dhHandler) char isShifted
  , do
      Press <- return isPress
      AllCharsHandler _ handler <- mAllCharHandlers
      flip (handler ^. dhHandler) isShifted =<< mchar
  ]
  where
    isShifted = shiftedMods ms
    modKey = mkModKey ms k

sCharGroup :: InputDoc -> Doc -> [(Char, IsShifted)] -> (Char -> IsShifted -> a) -> EventMap a
sCharGroup iDoc oDoc chars handler =
  mempty
  { _emCharGroupHandlers =
      [CharGroupHandler iDoc s (DocHandler oDoc handler)]
  , _emCharGroupChars = s
  }
  where
    s = Set.fromList chars

charGroup :: InputDoc -> Doc -> String -> (Char -> IsShifted -> a) -> EventMap a
charGroup iDoc oDoc = sCharGroup iDoc oDoc . (flip (,) <$> [NotShifted, Shifted] <*>)

-- low-level "smart constructor" in case we need to enforce
-- invariants:
charEventMap
  :: InputDoc -> Doc -> (Char -> IsShifted -> Maybe a) -> EventMap a
charEventMap iDoc oDoc handler =
  mempty
  { _emAllCharsHandler =
    Just $ AllCharsHandler iDoc (DocHandler oDoc handler)
  }

allChars :: InputDoc -> Doc -> (Char -> IsShifted -> a) -> EventMap a
allChars iDoc oDoc f = charEventMap iDoc oDoc $ fmap Just . f

simpleChars :: InputDoc -> Doc -> (Char -> a) -> EventMap a
simpleChars iDoc oDoc f =
  allChars iDoc oDoc (const . f)

keyEventMap :: KeyEvent -> Doc -> a -> EventMap a
keyEventMap eventType doc handler =
  mempty
  { _emKeyMap = Map.singleton eventType $ DocHandler doc handler
  }

keyPress :: ModKey -> Doc -> a -> EventMap a
keyPress =
  keyEventMap . KeyEvent Press

keyPresses :: [ModKey] -> Doc -> a -> EventMap a
keyPresses = mconcat . map keyPress

tickHandler :: a -> EventMap a
tickHandler x = mempty { _emTickHandlers = [x] }
