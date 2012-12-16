{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Graphics.UI.Bottle.EventMap
  ( KeyEvent(..), IsPress(..), ModKey(..)
  , prettyModKey
  , ModState(..), noMods, shift, ctrl, alt
  , Key(..), InputDoc, Subtitle, Doc(..)
  , EventMap, lookup, emTickHandlers
  , charEventMap, allChars, simpleChars, charGroup
  , keyEventMap, keyPress, keyPresses
  , deleteKey, filterChars
  , charKey, eventMapDocs
  , tickHandler
  ) where

import Control.Arrow ((***), (&&&))
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
import qualified Control.Lens.TH as LensTH
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  , _chDocHandler :: DocHandler (Char -> Maybe (IsShifted -> a))
  } deriving (Functor)

data CharGroupHandler a = CharGroupHandler
  { _cgInputDoc :: InputDoc
  , _cgChars :: Set Char
  , _cgDocHandler :: DocHandler (Char -> IsShifted -> a)
  } deriving (Functor)

data EventMap a = EventMap
  { _emKeyMap :: Map KeyEvent (DocHandler a)
  , _emCharGroupHandlers :: [CharGroupHandler a]
  , _emCharGroupChars :: Set Char
  , _emAllCharsHandler :: Maybe (AllCharsHandler a)
  , _emTickHandlers :: [a]
  } deriving (Functor)

LensTH.makeLenses ''DocHandler
LensTH.makeLenses ''CharGroupHandler
LensTH.makeLenses ''AllCharsHandler
LensTH.makeLenses ''EventMap

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
        maybe False (isCharConflict x) $ charOfKey key
      | otherwise = False
    filteredYCharGroups =
      filterCharGroups (not . isCharConflict x) yCharGroups

filterCharGroups ::
  (Char -> Bool) ->
  [CharGroupHandler a] ->
  [CharGroupHandler a]
filterCharGroups f =
  filter (not . Set.null . Lens.view cgChars) .
  (map . Lens.over cgChars . Set.filter) f

isCharConflict :: EventMap a -> Char -> Bool
isCharConflict eventMap char =
  Set.member char (Lens.view emCharGroupChars eventMap) ||
  isJust
  (($ char) . Lens.view (chDocHandler . dhHandler) =<<
   Lens.view emAllCharsHandler eventMap)

filterChars
  :: (Char -> Bool) -> EventMap a -> EventMap a
filterChars p =
  (Lens.over emCharGroupHandlers . filterCharGroups) p .
  Lens.over (emAllCharsHandler . Lens.mapped . chDocHandler . dhHandler) f
  where
    f handler c = do
      guard $ p c
      handler c

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

isShifted :: ModState -> IsShifted
isShifted ModState { modShift = True } = Shifted
isShifted ModState { modShift = False } = NotShifted

mkModKey :: ModState -> Key -> ModKey
mkModKey ms k
  | isCharMods ms && k == CharKey ' ' = ModKey ms KeySpace
  | otherwise = ModKey ms k

eventMapDocs :: EventMap a -> [(InputDoc, Doc)]
eventMapDocs (EventMap dict charGroups _ mAllCharsHandler _) =
  concat
  [ map (Lens.view chInputDoc &&& Lens.view (chDocHandler . dhDoc)) $ maybeToList mAllCharsHandler
  , map (Lens.view cgInputDoc &&& Lens.view (cgDocHandler . dhDoc)) charGroups
  , map (prettyKeyEvent *** Lens.view dhDoc) $ Map.toList dict
  ]

filterByKey :: Ord k => (k -> Bool) -> Map k v -> Map k v
filterByKey p = Map.filterWithKey (const . p)

deleteKey :: KeyEvent -> EventMap a -> EventMap a
deleteKey = Lens.over emKeyMap . Map.delete

lookup :: Events.KeyEvent -> EventMap a -> Maybe a
lookup (Events.KeyEvent isPress ms mchar k) (EventMap dict charGroups _ mAllCharHandlers _) =
  msum
  [ fmap (Lens.view dhHandler) $
    KeyEvent isPress modKey `Map.lookup` dict
  , listToMaybe $ do
      Press <- return isPress
      char <- maybeToList mchar
      CharGroupHandler _ chars handler <- charGroups
      guard $ Set.member char chars
      return . Lens.view dhHandler handler char $ isShifted ms
  , do
      Press <- return isPress
      AllCharsHandler _ handler <- mAllCharHandlers
      charHandler <- Lens.view dhHandler handler =<< mchar
      return . charHandler $ isShifted ms
  ]
  where
    modKey = mkModKey ms k

charGroup :: InputDoc -> Doc -> String -> (Char -> IsShifted -> a) -> EventMap a
charGroup iDoc oDoc chars handler =
  mempty
  { _emCharGroupHandlers =
      [CharGroupHandler iDoc s (DocHandler oDoc handler)]
  , _emCharGroupChars = s
  }
  where
    s = Set.fromList chars

-- low-level "smart constructor" in case we need to enforce
-- invariants:
charEventMap
  :: InputDoc -> Doc -> (Char -> Maybe (IsShifted -> a)) -> EventMap a
charEventMap iDoc oDoc handler =
  mempty
  { _emAllCharsHandler =
    Just $ AllCharsHandler iDoc (DocHandler oDoc handler)
  }

allChars :: InputDoc -> Doc -> (Char -> IsShifted -> a) -> EventMap a
allChars iDoc oDoc f = charEventMap iDoc oDoc $ Just . f

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
