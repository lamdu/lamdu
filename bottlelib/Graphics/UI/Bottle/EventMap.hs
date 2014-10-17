{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveGeneric, StandaloneDeriving, RecordWildCards #-}
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
  , eventMapDocs
  , tickHandler
  , specialCharKey
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***), (&&&))
import Control.Lens (Lens, Lens')
import Control.Lens.Operators
import Control.Monad (guard, mplus, msum)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Maybe (isJust, listToMaybe, maybeToList)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import GHC.Generics (Generic)
import Graphics.UI.GLFW (Key(..))
import Graphics.UI.GLFW.Events (IsPress(..))
import Graphics.UI.GLFW.ModState (ModState(..), noMods, shift, ctrl, alt)
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW.Events as Events

deriving instance Generic Key

data ModKey = ModKey ModState Key
  deriving (Generic, Show, Eq, Ord)

data KeyEvent = KeyEvent Events.IsPress ModKey
  deriving (Generic, Show, Eq, Ord)

anyShiftedChars :: String -> [(Char, IsShifted)]
anyShiftedChars s = (,) <$> s <*> [Shifted, NotShifted]

specialCharKey :: Char -> Maybe Key
specialCharKey c =
  case c of
  ' ' -> Just Key'Space
  '0' -> Just Key'Pad0
  '1' -> Just Key'Pad1
  '2' -> Just Key'Pad2
  '3' -> Just Key'Pad3
  '4' -> Just Key'Pad4
  '5' -> Just Key'Pad5
  '6' -> Just Key'Pad6
  '7' -> Just Key'Pad7
  '8' -> Just Key'Pad8
  '9' -> Just Key'Pad9
  '/' -> Just Key'PadDivide
  '*' -> Just Key'PadMultiply
  '-' -> Just Key'PadSubtract
  '+' -> Just Key'PadAdd
  '.' -> Just Key'PadDecimal
  '=' -> Just Key'PadEqual
  _ -> Nothing

charOfKey :: Key -> Maybe Char
charOfKey key =
  case key of
  Key'A           -> Just 'A'
  Key'B           -> Just 'B'
  Key'C           -> Just 'C'
  Key'D           -> Just 'D'
  Key'E           -> Just 'E'
  Key'F           -> Just 'F'
  Key'G           -> Just 'G'
  Key'H           -> Just 'H'
  Key'I           -> Just 'I'
  Key'J           -> Just 'J'
  Key'K           -> Just 'K'
  Key'L           -> Just 'L'
  Key'M           -> Just 'M'
  Key'N           -> Just 'N'
  Key'O           -> Just 'O'
  Key'P           -> Just 'P'
  Key'Q           -> Just 'Q'
  Key'R           -> Just 'R'
  Key'S           -> Just 'S'
  Key'T           -> Just 'T'
  Key'U           -> Just 'U'
  Key'V           -> Just 'V'
  Key'W           -> Just 'W'
  Key'X           -> Just 'X'
  Key'Y           -> Just 'Y'
  Key'Z           -> Just 'Z'
  Key'Space       -> Just ' '
  Key'Pad0        -> Just '0'
  Key'Pad1        -> Just '1'
  Key'Pad2        -> Just '2'
  Key'Pad3        -> Just '3'
  Key'Pad4        -> Just '4'
  Key'Pad5        -> Just '5'
  Key'Pad6        -> Just '6'
  Key'Pad7        -> Just '7'
  Key'Pad8        -> Just '8'
  Key'Pad9        -> Just '9'
  Key'PadDivide   -> Just '/'
  Key'PadMultiply -> Just '*'
  Key'PadSubtract -> Just '-'
  Key'PadAdd      -> Just '+'
  Key'PadDecimal  -> Just '.'
  Key'PadEqual    -> Just '='
  _              -> Nothing

type Subtitle = String

newtype Doc = Doc
  { docStrs :: [Subtitle]
  } deriving (Generic, Eq, Ord)

data DocHandler a = DocHandler
  { _dhDoc :: Doc
  , _dhHandler :: a
  } deriving (Generic, Functor)

dhDoc :: Lens' (DocHandler a) Doc
dhDoc f DocHandler{..} = (\_dhDoc -> DocHandler{..}) <$> f _dhDoc

dhHandler :: Lens (DocHandler a) (DocHandler b) a b
dhHandler f DocHandler{..} = (\_dhHandler -> DocHandler{..}) <$> f _dhHandler

data IsShifted = Shifted | NotShifted
  deriving (Generic, Eq, Ord, Show, Read)

type InputDoc = String

-- AllCharsHandler always conflict with each other, but they may or may
-- not conflict with shifted/unshifted key events (but not with
-- alt'd/ctrl'd)
data AllCharsHandler a = AllCharsHandler
  { _chInputDoc :: InputDoc
  , _chDocHandler :: DocHandler (Char -> IsShifted -> Maybe a)
  } deriving (Generic, Functor)

chInputDoc :: Lens' (AllCharsHandler a) InputDoc
chInputDoc f AllCharsHandler{..} = (\_chInputDoc -> AllCharsHandler{..}) <$> f _chInputDoc

chDocHandler ::
  Lens
  (AllCharsHandler a) (AllCharsHandler b)
  (DocHandler (Char -> IsShifted -> Maybe a)) (DocHandler (Char -> IsShifted -> Maybe b))
chDocHandler f AllCharsHandler{..} = (\_chDocHandler -> AllCharsHandler{..}) <$> f _chDocHandler

data CharGroupHandler a = CharGroupHandler
  { _cgInputDoc :: InputDoc
  , _cgChars :: Set (Char, IsShifted)
  , _cgDocHandler :: DocHandler (Char -> IsShifted -> a)
  } deriving (Generic, Functor)

cgInputDoc :: Lens' (CharGroupHandler a) InputDoc
cgInputDoc f CharGroupHandler{..} = (\_cgInputDoc -> CharGroupHandler{..}) <$> f _cgInputDoc

cgChars :: Lens' (CharGroupHandler a) (Set (Char, IsShifted))
cgChars f CharGroupHandler{..} = (\_cgChars -> CharGroupHandler{..}) <$> f _cgChars

cgDocHandler ::
  Lens
  (CharGroupHandler a)
  (CharGroupHandler b)
  (DocHandler (Char -> IsShifted -> a))
  (DocHandler (Char -> IsShifted -> b))
cgDocHandler f CharGroupHandler{..} = (\_cgDocHandler -> CharGroupHandler{..}) <$> f _cgDocHandler

data EventMap a = EventMap
  { _emKeyMap :: Map KeyEvent (DocHandler a)
  , _emCharGroupHandlers :: [CharGroupHandler a]
  , _emCharGroupChars :: Set (Char, IsShifted)
  , _emAllCharsHandler :: Maybe (AllCharsHandler a)
  , _emTickHandlers :: [a]
  } deriving (Generic, Functor)

emKeyMap :: Lens' (EventMap a) (Map KeyEvent (DocHandler a))
emKeyMap f EventMap{..} = (\_emKeyMap -> EventMap{..}) <$> f _emKeyMap

emCharGroupHandlers :: Lens' (EventMap a) [CharGroupHandler a]
emCharGroupHandlers f EventMap{..} = (\_emCharGroupHandlers -> EventMap{..}) <$> f _emCharGroupHandlers

emCharGroupChars :: Lens' (EventMap a) (Set (Char, IsShifted))
emCharGroupChars f EventMap{..} = (\_emCharGroupChars -> EventMap{..}) <$> f _emCharGroupChars

emAllCharsHandler :: Lens' (EventMap a) (Maybe (AllCharsHandler a))
emAllCharsHandler f EventMap{..} = (\_emAllCharsHandler -> EventMap{..}) <$> f _emAllCharsHandler

emTickHandlers :: Lens' (EventMap a) [a]
emTickHandlers f EventMap{..} = (\_emTickHandlers -> EventMap{..}) <$> f _emTickHandlers

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

-- TODO: Remove this:
mkModKey :: ModState -> Key -> ModKey
mkModKey = ModKey

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
