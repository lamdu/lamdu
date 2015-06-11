{-# LANGUAGE DeriveFunctor, DeriveGeneric, RecordWildCards #-}
module Graphics.UI.Bottle.EventMap
    ( KeyEvent(..)
    , InputDoc, Subtitle, Doc(..), docStrs
    , EventMap, lookup, emTickHandlers
    , emDocs
    , charEventMap, allChars
    , charGroup
    , keyEventMap, keyPress, keyPresses
    , deleteKey, deleteKeys
    , filterChars
    , eventMapDocs
    , tickHandler
    , specialCharKey
    ) where

import           Prelude hiding (lookup)

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Arrow ((***), (&&&))
import           Control.Lens (Lens, Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (guard, msum)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, listToMaybe, maybeToList)
import           Data.Monoid (Monoid(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Events as Events

data KeyEvent = KeyEvent GLFW.KeyState ModKey
    deriving (Generic, Show, Eq, Ord)

specialCharKey :: Char -> Maybe GLFW.Key
specialCharKey c =
    case c of
    ' ' -> Just GLFW.Key'Space
    '0' -> Just GLFW.Key'Pad0
    '1' -> Just GLFW.Key'Pad1
    '2' -> Just GLFW.Key'Pad2
    '3' -> Just GLFW.Key'Pad3
    '4' -> Just GLFW.Key'Pad4
    '5' -> Just GLFW.Key'Pad5
    '6' -> Just GLFW.Key'Pad6
    '7' -> Just GLFW.Key'Pad7
    '8' -> Just GLFW.Key'Pad8
    '9' -> Just GLFW.Key'Pad9
    '/' -> Just GLFW.Key'PadDivide
    '*' -> Just GLFW.Key'PadMultiply
    '-' -> Just GLFW.Key'PadSubtract
    '+' -> Just GLFW.Key'PadAdd
    '.' -> Just GLFW.Key'PadDecimal
    '=' -> Just GLFW.Key'PadEqual
    _ -> Nothing

charOfKey :: GLFW.Key -> Maybe Char
charOfKey key =
    case key of
    GLFW.Key'A           -> Just 'A'
    GLFW.Key'B           -> Just 'B'
    GLFW.Key'C           -> Just 'C'
    GLFW.Key'D           -> Just 'D'
    GLFW.Key'E           -> Just 'E'
    GLFW.Key'F           -> Just 'F'
    GLFW.Key'G           -> Just 'G'
    GLFW.Key'H           -> Just 'H'
    GLFW.Key'I           -> Just 'I'
    GLFW.Key'J           -> Just 'J'
    GLFW.Key'K           -> Just 'K'
    GLFW.Key'L           -> Just 'L'
    GLFW.Key'M           -> Just 'M'
    GLFW.Key'N           -> Just 'N'
    GLFW.Key'O           -> Just 'O'
    GLFW.Key'P           -> Just 'P'
    GLFW.Key'Q           -> Just 'Q'
    GLFW.Key'R           -> Just 'R'
    GLFW.Key'S           -> Just 'S'
    GLFW.Key'T           -> Just 'T'
    GLFW.Key'U           -> Just 'U'
    GLFW.Key'V           -> Just 'V'
    GLFW.Key'W           -> Just 'W'
    GLFW.Key'X           -> Just 'X'
    GLFW.Key'Y           -> Just 'Y'
    GLFW.Key'Z           -> Just 'Z'
    GLFW.Key'Space       -> Just ' '
    GLFW.Key'Pad0        -> Just '0'
    GLFW.Key'Pad1        -> Just '1'
    GLFW.Key'Pad2        -> Just '2'
    GLFW.Key'Pad3        -> Just '3'
    GLFW.Key'Pad4        -> Just '4'
    GLFW.Key'Pad5        -> Just '5'
    GLFW.Key'Pad6        -> Just '6'
    GLFW.Key'Pad7        -> Just '7'
    GLFW.Key'Pad8        -> Just '8'
    GLFW.Key'Pad9        -> Just '9'
    GLFW.Key'PadDivide   -> Just '/'
    GLFW.Key'PadMultiply -> Just '*'
    GLFW.Key'PadSubtract -> Just '-'
    GLFW.Key'PadAdd      -> Just '+'
    GLFW.Key'PadDecimal  -> Just '.'
    GLFW.Key'PadEqual    -> Just '='
    _              -> Nothing

type Subtitle = String

newtype Doc = Doc
    { _docStrs :: [Subtitle]
    } deriving (Generic, Eq, Ord)

docStrs :: Lens.Iso' Doc [Subtitle]
docStrs = Lens.iso _docStrs Doc

data DocHandler a = DocHandler
    { _dhDoc :: Doc
    , _dhHandler :: a
    } deriving (Generic, Functor)

dhDoc :: Lens' (DocHandler a) Doc
dhDoc f DocHandler{..} = f _dhDoc <&> \_dhDoc -> DocHandler{..}

dhHandler :: Lens (DocHandler a) (DocHandler b) a b
dhHandler f DocHandler{..} = f _dhHandler <&> \_dhHandler -> DocHandler{..}

type InputDoc = String

-- AllCharsHandler always conflict with each other
data AllCharsHandler a = AllCharsHandler
    { _chInputDoc :: InputDoc
    , _chDocHandler :: DocHandler (Char -> Maybe a)
    } deriving (Generic, Functor)

chInputDoc :: Lens' (AllCharsHandler a) InputDoc
chInputDoc f AllCharsHandler{..} = f _chInputDoc <&> \_chInputDoc -> AllCharsHandler{..}

chDocHandler ::
    Lens
    (AllCharsHandler a) (AllCharsHandler b)
    (DocHandler (Char -> Maybe a)) (DocHandler (Char -> Maybe b))
chDocHandler f AllCharsHandler{..} = f _chDocHandler <&> \_chDocHandler -> AllCharsHandler{..}

data CharGroupHandler a = CharGroupHandler
    { _cgInputDoc :: InputDoc
    , _cgChars :: Set Char
    , _cgDocHandler :: DocHandler (Char -> a)
    } deriving (Generic, Functor)

cgInputDoc :: Lens' (CharGroupHandler a) InputDoc
cgInputDoc f CharGroupHandler{..} = f _cgInputDoc <&> \_cgInputDoc -> CharGroupHandler{..}

cgChars :: Lens' (CharGroupHandler a) (Set Char)
cgChars f CharGroupHandler{..} = f _cgChars <&> \_cgChars -> CharGroupHandler{..}

cgDocHandler ::
    Lens
    (CharGroupHandler a)
    (CharGroupHandler b)
    (DocHandler (Char -> a))
    (DocHandler (Char -> b))
cgDocHandler f CharGroupHandler{..} = f _cgDocHandler <&> \_cgDocHandler -> CharGroupHandler{..}

data EventMap a = EventMap
    { _emKeyMap :: Map KeyEvent (DocHandler a)
    , _emCharGroupHandlers :: [CharGroupHandler a]
    , _emCharGroupChars :: Set Char
    , _emAllCharsHandler :: [AllCharsHandler a]
    , _emTickHandlers :: [a]
    } deriving (Generic, Functor)

emDocs :: Lens.Traversal' (EventMap a) Doc
emDocs f (EventMap keyMap charGroupHandlers charGroupChars allCharsHandler tickHandlers) =
    EventMap
    <$> (Lens.traverse . dhDoc) f keyMap
    <*> (Lens.traverse . cgDocHandler . dhDoc) f charGroupHandlers
    <*> pure charGroupChars
    <*> (Lens.traverse . chDocHandler . dhDoc) f allCharsHandler
    <*> pure tickHandlers

emKeyMap :: Lens' (EventMap a) (Map KeyEvent (DocHandler a))
emKeyMap f EventMap{..} = f _emKeyMap <&> \_emKeyMap -> EventMap{..}

emCharGroupHandlers :: Lens' (EventMap a) [CharGroupHandler a]
emCharGroupHandlers f EventMap{..} = f _emCharGroupHandlers <&> \_emCharGroupHandlers -> EventMap{..}

emCharGroupChars :: Lens' (EventMap a) (Set Char)
emCharGroupChars f EventMap{..} = f _emCharGroupChars <&> \_emCharGroupChars -> EventMap{..}

emAllCharsHandler :: Lens' (EventMap a) [AllCharsHandler a]
emAllCharsHandler f EventMap{..} = f _emAllCharsHandler <&> \_emAllCharsHandler -> EventMap{..}

emTickHandlers :: Lens' (EventMap a) [a]
emTickHandlers f EventMap{..} = f _emTickHandlers <&> \_emTickHandlers -> EventMap{..}

instance Monoid (EventMap a) where
    mempty = EventMap Map.empty [] Set.empty [] []
    mappend = overrides

overrides :: EventMap a -> EventMap a -> EventMap a
overrides
    x@(EventMap xMap xCharGroups xChars xMAllChars xTicks)
    (EventMap yMap yCharGroups yChars yMAllChars yTicks) =
    EventMap
    (xMap `mappend` filteredYMap)
    (xCharGroups ++ filteredYCharGroups)
    (xChars `mappend` yChars)
    (xMAllChars ++ yMAllChars)
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
    filter (not . Set.null . (^. cgChars)) .
    (Lens.traversed . cgChars %~ Set.filter f)

isCharConflict :: EventMap a -> Char -> Bool
isCharConflict eventMap char =
    char `Set.member` (eventMap ^. emCharGroupChars) ||
    (not . null . catMaybes)
    (($ char) . (^. chDocHandler . dhHandler) <$>
      eventMap ^. emAllCharsHandler)

filterChars
    :: (Char -> Bool) -> EventMap a -> EventMap a
filterChars p =
    (emCharGroupHandlers %~ filterCharGroups p) .
    (emCharGroupChars %~ Set.filter p) .
    (emAllCharsHandler . Lens.traversed . chDocHandler . dhHandler %~ f)
    where
        f handler c = do
            guard $ p c
            handler c

prettyKeyEvent :: KeyEvent -> InputDoc
prettyKeyEvent (KeyEvent GLFW.KeyState'Pressed modKey) = ModKey.pretty modKey
prettyKeyEvent (KeyEvent GLFW.KeyState'Repeating modKey) = "Repeat " ++ ModKey.pretty modKey
prettyKeyEvent (KeyEvent GLFW.KeyState'Released modKey) = "Depress " ++ ModKey.pretty modKey

isCharMods :: GLFW.ModifierKeys -> Bool
isCharMods modKeys =
        not $ any ($ modKeys)
        [ GLFW.modifierKeysSuper
        , GLFW.modifierKeysControl
        , GLFW.modifierKeysAlt
        ]

-- TODO: Remove this:
mkModKey :: GLFW.ModifierKeys -> GLFW.Key -> ModKey
mkModKey = ModKey

eventMapDocs :: EventMap a -> [(InputDoc, Doc)]
eventMapDocs (EventMap dict charGroups _ mAllCharsHandler _) =
    concat
    [ map ((^. chInputDoc) &&& (^. chDocHandler . dhDoc)) mAllCharsHandler
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
lookup (Events.KeyEvent k _scanCode keyState modKeys mchar) (EventMap dict charGroups _ allCharHandlers _) =
    msum
    [ (^. dhHandler) <$>
        KeyEvent keyState modKey `Map.lookup` dict
    , listToMaybe $ do
            GLFW.KeyState'Pressed <- return keyState
            char <- maybeToList mchar
            CharGroupHandler _ chars handler <- charGroups
            guard $ Set.member char chars
            return $ (handler ^. dhHandler) char
    , listToMaybe $ do
            GLFW.KeyState'Pressed <- return keyState
            char <- maybeToList mchar
            AllCharsHandler _ handler <- allCharHandlers
            maybeToList $ (handler ^. dhHandler) char
    ]
    where
        modKey = mkModKey modKeys k

charGroup :: InputDoc -> Doc -> String -> (Char -> a) -> EventMap a
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
    :: InputDoc -> Doc -> (Char -> Maybe a) -> EventMap a
charEventMap iDoc oDoc handler =
    mempty
    { _emAllCharsHandler =
        [AllCharsHandler iDoc (DocHandler oDoc handler)]
    }

allChars :: InputDoc -> Doc -> (Char -> a) -> EventMap a
allChars iDoc oDoc f = charEventMap iDoc oDoc $ Just . f

keyEventMap :: KeyEvent -> Doc -> a -> EventMap a
keyEventMap eventType doc handler =
    mempty
    { _emKeyMap = Map.singleton eventType $ DocHandler doc handler
    }

keyPress :: ModKey -> Doc -> a -> EventMap a
keyPress = keyEventMap . KeyEvent GLFW.KeyState'Pressed

keyPresses :: [ModKey] -> Doc -> a -> EventMap a
keyPresses = mconcat . map keyPress

tickHandler :: a -> EventMap a
tickHandler x = mempty { _emTickHandlers = [x] }
