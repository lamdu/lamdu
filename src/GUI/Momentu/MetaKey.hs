-- | Cross platform ModKey.

{-# LANGUAGE TemplateHaskell #-}

module GUI.Momentu.MetaKey
    ( ModifierKeys(..), cmdOn, altOn, shiftOn, metaOn
    , noMods, cmd, shift, numModsOn
    , MetaKey(..), modifiers, key
    , ModKey.Key(..), ModKey.KeyState(..)
    , parse, format
    , toModKey, toGLFWModifiers
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.ModKey as ModKey
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Instances ()
import qualified System.Info as SysInfo
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint.HughesPJClass (Pretty(..))
import           Text.Read (readMaybe)

import           GUI.Momentu.Prelude

data ModifierKeys = ModifierKeys
    { _cmdOn :: Bool -- Ctrl on most platforms. Cmd on macOS
    , _altOn :: Bool
    , _shiftOn :: Bool
    , _metaOn :: Bool -- Win-key on most platforms. Ctrl on macOS
    } deriving (Show, Eq, Ord)
Lens.makeLenses ''ModifierKeys

data MetaKey = MetaKey
    { _modifiers :: ModifierKeys
    , _key :: ModKey.Key
    }
    deriving (Show, Eq, Ord)
Lens.makeLenses ''MetaKey

noMods :: ModifierKeys
noMods = ModifierKeys False False False False

numModsOn :: ModifierKeys -> Int
numModsOn mods =
    f cmdOn + f altOn + f shiftOn + f metaOn
    where
        f m
            | mods ^. m = 1
            | otherwise = 0

cmd :: ModKey.Key -> MetaKey
cmd = MetaKey (noMods & cmdOn .~ True)

shift :: ModKey.Key -> MetaKey
shift = MetaKey (noMods & shiftOn .~ True)

parse :: Text -> Maybe MetaKey
parse s =
    case readMaybe ("Key'" ++ Text.unpack (last parts)) of
    Just k | numModsOn mods == length modsTexts ->
        Just MetaKey
        { _key = k
        , _modifiers = mods
        }
    _ -> Nothing
    where
        parts = Text.splitOn "+" s
        modsTexts = init parts
        mods =
            ModifierKeys
            { _cmdOn = "Cmd" `elem` modsTexts
            , _altOn = "Alt" `elem` modsTexts
            , _shiftOn = "Shift" `elem` modsTexts
            , _metaOn = "Meta" `elem` modsTexts
            }

instance Pretty MetaKey where
    pPrint = Pretty.text . Text.unpack . format

format :: MetaKey -> Text
format (MetaKey mods k) =
    ["Cmd+" | mods ^. cmdOn] ++
    ["Alt+" | mods ^. altOn] ++
    ["Shift+" | mods ^. shiftOn] ++
    ["Meta+" | mods ^. metaOn] ++
    [show k & drop 4 & Text.pack]
    & mconcat

instance Aeson.FromJSON MetaKey where
    parseJSON (Aeson.String s) =
        parse s & maybe (fail ("invalid key " ++ Text.unpack s)) pure
    parseJSON _ = fail "expected string"

instance Aeson.ToJSON MetaKey where
    toJSON m = format m & Aeson.String

toGLFWModifiers :: ModifierKeys -> GLFW.ModifierKeys
toGLFWModifiers (ModifierKeys cmd_ alt_ shift_ meta_)
    | SysInfo.os == "darwin" =
        GLFW.ModifierKeys shift_ meta_ alt_ cmd_ False False
    | otherwise =
        GLFW.ModifierKeys shift_ cmd_ alt_ meta_ False False

toModKey :: MetaKey -> ModKey
toModKey (MetaKey mods k) = ModKey (toGLFWModifiers mods) k
