-- | ModKey type: Grouping the modifier keys with the key
{-# OPTIONS -fno-warn-orphans #-}
module GUI.Momentu.ModKey
    ( ModKey(..), ctrlMods, altMods, shiftMods, superMods
    , GLFW.KeyState(..), GLFW.Key(..)
    , ctrl, alt, shift, super
    , prettyKey
    , pretty
    ) where

import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.List (isPrefixOf)
import qualified Data.Text as Text
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Instances ()
import qualified System.Info as SysInfo

import           GUI.Momentu.Prelude

instance Semigroup GLFW.ModifierKeys where
    GLFW.ModifierKeys a0 b0 c0 d0 e0 f0 <> GLFW.ModifierKeys a1 b1 c1 d1 e1 f1 =
        GLFW.ModifierKeys (a0||a1) (b0||b1) (c0||c1) (d0||d1) (e0||e1) (f0||f1)
instance Monoid GLFW.ModifierKeys where
    mempty = GLFW.ModifierKeys False False False False False False

ctrlMods :: GLFW.ModifierKeys
ctrlMods = mempty { GLFW.modifierKeysControl = True }

altMods :: GLFW.ModifierKeys
altMods = mempty { GLFW.modifierKeysAlt = True }

shiftMods :: GLFW.ModifierKeys
shiftMods = mempty { GLFW.modifierKeysShift = True }

superMods :: GLFW.ModifierKeys
superMods = mempty { GLFW.modifierKeysSuper = True }

ctrl :: GLFW.Key -> ModKey
ctrl = ModKey ctrlMods

alt :: GLFW.Key -> ModKey
alt = ModKey altMods

shift :: GLFW.Key -> ModKey
shift = ModKey shiftMods

super :: GLFW.Key -> ModKey
super = ModKey superMods

data ModKey = ModKey GLFW.ModifierKeys GLFW.Key
    deriving stock (Generic, Show, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON)

prettyKey :: GLFW.Key -> Text
prettyKey k
    | "Key'" `isPrefixOf` show k = Text.pack $ drop 4 $ show k
    | otherwise = Text.pack $ show k

prettyModKeys :: GLFW.ModifierKeys -> Text
prettyModKeys ms =
    mconcat $
    [superName | GLFW.modifierKeysSuper ms] ++
    ["Ctrl+" | GLFW.modifierKeysControl ms] ++
    ["Alt+" | GLFW.modifierKeysAlt ms] ++
    ["Shift+" | GLFW.modifierKeysShift ms]
    where
        superName
            | SysInfo.os /= "darwin" = "Win+"
            | ms == superMods = "⌘"
            | otherwise = "⌘+"

pretty :: ModKey -> Text
pretty (ModKey ms key) = prettyModKeys ms <> prettyKey key
