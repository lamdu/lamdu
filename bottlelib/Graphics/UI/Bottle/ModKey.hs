-- | ModKey type: Grouping the modifier keys with the key
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
module Graphics.UI.Bottle.ModKey
    ( ModKey(..), ctrlMods, altMods, shiftMods, superMods
    , ctrl, alt, shift, super
    , prettyKey
    , pretty
    ) where

import           Prelude.Compat

import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.List (isPrefixOf)
import           GHC.Generics (Generic)
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Instances ()

instance Monoid GLFW.ModifierKeys where
    mempty = GLFW.ModifierKeys False False False False
    mappend
        (GLFW.ModifierKeys a0 b0 c0 d0)
        (GLFW.ModifierKeys a1 b1 c1 d1) =
            GLFW.ModifierKeys (a0||a1) (b0||b1) (c0||c1) (d0||d1)

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
    deriving (Generic, Show, Eq, Ord)

instance ToJSON ModKey
instance FromJSON ModKey

prettyKey :: GLFW.Key -> String
prettyKey k
    | "Key'" `isPrefixOf` show k = drop 4 $ show k
    | otherwise = show k

prettyModKeys :: GLFW.ModifierKeys -> String
prettyModKeys ms = concat $
    ["Ctrl+" | GLFW.modifierKeysControl ms] ++
    ["Alt+" | GLFW.modifierKeysAlt ms] ++
    ["Shift+" | GLFW.modifierKeysShift ms]

pretty :: ModKey -> String
pretty (ModKey ms key) = prettyModKeys ms ++ prettyKey key
