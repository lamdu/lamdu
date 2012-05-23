module Graphics.UI.GLFW.ModState (
  ModState(..), noMods, shift, ctrl, alt,
  Pos(..), ModType(..), Modkey(..),
  modStateFromModkeySet, asModkey, isModifierKey) where

import Prelude hiding (Left, Right)
import Data.Maybe(isJust)
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW

data ModState = ModState {
  modCtrl :: Bool,
  modAlt :: Bool,
  modShift :: Bool
  }
  deriving (Show, Read, Eq, Ord)

data Pos = Left | Right
  deriving (Enum, Bounded, Show, Read, Eq, Ord)
data ModType = Ctrl | Alt | Shift
  deriving (Enum, Bounded, Show, Read, Eq, Ord)
data Modkey = Modkey Pos ModType
  deriving (Show, Read, Eq, Ord)

allVals :: (Enum a, Bounded a) => [a]
allVals = [minBound..maxBound]

asModkey :: GLFW.Key -> Maybe Modkey
asModkey GLFW.KeyRightShift = Just $ Modkey Right Shift
asModkey GLFW.KeyLeftShift = Just $ Modkey Left Shift
asModkey GLFW.KeyRightCtrl = Just $ Modkey Right Ctrl
asModkey GLFW.KeyLeftCtrl = Just $ Modkey Left Ctrl
asModkey GLFW.KeyRightAlt = Just $ Modkey Right Alt
asModkey GLFW.KeyLeftAlt = Just $ Modkey Left Alt
asModkey _ = Nothing

isModifierKey :: GLFW.Key -> Bool
isModifierKey = isJust . asModkey

noMods :: ModState
noMods = ModState False False False

shift :: ModState
shift = noMods { modShift = True }

ctrl :: ModState
ctrl = noMods { modCtrl = True }

alt :: ModState
alt = noMods { modAlt = True }

modStateFromModkeySet :: Set Modkey -> ModState
modStateFromModkeySet keySet =
  ModState {
    modCtrl = isPressed [Modkey d Ctrl | d <- allVals],
    modAlt = isPressed [Modkey d Alt | d <- allVals],
    modShift = isPressed [Modkey d Shift | d <- allVals]
    }
  where
    isPressed = any (`Set.member` keySet)
