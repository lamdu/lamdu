{-# LANGUAGE DeriveGeneric #-}
module Graphics.UI.GLFW.ModState (
  ModState(..), noMods, shift, ctrl, alt,
  Pos(..), ModType(..), Modkey(..),
  modStateFromModkeySet, asModkey, isModifierKey) where

import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Data.Set(Set)
import GHC.Generics (Generic)
import Prelude hiding (Left, Right)
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW

data ModState = ModState
  { modCtrl :: Bool
  , modAlt :: Bool
  , modShift :: Bool
  } deriving (Generic, Show, Read, Eq, Ord)

instance Monoid ModState where
  mempty = noMods
  ModState c0 a0 s0 `mappend` ModState c1 a1 s1 =
    ModState (c0 || c1) (a0 || a1) (s0 || s1)

data Pos = Left | Right
  deriving (Generic, Enum, Bounded, Show, Read, Eq, Ord)
data ModType = Ctrl | Alt | Shift
  deriving (Generic, Enum, Bounded, Show, Read, Eq, Ord)
data Modkey = Modkey Pos ModType
  deriving (Generic, Show, Read, Eq, Ord)

allVals :: (Enum a, Bounded a) => [a]
allVals = [minBound..maxBound]

asModkey :: GLFW.Key -> Maybe Modkey
asModkey GLFW.Key'RightShift = Just $ Modkey Right Shift
asModkey GLFW.Key'LeftShift = Just $ Modkey Left Shift
asModkey GLFW.Key'RightControl = Just $ Modkey Right Ctrl
asModkey GLFW.Key'LeftControl = Just $ Modkey Left Ctrl
asModkey GLFW.Key'RightAlt = Just $ Modkey Right Alt
asModkey GLFW.Key'LeftAlt = Just $ Modkey Left Alt
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
