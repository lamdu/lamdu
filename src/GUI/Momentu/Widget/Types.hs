{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, TemplateHaskell, DeriveGeneric, DeriveFunctor #-}
module GUI.Momentu.Widget.Types
    ( Id(..)
    , EnterResult(..), enterResultEvent, enterResultRect, enterResultLayer
    , EventResult(..), eCursor, eVirtualCursor, eAnimIdMapping
    , State(..), _StateFocused, _StateUnfocused
    , Widget(..), wState, wSize
    , VirtualCursor(..), virtualCursor
    , Unfocused(..), uMEnter, uLayers
    , Focused(..), fFocalAreas, fEventMap, fMEnter, fLayers
    , Surrounding(..), sLeft, sTop, sRight, sBottom
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import qualified Data.Monoid as Monoid
import           GUI.Momentu.Animation (AnimId, R, Size)
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.Rect (Rect)

import           Lamdu.Prelude

data Widget a = Widget
    { _wSize :: Size
    , _wState :: State a
    } deriving Functor

data State a
    = StateUnfocused (Unfocused a)
    | StateFocused (Surrounding -> Focused a)
    deriving Functor

data Unfocused a = Unfocused
    { _uMEnter :: Maybe (Direction -> EnterResult a)
    , _uLayers :: Element.Layers
    } deriving Functor

data EnterResult a = EnterResult
    { -- The new focal area upon this entrace.
      -- Used in Grid to decide which cell's EnterResult to use.
      _enterResultRect :: Rect
    , -- Used to allow grid to choose hovering results over the results below them.
      _enterResultLayer :: Int
    , _enterResultEvent :: a
    } deriving Functor

-- Area on screen around a focused widget. Used for positioning of hovers.
data Surrounding = Surrounding
    { _sLeft :: !R
    , _sTop :: !R
    , _sRight :: !R
    , _sBottom :: !R
    } deriving (Eq, Ord, Show)

-- When focused, mEnter may still be relevant, e.g: Mouse click in an
-- active textedit, to move to a different text-edit position.

data Focused a = Focused
    { -- When browsing sub-menus each selected menu is considered focal.
      -- The last focal area is where the cursor is,
      -- however Zoom should care about the first focal area
      _fFocalAreas :: [Rect]
    , _fEventMap :: VirtualCursor -> EventMap a
    , -- TODO: Replace with fMEnterPoint that is for Point direction only
      _fMEnter :: Maybe (Direction -> EnterResult a)
    , _fLayers :: Element.Layers
    } deriving Functor

-- The virtual cursor is the focal area that would ideally match the
-- direction of user movements
newtype VirtualCursor = VirtualCursor { _virtualCursor :: Rect }
Lens.makeLenses ''VirtualCursor

data EventResult = EventResult
    { _eCursor :: Monoid.Last Id
    , _eVirtualCursor :: Monoid.Last VirtualCursor
    , _eAnimIdMapping :: Monoid.Endo AnimId
    } deriving (Generic)

newtype Id = Id
    { toAnimId :: AnimId
    } deriving (Eq, Ord, Read, Binary, Monoid)

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Focused
Lens.makeLenses ''Surrounding
Lens.makeLenses ''Unfocused
Lens.makeLenses ''Widget
Lens.makePrisms ''State
