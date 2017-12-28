{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor #-}
module GUI.Momentu.Widget.Types
    ( Widget(..), wState, wSize
    , State(..), _StateFocused, _StateUnfocused
    , Unfocused(..), uMEnter, uLayers
    , EnterResult(..), enterResultEvent, enterResultRect, enterResultLayer
    , Surrounding(..), sLeft, sTop, sRight, sBottom
    , Focused(..), fFocalAreas, fEventMap, fMEnterPoint, fLayers, fPreEvents
    , PreEvent(..), pDesc, pAction, pTextRemainder
    , EventContext(..), eVirtualCursor, ePrevTextRemainder
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2)
import           GUI.Momentu.Animation (R, Size)
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap, Subtitle)
import           GUI.Momentu.Rect (Rect)
import           GUI.Momentu.State (VirtualCursor)

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

data Focused a = Focused
    { -- When browsing sub-menus each selected menu is considered focal.
      -- The last focal area is where the cursor is,
      -- however Zoom should care about the first focal area
      _fFocalAreas :: [Rect]
    , _fEventMap :: EventContext -> EventMap a
    , _fPreEvents :: [PreEvent a]
    , _fMEnterPoint :: Maybe (Vector2 R -> EnterResult a)
    , _fLayers :: Element.Layers
    } deriving Functor

data PreEvent a = PreEvent
    { _pDesc :: Subtitle
    , _pAction :: a
    , _pTextRemainder :: Text
    } deriving Functor

data EventContext = EventContext
    { _eVirtualCursor :: VirtualCursor
    , -- | Remainder text from previous ambigious entry.
      -- For example, when typing expressions: the partial text "42."
      -- can end up as "42..50" (a range) or "42.1" (a number).
      -- When typing the second dot in "42..50" the first dot is the
      -- "text remainder".  Used when creating widgets using
      -- `PreEvent`s.
      _ePrevTextRemainder :: Text
    }

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventContext
Lens.makeLenses ''Focused
Lens.makeLenses ''PreEvent
Lens.makeLenses ''Surrounding
Lens.makeLenses ''Unfocused
Lens.makeLenses ''Widget
Lens.makePrisms ''State
