{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Widget.Types
    ( Widget(..), wState, wSize
    , State(..), _StateFocused, _StateUnfocused
    , Unfocused(..), uMEnter, uMStroll, uLayers
    , EnterResult(..), enterResultEvent, enterResultRect, enterResultLayer
    , Surrounding(..), sLeft, sTop, sRight, sBottom
    , Focused(..), fFocalAreas, fEventMap, fMEnterPoint, fLayers, fPreEvents
    , PreEvents(..), _BlockEvents, _PreEvents
    , PreEvent(..), pDesc, pAction, pTextRemainder
    , EventContext(..), eVirtualCursor, ePrevTextRemainder
    ) where

import qualified Control.Lens as Lens
import qualified Data.Semigroup as Semigroup
import           Data.Vector.Vector2 (Vector2)
import           GUI.Momentu.Animation (R, Size)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.FocusDirection (FocusDirection)
import           GUI.Momentu.Rect (Rect)
import           GUI.Momentu.State (VirtualCursor, Update)
import           GUI.Momentu.Widget.Id (Id)

import           GUI.Momentu.Prelude

data Widget f = Widget
    { _wSize :: Size
    , _wState :: State (f Update)
    }

data State a
    = StateUnfocused (Unfocused a)
    | StateFocused (Surrounding -> Focused a)
    deriving Functor

data Unfocused a = Unfocused
    { _uMEnter :: Maybe (FocusDirection -> EnterResult a)
    , _uMStroll ::
        -- "Strolling" is navigating using "Tab"/"Shift-Tab"
        -- to form entry fields or other points of interest.
        Maybe (Semigroup.First Id, Semigroup.Last Id)
    , _uLayers :: Element.LayeredImage
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

data PreEvents a
    = PreEvents [PreEvent a]
    | BlockEvents
    deriving Functor

instance Semigroup (PreEvents a) where
    BlockEvents <> _ = BlockEvents
    _ <> BlockEvents = BlockEvents
    PreEvents xs <> PreEvents ys = PreEvents (xs ++ ys)

instance Monoid (PreEvents a) where mempty = PreEvents []

data Focused a = Focused
    { -- When browsing sub-menus each selected menu is considered focal.
      -- The last focal area is where the cursor is,
      -- however Zoom should care about the first focal area
      _fFocalAreas :: [Rect]
    , _fEventMap :: EventContext -> EventMap a
    , _fPreEvents :: PreEvents a
    , _fMEnterPoint :: Maybe (Vector2 R -> EnterResult a)
    , _fLayers :: Element.LayeredImage
    } deriving Functor

data PreEvent a = PreEvent
    { _pDesc :: Text
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

traverse Lens.makeLenses
    [ ''EnterResult, ''EventContext, ''Focused, ''PreEvent
    , ''Surrounding, ''Unfocused, ''Widget]
    <&> concat
traverse Lens.makePrisms [''PreEvents, ''State] <&> concat
