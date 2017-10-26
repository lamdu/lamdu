{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor #-}
module GUI.Momentu.Widget.Types
    ( Widget(..), wState, wSize
    , State(..), _StateFocused, _StateUnfocused
    , Unfocused(..), uMEnter, uLayers
    , EnterResult(..), enterResultEvent, enterResultRect, enterResultLayer
    , Surrounding(..), sLeft, sTop, sRight, sBottom
    , Focused(..), fFocalAreas, fEventMap, fMEnter, fLayers
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (R, Size)
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
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
    , _fEventMap :: VirtualCursor -> EventMap a
    , -- When focused, mEnter may still be relevant, e.g: Mouse click in an
      -- active textedit, to move to a different text-edit position.
      -- TODO: Replace with fMEnterPoint that is for Point direction only
      _fMEnter :: Maybe (Direction -> EnterResult a)
    , _fLayers :: Element.Layers
    } deriving Functor

Lens.makeLenses ''EnterResult
Lens.makeLenses ''Focused
Lens.makeLenses ''Surrounding
Lens.makeLenses ''Unfocused
Lens.makeLenses ''Widget
Lens.makePrisms ''State
