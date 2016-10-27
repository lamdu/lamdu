-- | TreeLayout is a layout form intended for tree-data, such as program code.
--
-- Its design goals are:
-- * Make good use of the available screen real-estate.
-- * Avoid horizontal scroll
-- * Display the hierarchy/tree structure clearly
-- * Make changes in layout (due to edits or zooming) easy to follow
--
-- The upper nodes in the tree are layed out vertically, while the subtrees that
-- have space available for it are layed out horizontically.
-- Tree elements which have a horizontal layout must also have a fallback mode
-- to be rendered vertically when there isn't enough horizontal space for their
-- horizontal layout.
--
-- Hierarchy disambiguation happens using parentheses and indentation,
-- but only when necessary, so a horizontal child of a vertical element
-- will not need parentheses as the hierarchy is already clear by the layout.

{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Graphics.UI.Bottle.Widget.TreeLayout
    ( TreeLayout(..), render

    -- Lenses:
    , alignedWidget, widget, alignment

    -- Layout params:
    , LayoutParams(..), layoutMode, layoutContext
    , LayoutMode(..), _LayoutNarrow, _LayoutWide, modeWidths
    , LayoutDisambiguationContext(..)

    -- Various lifters:
    , fixedLayout, fromCenteredWidget, fromCenteredView, empty

    -- Operations:
    , pad
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2)
import           Graphics.UI.Bottle.Alignment (Alignment)
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget

import           Prelude.Compat

data LayoutMode
    = LayoutNarrow Widget.R -- ^ limited by the given
    | LayoutWide -- ^ no limit on width
Lens.makePrisms ''LayoutMode

modeWidths :: Lens.Traversal' LayoutMode Widget.R
modeWidths _ LayoutWide = pure LayoutWide
modeWidths f (LayoutNarrow limit) = f limit <&> LayoutNarrow

-- The relevant context for knowing whether parenthesis/indentation is needed
data LayoutDisambiguationContext
    = LayoutClear
    | LayoutHorizontal
    | LayoutVertical

data LayoutParams = LayoutParams
    { _layoutMode :: LayoutMode
    , _layoutContext :: LayoutDisambiguationContext
    }
Lens.makeLenses ''LayoutParams

newtype TreeLayout a = TreeLayout
    { _render :: LayoutParams -> AlignedWidget a
    }
Lens.makeLenses ''TreeLayout

alignedWidget ::
    Lens.Setter
    (TreeLayout a) (TreeLayout b)
    (AlignedWidget a) (AlignedWidget b)
alignedWidget = render . Lens.mapped

widget :: Lens.Setter (TreeLayout a) (TreeLayout b) (Widget a) (Widget b)
widget = alignedWidget . AlignedWidget.widget

alignment :: Lens.Setter' (TreeLayout a) Alignment
alignment = alignedWidget . AlignedWidget.alignment

fixedLayout :: AlignedWidget a -> TreeLayout a
fixedLayout = TreeLayout . const

fromCenteredWidget :: Widget a -> TreeLayout a
fromCenteredWidget = fixedLayout . AlignedWidget.fromCenteredWidget

fromCenteredView :: View -> TreeLayout a
fromCenteredView = fromCenteredWidget . Widget.fromView

empty :: TreeLayout a
empty = fromCenteredView View.empty

pad :: Vector2 Widget.R -> TreeLayout a -> TreeLayout a
pad p w =
    w
    & render . Lens.argument . layoutMode . modeWidths -~ 2 * (p ^. _1)
    & alignedWidget %~ AlignedWidget.pad p
