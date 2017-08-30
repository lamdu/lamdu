{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, DeriveTraversable #-}

module GUI.Momentu.Widgets.Menu
    ( Option(..), oPickEventMap, oWidget, oSubmenuSymbol, oSubmenuWidget
    , OrderedOptions(..), optionsFromTop, optionsFromBottom
    , Placement(..), HasMoreOptions(..)
    , layout
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Option m = Option
    { -- An event-map to pick this option
      _oPickEventMap :: Widget.EventMap (m Widget.EventResult)
    , -- A widget that represents this option
      _oWidget :: WithTextPos (Widget (m Widget.EventResult))
    , -- An optionally empty symbol indicating a submenu is available
      _oSubmenuSymbol :: WithTextPos View
    , -- An optionally empty submenu
      _oSubmenuWidget :: Widget (m Widget.EventResult)
    }
Lens.makeLenses ''Option

data OrderedOptions a = OrderedOptions
    { _optionsFromTop :: a
    , _optionsFromBottom :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''OrderedOptions

instance Applicative OrderedOptions where
    pure = join OrderedOptions
    OrderedOptions fa fb <*> OrderedOptions xa xb =
        OrderedOptions (fa xa) (fb xb)

-- | You may want to limit the placement of hovering pop-up menus,
-- so that they don't cover other ui elements.
data Placement = Above | Below | AnyPlace

data HasMoreOptions = MoreOptionsAvailable | NoMoreOptions

makeNoResults ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    m (WithTextPos View)
makeNoResults = TextView.makeLabel "(No results)"

makeMoreOptionsView ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    HasMoreOptions -> m (WithTextPos View)
makeMoreOptionsView NoMoreOptions = pure Element.empty
makeMoreOptionsView MoreOptionsAvailable = TextView.makeLabel "..."

blockEvents ::
    Applicative f =>
    OrderedOptions (Widget (f (Widget.EventResult)) -> Widget (f (Widget.EventResult)))
blockEvents =
    OrderedOptions
    { _optionsFromTop = blockDirection MetaKey.Key'Down "down"
    , _optionsFromBottom = blockDirection MetaKey.Key'Up "up"
    }
    where
        blockDirection key keyName =
            pure mempty
            & E.keyPresses
                [ModKey mempty key]
                (E.Doc ["Navigation", "Move", keyName <> " (blocked)"])
            & E.weakerEvents

layout ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env, Applicative f) =>
    Widget.R -> [Option f] -> HasMoreOptions ->
    m (OrderedOptions (Widget (f Widget.EventResult)))
layout minWidth options hiddenResults
    | null options = makeNoResults <&> (^. Align.tValue) <&> Widget.fromView <&> pure
    | otherwise =
        do
            hiddenOptionsWidget <- makeMoreOptionsView hiddenResults
            blockEvents <*>
                ( OrderedOptions
                    { _optionsFromTop = id
                    , _optionsFromBottom = reverse
                    } ?? ((options <&> layoutOption) ++ [Widget.fromView (hiddenOptionsWidget ^. Align.tValue)])
                    <&> Glue.vbox
                ) & pure
    where
        layoutOption option =
            Hover.hoverInPlaceOf
            (Hover.hoverBesideOptionsAxis Glue.Horizontal (option ^. oSubmenuWidget) base)
            base
            where
                base =
                    ((option ^. oWidget
                         & Element.width .~ maxMainResultWidth - option ^. oSubmenuSymbol . Element.width)
                        /|/ (option ^. oSubmenuSymbol)) ^. Align.tValue & Hover.anchor
        maxMainResultWidth = options <&> optionMinWidth & maximum & max minWidth
        optionMinWidth option =
            option ^. oWidget . Element.width +
            option ^. oSubmenuSymbol . Element.width
