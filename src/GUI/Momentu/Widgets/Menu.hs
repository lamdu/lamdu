{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveGeneric, OverloadedStrings, DeriveTraversable #-}

module GUI.Momentu.Widgets.Menu
    ( Style(..), HasStyle(..)
    , Option(..), oId, oWidget, oSubmenuWidgets
    , OrderedOptions(..), optionsFromTop, optionsFromBottom
    , Placement(..), HasMoreOptions(..)
    , layout
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson.Types as Aeson
import           GHC.Generics (Generic)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Style = Style
    { submenuSymbolColorUnselected :: Draw.Color
    , submenuSymbolColorSelected :: Draw.Color
    , bgColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Style where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

data Option m = Option
    { _oId :: !Widget.Id
    , -- A widget that represents this option
      _oWidget :: !(WithTextPos (Widget (m Widget.EventResult)))
    , -- An optionally empty submenu
      _oSubmenuWidgets :: ![WithTextPos (Widget (m Widget.EventResult))]
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
    OrderedOptions (Widget (f Widget.EventResult) -> Widget (f Widget.EventResult))
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


submenuSymbolText :: Text
submenuSymbolText = " ▷"

makeSubmenuSymbol ::
    ( MonadReader env m, HasStyle env, Element.HasAnimIdPrefix env
    , TextView.HasStyle env
    ) =>
    Bool -> m (WithTextPos View)
makeSubmenuSymbol isSelected =
    do
        color <- Lens.view style <&> submenuSymbolColor
        TextView.makeLabel submenuSymbolText
            & Reader.local (TextView.color .~ color)
    where
        submenuSymbolColor
            | isSelected = submenuSymbolColorSelected
            | otherwise = submenuSymbolColorUnselected

addBackground ::
    (MonadReader env m, Element.HasAnimIdPrefix env, HasStyle env, Element a) =>
    m (a -> a)
addBackground =
    do
        animId <- Element.subAnimId ["hover background"]
        color <- Lens.view style <&> bgColor
        Draw.backgroundColor animId color & pure

layoutOption ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    , Hover.HasStyle env, HasStyle env, Functor f
    ) =>
    Widget.R -> Option f -> m (WithTextPos (Widget (f Widget.EventResult)))
layoutOption maxOptionWidth option =
    case option ^. oSubmenuWidgets of
    [] -> option ^. oWidget & Element.width .~ maxOptionWidth & pure
    submenus ->
        do
            submenuSymbol <- makeSubmenuSymbol isSelected
            let base =
                    (option ^. oWidget
                     & Element.width .~ maxOptionWidth - submenuSymbol ^. Element.width)
                    /|/ submenuSymbol
                    & Align.tValue %~ Hover.anchor
            hover <- Hover.hover
            addBg <- addBackground
            base
                & Align.tValue %~
                Hover.hoverInPlaceOf
                (Hover.hoverBesideOptionsAxis Glue.Horizontal
                 (Glue.vbox submenus & addBg <&> hover) base
                 <&> (^. Align.tValue))
                & pure
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = option ^. oId & Widget.toAnimId
        isSelected =
            Widget.isFocused (option ^. oWidget . Align.tValue)
            || Lens.anyOf (oSubmenuWidgets . traverse . Align.tValue)
               Widget.isFocused option

layout ::
    ( MonadReader env m, TextView.HasStyle env, Hover.HasStyle env
    , Element.HasAnimIdPrefix env, HasStyle env
    , Applicative f
    ) =>
    Widget.R -> [Option f] -> HasMoreOptions ->
    m (OrderedOptions (Widget (f Widget.EventResult)))
layout minWidth options hiddenResults =
    (addBackground <&> fmap) <*>
    case options of
    [] -> makeNoResults <&> (^. Align.tValue) <&> Widget.fromView <&> pure
    _:_ ->
        do
            submenuSymbolWidth <-
                TextView.drawText ?? submenuSymbolText
                <&> (^. TextView.renderedTextSize . TextView.bounding . _1)
            let optionMinWidth option =
                    option ^. oWidget . Element.width +
                    case option ^. oSubmenuWidgets of
                    [] -> 0
                    _:_ -> submenuSymbolWidth
            let maxOptionWidth = options <&> optionMinWidth & maximum & max minWidth
            hiddenOptionsWidget <-
                makeMoreOptionsView hiddenResults
                <&> (^. Align.tValue) <&> Widget.fromView
            laidOutOptions <-
                traverse (layoutOption maxOptionWidth) options
                <&> map (^. Align.tValue)
            blockEvents <*>
                ( OrderedOptions
                    { _optionsFromTop = id
                    , _optionsFromBottom = reverse
                    } ?? (laidOutOptions ++ [hiddenOptionsWidget])
                    <&> Glue.vbox
                ) & pure
