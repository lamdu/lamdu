{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, DeriveTraversable, FlexibleContexts, DisambiguateRecordFields #-}

module GUI.Momentu.Widgets.Menu
    ( Style(..), HasStyle(..)
    , Submenu(..), _SubmenuEmpty, _SubmenuItems
    , OptionList(..), olOptions, olIsTruncated
    , Option(..), oId, oWidget, oSubmenuWidgets
    , optionWidgets
    , Placement(..)
    , make, makeHovered
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           GUI.Momentu.Align (WithTextPos, Aligned(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/), (/-/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Style = Style
    { submenuSymbolColorUnselected :: Draw.Color
    , submenuSymbolColorSelected :: Draw.Color
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

data Submenu f a
    = SubmenuEmpty
    | SubmenuItems (f [WithTextPos (Widget a)])
    deriving (Functor)
Lens.makePrisms ''Submenu

data Option f a = Option
    { -- | Must be the prefix of all both the menu option and its submenu options,
      --  also used to create this option's submenu arrow frame:
      _oId :: !Widget.Id
    , -- A widget that represents this option
      _oWidget :: f (WithTextPos (Widget a))
    , -- An optionally empty submenu
      _oSubmenuWidgets :: !(Submenu f a)
    }
Lens.makeLenses ''Option

optionWidgets :: Functor f => Lens.Setter (Option f a) (Option f b) (WithTextPos (Widget a)) (WithTextPos (Widget b))
optionWidgets f (Option i w s) =
    Option i <$> Lens.mapped f w <*> (_SubmenuItems . Lens.mapped . Lens.mapped) f s

makeNoResults ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    m (WithTextPos View)
makeNoResults = TextView.makeLabel "(No results)"

blockEvents ::
    Applicative f =>
    Hover.Ordered (Widget (f State.Update) -> Widget (f State.Update))
blockEvents =
    Hover.Ordered
    { _forward = blockDirection MetaKey.Key'Down "down"
    , _backward = blockDirection MetaKey.Key'Up "up"
    }
    where
        doc keyName = E.Doc ["Navigation", "Move", keyName <> " (blocked)"]
        blockDirection key keyName =
            Widget.eventMapMaker . Lens.mapped <>~
            E.keyPresses [ModKey mempty key] (doc keyName) (pure mempty)

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

layoutOption ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    , State.HasCursor env, Hover.HasStyle env, HasStyle env, Functor f
    ) =>
    Widget.R -> (Widget.Id, WithTextPos (Widget (f State.Update)), Submenu m (f State.Update)) ->
    m (WithTextPos (Widget (f State.Update)))
layoutOption maxOptionWidth (optionId, rendered, submenu) =
    case submenu of
    SubmenuEmpty -> rendered & Element.width .~ maxOptionWidth & pure
    SubmenuItems action ->
        do
            isSelected <- State.isSubCursor ?? optionId
            submenuSymbol <- makeSubmenuSymbol isSelected
            let base =
                    (rendered & Element.width .~ maxOptionWidth - submenuSymbol ^. Element.width)
                    /|/ submenuSymbol
            if isSelected
                then do
                    hover <- Hover.hover
                    submenus <- action
                    let anchored = base & Align.tValue %~ Hover.anchor
                    anchored
                        & Align.tValue %~
                        Hover.hoverInPlaceOf
                        (Hover.hoverBesideOptionsAxis Glue.Horizontal
                         Hover.Ordered
                         { _forward = Glue.vbox submenus <&> hover
                         , _backward = Glue.vbox submenus <&> hover
                         } anchored <&> (^. Align.tValue))
                        & pure
                else pure base
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId optionId)

data OptionList a = OptionList
    { _olOptions :: [a]
    , _olIsTruncated :: Bool
        -- ^ more hidden options exist (relevant for search menus)
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''OptionList

instance Monoid (OptionList a) where
    mempty = OptionList [] False
    OptionList o0 t0 `mappend` OptionList o1 t1 =
        OptionList (o0 <> o1) (t0 || t1)

make ::
    ( MonadReader env m, TextView.HasStyle env, Hover.HasStyle env
    , Element.HasAnimIdPrefix env, HasStyle env, State.HasCursor env
    , Applicative f
    ) =>
    Widget.R -> OptionList (Option m (f State.Update)) ->
    m (Hover.Ordered (Widget (f State.Update)))
make minWidth options =
    case options ^. olOptions of
    [] -> makeNoResults <&> (^. Align.tValue) <&> Widget.fromView <&> pure
    opts@(_:_) ->
        do
            submenuSymbolWidth <-
                TextView.drawText ?? submenuSymbolText
                <&> (^. TextView.renderedTextSize . TextView.bounding . _1)
            let optionMinWidth (_, w, submenu) =
                    w ^. Element.width +
                    case submenu of
                    SubmenuEmpty -> 0
                    SubmenuItems {} -> submenuSymbolWidth
            rendered <- traverse render opts
            let maxOptionWidth = rendered <&> optionMinWidth & maximum & max minWidth
            laidOutOptions <-
                traverse (layoutOption maxOptionWidth) rendered
                <&> map (^. Align.tValue)
            hiddenOptionsWidget <-
                if options ^. olIsTruncated
                then TextView.makeLabel "..." <&> (^. Align.tValue) <&> Widget.fromView
                else pure Element.empty
            blockEvents <*>
                ( Hover.Ordered
                    { _forward = id
                    , _backward = reverse
                    } ?? (laidOutOptions ++ [hiddenOptionsWidget])
                    <&> Glue.vbox
                ) & pure
    where
        render (Option optionId mkWidget submenu) =
            mkWidget <&> ((,,) optionId ?? submenu)

-- | You may want to limit the placement of hovering pop-up menus,
-- so that they don't cover other ui elements.
data Placement = Above | Below | AnyPlace

hoverOptions ::
    ( MonadReader env m, Hover.HasStyle env, Element.HasAnimIdPrefix env
    , Functor f
    ) =>
    m ( Placement ->
        View ->
        Hover.Ordered (Widget (f State.Update)) ->
        Hover.AnchoredWidget (f State.Update) ->
        [Hover.AnchoredWidget (f State.Update)]
      )
hoverOptions =
    Hover.hover <&>
    \hover pos annotation results searchTerm ->
    let resultsAbove alignment =
            results ^. Hover.backward & hover & Aligned alignment
        annotatedTerm alignment = searchTerm & Widget.widget %~ (/-/ annotation) & Aligned alignment
        aboveRight = resultsAbove 0 /-/ annotatedTerm 0
        aboveLeft =
            resultsAbove 1
            /-/ annotatedTerm 1
        annotatedResultsBelow = (results ^. Hover.forward) /-/ annotation & hover
        resultsBelow = results ^. Hover.forward & hover
        belowRight =
            Aligned 0 searchTerm
            /-/
            Aligned 0 annotatedResultsBelow
        belowLeft =
            Aligned 1 searchTerm
            /-/
            Aligned 1 annotatedResultsBelow
        centerRight = annotatedTerm 0.5 /|/ Aligned 0.5 resultsBelow
        rightAbove = annotatedTerm 1 /|/ resultsAbove 1
        leftAbove = resultsAbove 1 /|/ annotatedTerm 1
    in
    case pos of
    Above ->
        [ aboveRight
        , aboveLeft
        ]
    AnyPlace ->
        [ belowRight
        , aboveRight
        , belowLeft
        , aboveLeft
        , centerRight
        ]
    Below ->
        [ belowRight
        , belowLeft
        , rightAbove
        , leftAbove
        ]
    <&> (^. Align.value)

makeHovered ::
    ( Applicative f, State.HasCursor env, HasStyle env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    , Hover.HasStyle env, MonadReader env m
    ) =>
    View -> OptionList (Option m (f State.Update)) ->
    m (Placement -> Widget (f State.Update) -> Widget (f State.Update))
makeHovered annotation options =
    do
        mkHoverOptions <- hoverOptions
        menu <- make (annotation ^. Element.width) options
        pure $
            \placement term ->
            let a = Hover.anchor term
            in  a
                & Hover.hoverInPlaceOf (mkHoverOptions placement annotation menu a)
