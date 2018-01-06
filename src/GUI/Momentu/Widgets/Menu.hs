{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, DeriveTraversable, FlexibleContexts, DisambiguateRecordFields #-}

module GUI.Momentu.Widgets.Menu
    ( Style(..), Keys(..), Config(..), HasConfig(..)
    , Submenu(..), _SubmenuEmpty, _SubmenuItems
    , OptionList(..), olOptions, olIsTruncated
    , PickResult(..), pickDest, pickDestIsEntryPoint
    , RenderedOption(..), rWidget, rPick
    , Option(..), oId, oRender, oSubmenuWidgets
    , optionWidgets
    , Placement(..)
    , make, makeHovered, hoverOptions, makePickEventMap
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           GUI.Momentu.Align (WithTextPos, Aligned(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/), (/-/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey)
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

data Keys = Keys
    { keysPickOption :: [MetaKey]
        -- ^ Pick option and stay on its dest
    , keysPickOptionAndGotoNext :: [MetaKey]
        -- ^ Pick option and goto the next "entry point" (see below)
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Keys

class HasConfig env where config :: Lens' env Config
instance HasConfig Config where config = id

data Config = Config
    { configStyle :: Style
    , configKeys :: Keys
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Config

-- | Menu supports picking results and setting cursor directly to it
-- (return), or picking and going to the next "entry point" (space).
--
-- When going to the next entry point, we want to go to an entry point
-- inside the picked result if it exists, or to an "outer" entry point
-- if it doesn't (and the "outer" entry point exists).
--
-- Thus, a PickedResult signifies whether the destination is an entry
-- point.
data PickResult = PickResult
    { _pickDest :: Widget.Id
    , _pickDestIsEntryPoint :: Bool
    }
Lens.makeLenses ''PickResult

data RenderedOption f = RenderedOption
    { _rWidget :: WithTextPos (Widget (f State.Update))
    , _rPick :: Widget.PreEvent (f PickResult)
    }
Lens.makeLenses ''RenderedOption

data Submenu m f
    = SubmenuEmpty
    | SubmenuItems (m [Option m f])

-- | Option record and cursor behavior
--
-- The search menu is expected to keep the cursor on the search results,
-- even as those "disappear".
--
-- To do this, the search menu must:
--
-- * Check if the cursor is on any of the menu options.
--
--   This requires knowing all the option widget ids
--
-- * Check if the cursor WAS on any menu option (that no longer exists)
--
--   This requires all options to have a common widget prefix that can be
--   identified even as options disappear
--
-- * If the cursor was on a result, but is not on a currently existing
--   result, we need to assign the cursor to any result (e.g: the first
--   one).
--
--   This requires the user's generated widgets, which *depend* on the
--   cursor, to be created AFTER we've made the above decision.
--
-- From all of the above, we can deduce that we must know all of the menu
-- options ids before generating any of the option widgets. Then we can
-- decide where the cursor is, then we can generate the widgets.
--
-- This is why each Option must expose the widget id and a function to
-- *generate* an option widget, that depends on a cursor computed by
-- looking at all of the option ids.

data Option m f = Option
    { -- | Must be the prefix of all both the menu option and its submenu options,
      --  also used to create this option's submenu arrow frame:
      _oId :: !Widget.Id
    , -- A widget that represents this option
      _oRender :: m (RenderedOption f)
    , -- An optionally empty submenu
      _oSubmenuWidgets :: !(Submenu m f)
    }

Lens.makePrisms ''Submenu
Lens.makeLenses ''Option

optionWidgets ::
    Functor m => Lens.Setter' (Option m f) (WithTextPos (Widget (f State.Update)))
optionWidgets f (Option i w s) =
    Option i <$> (Lens.mapped . rWidget) f w <*> (_SubmenuItems . Lens.mapped . Lens.mapped . optionWidgets) f s

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
    ( MonadReader env m, HasConfig env, Element.HasAnimIdPrefix env
    , TextView.HasStyle env
    ) =>
    Bool -> m (WithTextPos View)
makeSubmenuSymbol isSelected =
    do
        color <- Lens.view config <&> configStyle <&> submenuSymbolColor
        TextView.makeLabel submenuSymbolText
            & Reader.local (TextView.color .~ color)
    where
        submenuSymbolColor
            | isSelected = submenuSymbolColorSelected
            | otherwise = submenuSymbolColorUnselected

data OptionList a = OptionList
    { _olOptions :: [a]
    , _olIsTruncated :: Bool
        -- ^ more hidden options exist (relevant for search menus)
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''OptionList

layoutOption ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, TextView.HasStyle env
    , State.HasCursor env, Hover.HasStyle env, HasConfig env, Applicative f
    ) =>
    Widget.R ->
    Maybe Widget.Id ->
    (Widget.Id, WithTextPos (Widget (f State.Update)), Submenu m f) ->
    m (WithTextPos (Widget (f State.Update)))
layoutOption maxOptionWidth mNextEntry (optionId, rendered, submenu) =
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
                    (_, submenus) <- action <&> (`OptionList` False) >>= make 0 mNextEntry
                    let anchored = base & Align.tValue %~ Hover.anchor
                    anchored
                        & Align.tValue %~
                        Hover.hoverInPlaceOf
                        (Hover.hoverBesideOptionsAxis Glue.Horizontal
                         (submenus <&> hover <&> Align.WithTextPos 0) anchored <&> (^. Align.tValue))
                        & pure
                else pure base
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId optionId)

instance Monoid (OptionList a) where
    mempty = OptionList [] False
    OptionList o0 t0 `mappend` OptionList o1 t1 =
        OptionList (o0 <> o1) (t0 || t1)

makePickEventMap ::
    (MonadReader env m, HasConfig env, Applicative f) =>
    Maybe Widget.Id ->
    m (Widget.PreEvent (f PickResult) -> EventMap (f State.Update))
makePickEventMap mNextEntry =
    Lens.view config <&> configKeys
    <&>
    \keys pick ->
    let pickAndJumpEventMap =
            case mNextEntry of
            Nothing -> mempty
            Just nextEntry ->
                E.keysEventMapMovesCursor (keysPickOptionAndGotoNext keys)
                (E.Doc [pick ^. Widget.pDesc <> ", Next entry"]) (pick ^. Widget.pAction <&> pickEntry)
                where
                    pickEntry pickResult
                        | pickResult ^. pickDestIsEntryPoint = pickResult ^. pickDest
                        | otherwise = nextEntry
    in
    pickAndJumpEventMap
    <>
    E.keysEventMapMovesCursor (mappend keysPickOption keysPickOptionAndGotoNext keys)
    (E.Doc [pick ^. Widget.pDesc]) (pick ^. Widget.pAction <&> (^. pickDest))

addPickers ::
    (MonadReader env m, HasConfig env, Applicative f) =>
    Maybe Widget.Id ->
    m ( Widget.PreEvent (f PickResult) ->
        Widget (f State.Update) ->
        Widget (f State.Update)
      )
addPickers mNextEntry =
    makePickEventMap mNextEntry
    <&>
    \pickEventMap pick w ->
    let preEvent =
            pick
            <&> fmap (^. pickDest)
            <&> fmap State.updateCursor
    in
    w
    & Widget.addPreEventWith (liftA2 mappend) preEvent
    & Widget.eventMapMaker . Lens.mapped %~ mappend (pickEventMap pick)

make ::
    ( MonadReader env m, TextView.HasStyle env, Hover.HasStyle env
    , Element.HasAnimIdPrefix env, HasConfig env, State.HasCursor env
    , Applicative f
    ) =>
    Widget.R -> Maybe Widget.Id -> OptionList (Option m f) ->
    m (Maybe (Widget.PreEvent (f PickResult)), Hover.Ordered (Widget (f State.Update)))
make minWidth mNextEntry options =
    case options ^. olOptions of
    [] -> makeNoResults <&> (^. Align.tValue) <&> Widget.fromView <&> pure <&> (,) Nothing
    opts@(_:_) ->
        do
            submenuSymbolWidth <-
                TextView.drawText ?? submenuSymbolText
                <&> (^. TextView.renderedTextSize . TextView.bounding . _1)
            let optionMinWidth (_, (_, w, submenu)) =
                    w ^. Element.width +
                    case submenu of
                    SubmenuEmpty -> 0
                    SubmenuItems {} -> submenuSymbolWidth
            addPick <- addPickers mNextEntry
            let render (Option optionId optRender submenu) =
                    optRender <&>
                    \r -> (r ^. rPick, (optionId, r ^. rWidget <&> addPick (r ^. rPick), submenu))
            rendered <- traverse render opts
            let mPickFirstResult = rendered ^? Lens.ix 0 . _1
            let maxOptionWidth = rendered <&> optionMinWidth & maximum & max minWidth
            laidOutOptions <-
                rendered
                <&> snd
                & traverse (layoutOption maxOptionWidth mNextEntry)
                <&> map (^. Align.tValue)
            hiddenOptionsWidget <-
                if options ^. olIsTruncated
                then TextView.makeLabel "..." <&> (^. Align.tValue) <&> Widget.fromView
                else pure Element.empty
            pure
                ( mPickFirstResult
                , blockEvents <*>
                    ( Hover.Ordered
                        { _forward = id
                        , _backward = reverse
                        } ?? (laidOutOptions ++ [hiddenOptionsWidget])
                        <&> Glue.vbox
                    )
                )

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
    ( Applicative f, State.HasCursor env, HasConfig env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    , Hover.HasStyle env, MonadReader env m
    ) =>
    View ->
    Maybe Widget.Id ->
    OptionList (Option m f) ->
    m (Placement -> Widget (f State.Update) -> Widget (f State.Update))
makeHovered annotation mNextEntry options =
    do
        mkHoverOptions <- hoverOptions
        (_, menu) <- make (annotation ^. Element.width) mNextEntry options
        pure $
            \placement term ->
            let a = Hover.anchor term
            in  a
                & Hover.hoverInPlaceOf (mkHoverOptions placement annotation menu a)
