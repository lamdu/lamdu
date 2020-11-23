{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields, ConstraintKinds #-}

module GUI.Momentu.Widgets.Menu
    ( Style(..), submenuSymbolColorUnselected, submenuSymbolColorSelected
    , Keys(..), keysPickOption, keysPickOptionAndGotoNext
    , Config(..), configStyle, configKeys
    , configLens
    , Submenu(..), _SubmenuEmpty, _SubmenuItems
    , OptionList(..), _TooMany, _FullList, _Truncated
        , olOptions, olIsTruncated, toOptionList
    , PickResult(..), pickDest, pickMNextEntry
    , PickFirstResult(..)
    , RenderedOption(..), rWidget, rPick
    , Option(..), oId, oRender, oSubmenuWidgets
    , optionWidgets
    , Placement(..)
    , make, makeHovered, hoverOptions, makePickEventMap
    , noResultsId
    , HasTexts, Texts(..), noResults
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos, TextWidget, Aligned(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Direction (Orientation(..))
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Hover (Hover)
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _noResults :: a
    , _downBlocked :: a
    , _upBlocked :: a
    , _submenuSymbol :: a
    , _commaNextEntry :: a
    } deriving Eq

Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts

data Style = Style
    { _submenuSymbolColorUnselected :: Draw.Color
    , _submenuSymbolColorSelected :: Draw.Color
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Style

Lens.makeLenses ''Style

data Keys = Keys
    { _keysPickOption :: [MetaKey]
        -- ^ Pick option and stay on its dest
    , _keysPickOptionAndGotoNext :: [MetaKey]
        -- ^ Pick option and goto the next "entry point" (see below)
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Keys

Lens.makeLenses ''Keys

data Config = Config
    { _configStyle :: Style
    , _configKeys :: Keys
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Config

Lens.makeLenses ''Config

configLens ::
    Functor f =>
    Lens.ALens' env Keys -> Lens.ALens' env Style -> Lens.LensLike' f env Config
configLens keys style f env =
    f Config
    { _configKeys = env ^# keys
    , _configStyle = env ^# style
    }
    <&>
    \(Config newStyle newKeys) -> env & keys #~ newKeys & style #~ newStyle


-- | Menu supports picking results and setting cursor directly to it
-- (return), or picking it and strolling (space).
-- When pickMNextEntry has a value, strolling would go to that value
-- rather than the normal stroll.
data PickResult = PickResult
    { _pickDest :: Widget.Id
    , _pickMNextEntry :: Maybe Widget.Id
    }
Lens.makeLenses ''PickResult

data PickFirstResult f
    = NoPickFirstResult
    | PickFirstResult (Widget.PreEvent (f PickResult))

data RenderedOption f = RenderedOption
    { _rWidget :: TextWidget f
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
    Functor m => Lens.Setter' (Option m f) (TextWidget f)
optionWidgets f (Option i w s) =
    Option i
    <$> (Lens.mapped . rWidget) f w
    <*> (_SubmenuItems . Lens.mapped . Lens.mapped . optionWidgets) f s

type HasTexts env = (Has (Texts Text) env, Has Dir.Layout env)

makeNoResults ::
    ( MonadReader env m
    , Element.HasAnimIdPrefix env
    , Has TextView.Style env
    , HasTexts env
    ) =>
    m (WithTextPos View)
makeNoResults =
    TextView.make
    <*> Lens.view (has . noResults)
    <*> (Element.subAnimId ?? ["no results"])

blockEvents ::
    ( Applicative f
    , Has (Texts Text) env
    , Has (MomentuTexts.Texts Text) env
    ) =>
    env -> Hover.Ordered (Widget f -> Widget f)
blockEvents env =
    Hover.Ordered
    { _forward = blockDirection MetaKey.Key'Down downBlocked
    , _backward = blockDirection MetaKey.Key'Up upBlocked
    }
    where
        doc keyLens =
            E.toDoc env
            [ has . MomentuTexts.navigation
            , has . MomentuTexts.move
            , has . keyLens
            ]
        blockDirection key keyName =
            Widget.eventMapMaker . Lens.mapped <>~
            E.keyPresses [ModKey mempty key] (doc keyName) (pure mempty)

makeSubmenuSymbol ::
    ( MonadReader env m, Has Config env, Element.HasAnimIdPrefix env
    , Has TextView.Style env, HasTexts env
    ) =>
    Bool -> m (WithTextPos View)
makeSubmenuSymbol isSelected =
    do
        color <- Lens.view (has . configStyle . submenuSymbolColor)
        TextView.make
            <*> Lens.view (has . submenuSymbol)
            <*> (Element.subAnimId ?? ["submenu sym"])
            & Reader.local (TextView.color .~ color)
    where
        submenuSymbolColor
            | isSelected = submenuSymbolColorSelected
            | otherwise = submenuSymbolColorUnselected

data OptionList a
    = TooMany -- e.g: Null search term
    | FullList [a]
    | Truncated [a]
    deriving (Functor, Foldable, Traversable)
Lens.makePrisms ''OptionList

olOptions :: OptionList a -> [a]
olOptions TooMany = []
olOptions (FullList xs) = xs
olOptions (Truncated xs) = xs

olIsTruncated :: OptionList a -> Bool
olIsTruncated Truncated {} = True
olIsTruncated _ = False

toOptionList :: [a] -> Bool -> OptionList a
toOptionList xs False = FullList xs
toOptionList xs True = Truncated xs

layoutOption ::
    ( MonadReader env m, Applicative f, Has (Texts Text) env
    , Element.HasAnimIdPrefix env, Has TextView.Style env
    , State.HasCursor env, Has Hover.Style env, Has Config env
    , Glue.HasTexts env
    ) =>
    Widget.R ->
    (Widget.Id, TextWidget f, Submenu m f) ->
    m (TextWidget f)
layoutOption maxOptionWidth (optionId, rendered, submenu) =
    case submenu of
    SubmenuEmpty -> padToWidth maxOptionWidth rendered
    SubmenuItems action ->
        do
            isSelected <- State.isSubCursor ?? optionId
            submenuSym <- makeSubmenuSymbol isSelected
            base <-
                padToWidth (maxOptionWidth - submenuSym ^. Element.width)
                rendered
                /|/ pure submenuSym
            if isSelected
                then do
                    hover <- Hover.hover
                    anc <- Hover.anchor
                    hoverBeside <- Hover.hoverBesideOptionsAxis
                    (_, submenus) <-
                        action <&> FullList
                        >>= make (optionId `Widget.joinId` ["submenu"]) 0
                    let anchored = base & Align.tValue %~ anc
                    anchored
                        & Align.tValue %~
                        Hover.hoverInPlaceOf
                        (hoverBeside Horizontal
                         (submenus <&> hover <&> Hover.sequenceHover) anchored <&> (^. Align.tValue))
                        & pure
                else pure base
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId optionId)
    where
        padToWidth w r = Element.padToSize ?? Vector2 w 0 ?? 0 ?? r

instance Semigroup (OptionList a) where
    TooMany <> y = y
    x <> TooMany = x
    Truncated xs <> y = Truncated (xs ++ olOptions y)
    x <> Truncated ys = Truncated (olOptions x ++ ys)
    FullList xs <> FullList ys = FullList (xs ++ ys)

makePickEventMap ::
    (MonadReader env m, Has Config env, Has (Texts Text) env, Applicative f) =>
    m (Widget.PreEvent (f PickResult) -> EventMap (f State.Update))
makePickEventMap =
    Lens.view id
    <&>
    \env pick ->
    let keys = env ^. has . configKeys
    in  E.keyPresses (keys ^. keysPickOptionAndGotoNext <&> MetaKey.toModKey)
        (E.Doc [pick ^. Widget.pDesc <> env ^. has . commaNextEntry])
        (pick ^. Widget.pAction <&>
            \result ->
            case result ^. pickMNextEntry of
            Just nextEntry -> State.updateCursor nextEntry
            Nothing ->
                State.updateCursor (result ^. pickDest)
                & State.uPreferStroll .~ True ^. Lens._Unwrapped
            )
        <>
        E.keysEventMapMovesCursor (keys ^. keysPickOption)
        (E.Doc [pick ^. Widget.pDesc])
        (pick ^. Widget.pAction <&> (^. pickDest))

addPickers ::
    (MonadReader env m, Has Config env, Applicative f, Has (Texts Text) env) =>
    m ( Widget.PreEvent (f PickResult) ->
        Widget f ->
        Widget f
      )
addPickers =
    makePickEventMap
    <&>
    \pickEventMap pick w ->
    let preEvent =
            pick
            <&> fmap (^. pickDest)
            <&> fmap State.updateCursor
    in
    w
    & Widget.addPreEvent preEvent
    & Widget.eventMapMaker . Lens.mapped %~ mappend (pickEventMap pick)

noResultsId :: Widget.Id -> Widget.Id
noResultsId = (`Widget.joinId` ["no results"])

make ::
    ( MonadReader env m, Applicative f, Has TextView.Style env
    , Has Hover.Style env, Element.HasAnimIdPrefix env, Has Config env
    , State.HasCursor env, Has (Texts Text) env, Glue.HasTexts env
    ) =>
    Widget.Id -> Widget.R -> OptionList (Option m f) ->
    m (PickFirstResult f, Hover.Ordered (TextWidget f))
make myId minWidth options =
    case options of
    TooMany -> pure (NoPickFirstResult, pure Element.empty)
    Truncated [] -> error "empty truncated list of options is not supported"
    FullList [] ->
        (Widget.makeFocusableView ?? noResultsId myId <&> (Align.tValue %~))
        <*> makeNoResults
        <&> pure
        <&> (,) NoPickFirstResult
    Truncated opts -> makeOpts opts
    FullList opts -> makeOpts opts
    where
        makeOpts opts =
            do
                submenuSymbolWidth <-
                    TextView.drawText <*> Lens.view (has . submenuSymbol)
                    <&> (^. TextView.renderedTextSize . TextView.bounding . _1)
                let optionMinWidth (_, (_, w, submenu)) =
                        w ^. Element.width +
                        case submenu of
                        SubmenuEmpty -> 0
                        SubmenuItems {} -> submenuSymbolWidth
                addPick <- addPickers
                let render (Option optionId optRender submenu) =
                        optRender <&>
                        \r -> (r ^. rPick, (optionId, r ^. rWidget <&> addPick (r ^. rPick), submenu))
                rendered <- traverse render opts
                let mPickFirstResult = rendered ^? Lens.ix 0 . _1
                let maxOptionWidth = rendered <&> optionMinWidth & maximum & max minWidth
                laidOutOptions <-
                    rendered
                    <&> snd
                    & traverse (layoutOption maxOptionWidth)
                hiddenOptionsWidget <-
                    if olIsTruncated options
                    then Label.make "..." <&> Align.tValue %~ Widget.fromView
                    else pure Element.empty
                vbox <- Glue.vbox
                env <- Lens.view id
                pure
                    ( maybe NoPickFirstResult PickFirstResult mPickFirstResult
                    , (blockEvents env <&> (Align.tValue %~)) <*>
                        ( Hover.Ordered
                            { _forward = id
                            , _backward = reverse
                            } ?? laidOutOptions ++ [hiddenOptionsWidget]
                            <&> vbox
                        )
                    )

-- | You may want to limit the placement of hovering pop-up menus,
-- so that they don't cover other ui elements.
data Placement = Above | Below | AnyPlace

hoverOptions ::
    ( MonadReader env m, Applicative f, Has Hover.Style env
    , Element.HasAnimIdPrefix env, Glue.HasTexts env
    ) =>
    m ( Placement ->
        View ->
        Hover.Ordered (TextWidget f) ->
        Hover.AnchoredWidget f ->
        [Hover (Hover.AnchoredWidget f)]
      )
hoverOptions =
    (,,) <$> (Glue.mkPoly ?? Glue.Horizontal) <*> (Glue.mkPoly ?? Glue.Vertical) <*> Hover.hover
    <&> \(Glue.Poly (|||), Glue.Poly (|---|), hover) pos ann results searchTerm ->
    let resultsAbove alignment =
            results ^. Hover.backward
            & Align.tValue %~ hover
            & Align.fromWithTextPos alignment
        annotatedTerm alignment = searchTerm |---| ann & Aligned alignment
        aboveRight = resultsAbove 0 |---| annotatedTerm 0
        aboveLeft = resultsAbove 1 |---| annotatedTerm 1
        annotatedResultsBelow =
            (results ^. Hover.forward) |---| ann
            & Align.tValue %~ hover
        belowRight =
            Aligned 0 searchTerm
            |---|
            Align.fromWithTextPos 0 annotatedResultsBelow
        belowLeft =
            Aligned 1 searchTerm
            |---|
            Align.fromWithTextPos 1 annotatedResultsBelow
        rightAbove = annotatedTerm 1 ||| resultsAbove 1
        leftAbove = resultsAbove 1 ||| annotatedTerm 1
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
        ]
    Below ->
        [ belowRight
        , belowLeft
        , rightAbove
        , leftAbove
        ]
    <&> (^. Align.value)

makeHovered ::
    ( Applicative f, State.HasCursor env, Has Config env
    , Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has Hover.Style env, Has (Texts Text) env, MonadReader env m
    , Glue.HasTexts env
    ) =>
    Widget.Id -> View ->
    OptionList (Option m f) ->
    m
    ( PickFirstResult f
    , Placement -> Widget f -> Widget f
    )
makeHovered myId ann options =
    do
        mkHoverOptions <- hoverOptions
        anc <- Hover.anchor
        make myId (ann ^. Element.width) options
            <&> _2 %~ \menu placement term ->
                let a = anc term
                in
                Hover.hoverInPlaceOf (mkHoverOptions placement ann menu a) a
