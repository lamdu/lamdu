{-# LANGUAGE TemplateHaskell, DerivingVia, ConstraintKinds #-}

module GUI.Momentu.Widgets.Menu.Search
    ( emptyPickEventMap
    , resultsIdPrefix
    , ResultsContext(..), rSearchTerm, rResultIdPrefix

    , basicSearchTermEdit
    , defaultEmptyStrings
    , addPickFirstResultEvent
    , addSearchTermBgColor, addSearchTermEmptyColors
    , addSearchTermStyle
    , addDelSearchTerm
    , searchTermEdit

    , TermStyle(..), bgColors, emptyStrings, emptyStringsColors
    , enterWithSearchTerm
    , Term(..), termWidget, termEditEventMap

    , TermCtx(..), tcTextEdit, tcAdHoc
    , AllowedSearchTerm

    , make

    -- temporary exports that will be removed when transition of HoleEdit
    -- to Menu.Search is complete
    , readSearchTerm

    , Texts(..)
        , textPickNotApplicable, textSearchTerm
        , textAppendChar, textDeleteBackwards
    , HasTexts
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified Data.Text as Text
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.I18N as Texts
import           GUI.Momentu.MetaKey (MetaKey(..), toModKey, noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import           GUI.Momentu.State (HasState)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widget.Id (Id(..), joinId)
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _textPickNotApplicable :: a
    , _textSearchTerm :: a
    , _textAppendChar :: a
    , _textDeleteBackwards :: a
    } deriving Eq

Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_text" ''Texts

type HasTexts env =
    ( Has (Menu.Texts Text) env, Has (Texts Text) env
    , Has (Texts.Texts Text) env
    )

-- | Context necessary for creation of menu items for a search.
data ResultsContext = ResultsContext
    { _rSearchTerm :: Text
    , _rResultIdPrefix :: Id
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''ResultsContext

data TermStyle = TermStyle
    { _bgColors :: TextEdit.Modes Draw.Color
    , _emptyStrings :: TextEdit.EmptyStrings
    , _emptyStringsColors :: TextEdit.Modes Draw.Color
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_"''TermStyle

Lens.makeLenses ''TermStyle

data Term f = Term
    { _termWidget :: TextWidget f
    , _termEditEventMap :: EventMap (f State.Update)
    }
Lens.makeLenses ''Term

data TermCtx a = TermCtx
    { _tcTextEdit :: a
    , _tcAdHoc :: a
    } deriving stock (Generic, Generic1, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 TermCtx
Lens.makeLenses ''TermCtx

-- | Whether search term is allowed in each search term context:
type AllowedSearchTerm = Text -> TermCtx Bool

emptyPickEventMap ::
    ( MonadReader env m, Has Menu.Config env, Has (Texts Text) env
    , Applicative f
    ) =>
    m (EventMap (f State.Update))
emptyPickEventMap =
    Lens.view id
    <&> \env ->
    E.keysEventMap (allPickKeys env)
    (E.toDoc env [has . textPickNotApplicable]) (pure ())
    where
        allPickKeys env =
            keys ^. Menu.keysPickOption <>
            keys ^. Menu.keysPickOptionAndGotoNext
            where
                keys = env ^. has . Menu.configKeys

-- | All search menu results must start with a common prefix.
-- This is used to tell when cursor was on a result that got filtered out
-- when the search term changed in order to redirect it to a result.
resultsIdPrefix :: Id -> Id
resultsIdPrefix = (`joinId` ["Results"])

searchTermEditId :: Id -> Id
searchTermEditId = (`joinId` ["SearchTerm"])

readSearchTerm :: (MonadReader env m, HasState env) => Id -> m Text
readSearchTerm x = State.readWidgetState x <&> fromMaybe ""

defaultEmptyStrings :: TextEdit.EmptyStrings
defaultEmptyStrings = TextEdit.Modes "  " "  "

-- | Basic search term edit:
--   * no bg color / no Has TermStyle needed --> use addSearchTermStyle
--     to add it
--   * no pick of first result / no Has Menu.Config needed --> use
--     addPickFirstResultEvent to add it
basicSearchTermEdit ::
    ( MonadReader env m, Applicative f, HasTexts env
    , TextEdit.Deps env, HasState env
    ) =>
    AnimId -> Id -> AllowedSearchTerm -> TextEdit.EmptyStrings -> m (Term f)
basicSearchTermEdit searchTermId holeId allowedSearchTerm textEditEmpty =
    do
        searchTerm <- readSearchTerm holeId
        let onEvents (newSearchTerm, eventRes)
                | newSearchTerm == searchTerm = eventRes
                | otherwise =
                    eventRes
                    <> State.updateWidgetState holeId newSearchTerm
                    -- When first letter is typed in search term, jump to the
                    -- results, which will go to first result:
                    & if Text.null searchTerm
                        then
                            State.uCursor .~
                            Just (resultsIdPrefix holeId) ^. Lens._Unwrapped
                        else id
        widget <-
            TextEdit.makeWithAnimId ?? textEditEmpty ?? searchTerm ?? searchTermId ?? searchTermEditId holeId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                E.filter (_tcTextEdit . allowedSearchTerm . fst)
            <&> Align.tValue . Widget.updates %~ pure . onEvents
            <&> Align.tValue %~ Widget.takesStroll holeId
        env <- Lens.view id
        pure Term
            { _termWidget = widget
            , _termEditEventMap =
                searchTermEditEventMap env holeId (_tcAdHoc . allowedSearchTerm) searchTerm
            }

searchTermDoc :: HasTexts env => env -> Lens.ALens' env Text -> E.Doc
searchTermDoc env subtitle =
    E.toDoc env [has . MomentuTexts.edit, has . textSearchTerm, subtitle]

addDelSearchTerm ::
    (MonadReader env m, State.HasState env, HasTexts env, Applicative f) =>
    Id -> m (Term f -> Term f)
addDelSearchTerm myId =
    Lens.view id
    <&>
    \env term ->
    let searchTerm = readSearchTerm myId env
        delSearchTerm
            | Text.null searchTerm = mempty
            | otherwise =
                enterWithSearchTerm "" myId & pure
                & E.keyPress (toModKey (MetaKey noMods MetaKey.Key'Escape))
                (searchTermDoc env (has . MomentuTexts.delete))
    in  term
        & termWidget . Align.tValue %~ Widget.weakerEventsWithoutPreevents delSearchTerm
        & termEditEventMap <>~ delSearchTerm

addSearchTermBgColor ::
    ( MonadReader env m, State.HasCursor env, Has TermStyle env, Functor f
    ) => Id -> m (Term f -> Term f)
addSearchTermBgColor myId =
    do
        isActive <- State.isSubCursor ?? myId
        bgColor <-
            Lens.view
            (has . bgColors .
                if isActive then TextEdit.focused else TextEdit.unfocused)
        termWidget %~ Draw.backgroundColor bgAnimId bgColor & pure
    where
        bgAnimId = Widget.toAnimId myId <> ["hover background"]

addSearchTermEmptyColors ::
    ( MonadReader env m, Has TermStyle env, Has TextEdit.Style env
    ) =>
    m a -> m a
addSearchTermEmptyColors act =
    do
        colors <- Lens.view (has . emptyStringsColors)
        Reader.local (has . TextEdit.sEmptyStringsColors .~ colors) act

addSearchTermStyle ::
    ( MonadReader env m, Has TermStyle env, Has TextEdit.Style env
    , Functor f, State.HasCursor env
    ) =>
    Widget.Id -> m (Term f) -> m (Term f)
addSearchTermStyle myId act =
    addSearchTermBgColor myId <*> act
    & addSearchTermEmptyColors

searchTermEdit ::
    ( MonadReader env m, Applicative f, Has TermStyle env
    , TextEdit.Deps env, Has Menu.Config env, State.HasState env
    , HasTexts env
    ) =>
    Widget.Id -> (Text -> TermCtx Bool) -> Menu.PickFirstResult f -> m (Term f)
searchTermEdit myId allowedSearchTerm mPickFirst =
    Lens.view (has . emptyStrings)
    >>= basicSearchTermEdit (searchTermEditId myId & Widget.toAnimId) myId allowedSearchTerm
    & (addDelSearchTerm myId <*>)
    & addSearchTermStyle myId
    & (addPickFirstResultEvent myId mPickFirst <*>)

-- Add events on search term to pick the first result.
addPickFirstResultEvent ::
    ( MonadReader env m, Has Menu.Config env, HasTexts env, HasState env
    , Applicative f
    ) =>
    Id -> Menu.PickFirstResult f->
    m (Term f -> Term f)
addPickFirstResultEvent myId mPickFirst =
    do
        pickEventMap <-
            case mPickFirst of
            Menu.NoPickFirstResult -> emptyPickEventMap
            Menu.PickFirstResult pickFirst -> Menu.makePickEventMap ?? pickFirst
        searchTerm <- readSearchTerm myId
        pure $ termWidget . Align.tValue %~
            if Text.null searchTerm
            then Widget.weakerEvents pickEventMap
            else addPre . Widget.weakerEvents pickEventMap
    where
        addPre =
            Widget.wState . Widget._StateFocused . Lens.mapped .
            Widget.fPreEvents %~ (Widget.BlockEvents <>)

assignCursor ::
    (MonadReader env m, HasState env) =>
    Id -> [Id] -> m a -> m a
assignCursor myId resultIds action =
    do
        searchTerm <- readSearchTerm myId
        let destId
                | Text.null searchTerm =
                    -- When entering a hole with an empty search string
                    -- (Like after typing "factorial x="),
                    -- cursor should be on the search-string and not on a result
                    -- so that operators pressed will set the search string
                    -- rather than apply on the first result.
                    searchTermEditId myId
                | otherwise =
                      resultIds ^? Lens.traverse
                      & fromMaybe (Menu.noResultsId myId)

        -- Results appear and disappear when the search-string changes,
        -- but the cursor prefix signifies whether we should be on a result.
        -- When that is the case but is not currently on any of the existing results
        -- the cursor will be sent to the default one.
        shouldBeOnResult <- sub (resultsIdPrefix myId)
        isOnResult <- traverse sub resultIds <&> or

        action
            & if shouldBeOnResult && not isOnResult
            then Reader.local (State.cursor .~ destId)
            else State.assignCursor myId destId
    where
        sub x = State.isSubCursor ?? x

enterWithSearchTerm :: Text -> Id -> State.Update
enterWithSearchTerm searchTerm myId =
    State.updateCursor myId
    <> State.updateWidgetState myId searchTerm

make ::
    ( MonadReader env m, Applicative f, HasState env, Has Menu.Config env
    , Has TextView.Style env, Has Hover.Style env, Element.HasAnimIdPrefix env
    , Has (Menu.Texts Text) env, Glue.HasTexts env
    ) =>
    (Menu.PickFirstResult f -> m (Term f)) ->
    (ResultsContext -> m (Menu.OptionList (Menu.Option m f))) ->
    View -> Id ->
    m (Menu.Placement -> TextWidget f)
make makeSearchTerm makeOptions ann myId =
    readSearchTerm myId <&> (`ResultsContext` resultsIdPrefix myId)
    >>= makeOptions
    >>=
    \options ->
    do
        (mPickFirst, makeMenu) <- Menu.makeHovered myId ann options
        makeSearchTerm mPickFirst
            <&> \term placement ->
                term ^. termWidget <&> makeMenu placement
                <&> Widget.weakerEventsWithoutPreevents (term ^. termEditEventMap)
    & Reader.local (Element.animIdPrefix .~ toAnimId myId)
    & assignCursor myId (options ^.. traverse . Menu.oId)

searchTermEditEventMap ::
    (Applicative f, HasTexts env) =>
    env -> Id -> (Text -> Bool) -> Text -> EventMap (f State.Update)
searchTermEditEventMap env myId allowedTerms searchTerm =
    appendCharEventMap <> deleteCharEventMap
    <&> State.updateWidgetState myId
    <&> pure
    where
        appendCharEventMap =
            Text.snoc searchTerm
            & E.allChars "Character" (searchTermDoc env (has . textAppendChar))
            -- We only filter when appending last char, not when
            -- deleting last char, because after appending deletion
            -- necessarily preserves any invariant we enforce in
            -- allowedTerms
            & E.filter allowedTerms
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                Text.init searchTerm
                & E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                    (searchTermDoc env (has . textDeleteBackwards))
