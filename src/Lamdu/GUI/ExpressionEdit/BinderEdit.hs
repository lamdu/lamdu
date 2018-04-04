{-# LANGUAGE NamedFieldPuns, FlexibleContexts, NoMonomorphismRestriction, RankNTypes #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make
    , makeBinderBodyEdit
    , addLetEventMap
    , Parts(..), makeParts
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurAndPrev, current, fallbackToPrev)
import           Data.List.Utils (withPrevNext)
import qualified Data.Map as Map
import           Data.Property (Property)
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.MetaKey (MetaKey(..), noMods, toModKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.GUI.CodeEdit.AnnotationMode (AnnotationMode(..))
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (parentDelegator)
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.PresentationModeEdit as PresentationModeEdit
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeBinderNameEdit ::
    (Monad im, Monad am) =>
    Widget.Id -> Sugar.BinderActions (Name am) im am ->
    EventMap (am GuiState.Update) ->
    Sugar.Tag (Name am) im am -> Lens.Getter TextColors Draw.Color ->
    ExprGuiM im am (WithTextPos (Widget (am GuiState.Update)))
makeBinderNameEdit binderId binderActions rhsJumperEquals tag color =
    do
        addFirstParamEventMap <-
            ParamEdit.eventMapAddFirstParam binderId (binderActions ^. Sugar.baAddFirstParam)
        let eventMap = rhsJumperEquals <> addFirstParamEventMap
        TagEdit.makeBinderTagEdit color tag
            <&> Align.tValue %~ Widget.weakerEvents eventMap

data Parts am = Parts
    { pMParamsEdit :: Maybe (ExpressionGui am)
    , pMScopesEdit :: Maybe (Widget (am GuiState.Update))
    , pBodyEdit :: ExpressionGui am
    , pEventMap :: EventMap (am GuiState.Update)
    }

data ScopeCursor = ScopeCursor
    { sBinderScope :: Sugar.BinderParamScopeId
    , sMPrevParamScope :: Maybe Sugar.BinderParamScopeId
    , sMNextParamScope :: Maybe Sugar.BinderParamScopeId
    }

trivialScopeCursor :: Sugar.BinderParamScopeId -> ScopeCursor
trivialScopeCursor x = ScopeCursor x Nothing Nothing

scopeCursor :: Maybe Sugar.BinderParamScopeId -> [Sugar.BinderParamScopeId] -> Maybe ScopeCursor
scopeCursor mChosenScope scopes =
    do
        chosenScope <- mChosenScope
        (prevs, it:nexts) <- break (== chosenScope) scopes & Just
        Just ScopeCursor
            { sBinderScope = it
            , sMPrevParamScope = reverse prevs ^? Lens.traversed
            , sMNextParamScope = nexts ^? Lens.traversed
            }
    <|> (scopes ^? Lens.traversed <&> def)
    where
        def binderScope =
            ScopeCursor
            { sBinderScope = binderScope
            , sMPrevParamScope = Nothing
            , sMNextParamScope = scopes ^? Lens.ix 1
            }

lookupMKey :: Ord k => Maybe k -> Map k a -> Maybe a
lookupMKey k m = k >>= (`Map.lookup` m)

readBinderChosenScope ::
    Functor im => Sugar.Binder name im am expr -> im (Maybe Sugar.BinderParamScopeId)
readBinderChosenScope binder =
    binder ^. Sugar.bChosenScopeProp <&> Property.value

mkChosenScopeCursor ::
    Monad im =>
    Sugar.Binder (Name am) im am (ExprGui.SugarExpr im am) ->
    ExprGuiM im am (CurAndPrev (Maybe ScopeCursor))
mkChosenScopeCursor binder =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        case binder ^. Sugar.bBodyScopes of
            Sugar.SameAsParentScope ->
                mOuterScopeId <&> fmap (trivialScopeCursor . Sugar.BinderParamScopeId) & pure
            Sugar.BinderBodyScope binderBodyScope ->
                readBinderChosenScope binder & ExprGuiM.im
                <&> \mChosenScope ->
                liftA2 lookupMKey mOuterScopeId binderBodyScope
                <&> (>>= scopeCursor mChosenScope)

makeScopeEventMap ::
    Monad am =>
    [MetaKey] -> [MetaKey] -> ScopeCursor -> (Sugar.BinderParamScopeId -> am ()) ->
    EventMap (am GuiState.Update)
makeScopeEventMap prevKey nextKey cursor setter =
    do
        (key, doc, scope) <-
            (sMPrevParamScope cursor ^.. Lens._Just <&> (,,) prevKey prevDoc) ++
            (sMNextParamScope cursor ^.. Lens._Just <&> (,,) nextKey nextDoc)
        [setter scope & E.keysEventMap key doc]
    & mconcat
    where
        prevDoc = E.Doc ["Evaluation", "Scope", "Previous"]
        nextDoc = E.Doc ["Evaluation", "Scope", "Next"]

blockEventMap :: Monad m => EventMap (m GuiState.Update)
blockEventMap =
    pure mempty
    & E.keyPresses (dirKeys <&> toModKey)
    (E.Doc ["Navigation", "Move", "(blocked)"])
    where
        dirKeys = [MetaKey.Key'Left, MetaKey.Key'Right] <&> MetaKey noMods

makeScopeNavArrow ::
    ( MonadReader env m, Theme.HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env, Monoid a, Applicative am
    ) =>
    (w -> am a) -> Text -> Maybe w -> m (WithTextPos (Widget (am a)))
makeScopeNavArrow setScope arrowText mScopeId =
    do
        theme <- Lens.view Theme.theme
        TextView.makeLabel arrowText
            <&> Align.tValue %~ Widget.fromView
            <&> Align.tValue %~
                Widget.sizedState <. Widget._StateUnfocused . Widget.uMEnter
                .@~ mEnter
            & Reader.local
            ( TextView.color .~
                case mScopeId of
                Nothing -> theme ^. Theme.disabledColor
                Just _ -> theme ^. Theme.textColors . TextColors.grammarColor
            )
    where
        mEnter size =
            mScopeId
            <&> setScope
            <&> validate
            where
                r = Rect 0 size
                res = Widget.EnterResult r 0
                validate action (Direction.Point point)
                    | point `Rect.isWithin` r = res action
                validate _ _ = res (pure mempty)

makeScopeNavEdit ::
    (Monad im, Monad am) =>
    Sugar.Binder name im am expr -> Widget.Id -> ScopeCursor ->
    ExprGuiM im am
    ( EventMap (am GuiState.Update)
    , Maybe (Widget (am GuiState.Update))
    )
makeScopeNavEdit binder myId curCursor =
    do
        evalConfig <- Lens.view (Config.config . Config.eval)
        chosenScopeProp <- binder ^. Sugar.bChosenScopeProp & ExprGuiM.im
        let setScope =
                (mempty <$) .
                Property.set chosenScopeProp . Just
        let mkScopeEventMap l r = makeScopeEventMap l r curCursor setScope
        Lens.view (Settings.settings . Settings.sAnnotationMode)
            >>= \case
            Evaluation ->
                (Widget.makeFocusableWidget ?? myId)
                <*> ( mapM (uncurry (makeScopeNavArrow setScope)) scopes
                        <&> Glue.hbox <&> (^. Align.tValue)
                    )
                <&> Widget.weakerEvents
                    (mkScopeEventMap leftKeys rightKeys `mappend` blockEventMap)
                <&> Just
                <&> (,) (mkScopeEventMap
                         (evalConfig ^. Config.prevScopeKeys)
                         (evalConfig ^. Config.nextScopeKeys))
            _ -> pure (mempty, Nothing)
    where
        leftKeys = [MetaKey noMods MetaKey.Key'Left]
        rightKeys = [MetaKey noMods MetaKey.Key'Right]
        scopes :: [(Text, Maybe Sugar.BinderParamScopeId)]
        scopes =
            [ ("◀", sMPrevParamScope curCursor)
            , (" ", Nothing)
            , ("▶", sMNextParamScope curCursor)
            ]

data IsScopeNavFocused = ScopeNavIsFocused | ScopeNavNotFocused
    deriving (Eq, Ord)

makeMParamsEdit ::
    (Monad im, Monad am) =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    NearestHoles -> Widget.Id -> Sugar.Binder (Name am) im am a ->
    ExprGuiM im am (Maybe (ExpressionGui am))
makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId nearestHoles bodyId binder =
    do
        isPrepend <- GuiState.isSubCursor ?? prependId
        prependParamEdits <-
            case binder ^. Sugar.bActions . Sugar.baAddFirstParam of
            Sugar.PrependParam selection | isPrepend ->
                TagEdit.makeTagHoleEdit selection ParamEdit.mkParamPickResult prependId
                & Styled.withColor TextColors.parameterColor
                <&> Responsive.fromWithTextPos
                <&> (:[])
            _ -> pure []
        paramEdits <-
            makeParamsEdit annotationMode nearestHoles
            delVarBackwardsId myId bodyId params
            & ExprGuiM.withLocalMScopeId
                ( mScopeCursor
                    <&> Lens.traversed %~ (^. Sugar.bParamScopeId) . sBinderScope
                )
        case prependParamEdits ++ paramEdits of
            [] -> pure Nothing
            edits ->
                frame
                <*> (Options.boxSpaced ?? Options.disambiguationNone ?? edits)
                <&> Just
    where
        prependId = TagEdit.addParamId myId
        params = binder ^. Sugar.bParams
        frame =
            case params of
            Sugar.Params (_:_:_) -> Styled.addValFrame
            _ -> pure id
        mCurCursor =
            do
                ScopeNavIsFocused == isScopeNavFocused & guard
                mScopeCursor ^. current
        annotationMode =
            Annotation.NeighborVals
            (mCurCursor >>= sMPrevParamScope)
            (mCurCursor >>= sMNextParamScope)
            & Annotation.WithNeighbouringEvalAnnotations

binderContentNearestHoles ::
    Sugar.BinderContent name im am (ExprGui.SugarExpr im am) -> NearestHoles
binderContentNearestHoles body =
    body ^? Lens.traverse
    & fromMaybe (error "We have at least a body expression inside the binder")
    & ExprGui.nextHolesBefore

makeParts ::
    (Monad im, Monad am) =>
    ExprGui.FuncApplyLimit ->
    Sugar.Binder (Name am) im am (ExprGui.SugarExpr im am) ->
    Widget.Id -> Widget.Id ->
    ExprGuiM im am (Parts am)
makeParts funcApplyLimit binder delVarBackwardsId myId =
    do
        mScopeCursor <- mkChosenScopeCursor binder
        let binderScopeId = mScopeCursor <&> Lens.mapped %~ (^. Sugar.bParamScopeId) . sBinderScope
        (scopeEventMap, mScopeNavEdit) <-
            do
                guard (funcApplyLimit == ExprGui.UnlimitedFuncApply)
                scope <- fallbackToPrev mScopeCursor
                guard $
                    Lens.nullOf (Sugar.bParams . Sugar._NullParam) binder ||
                    Lens.has (Lens.traversed . Lens._Just) [sMPrevParamScope scope, sMNextParamScope scope]
                Just scope
            & maybe (pure (mempty, Nothing)) (makeScopeNavEdit binder scopesNavId)
        let isScopeNavFocused =
                case mScopeNavEdit of
                Just edit | Widget.isFocused edit -> ScopeNavIsFocused
                _ -> ScopeNavNotFocused
        do
            mParamsEdit <-
                makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId
                (binderContentNearestHoles bodyContent) bodyId binder
            rhs <- makeBinderBodyEdit body
            Parts mParamsEdit mScopeNavEdit rhs scopeEventMap & pure
            & case mScopeNavEdit of
              Nothing -> GuiState.assignCursorPrefix scopesNavId (const destId)
              Just _ -> id
            & ExprGuiM.withLocalMScopeId binderScopeId
    where
        destId =
            case params of
            Sugar.BinderWithoutParams -> bodyId
            Sugar.NullParam{} -> bodyId
            Sugar.Params ps ->
                ps ^?! traverse . Sugar.fpInfo . Sugar.piTag . Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody
        bodyContent = body ^. Sugar.bbContent
        bodyId = bodyContent ^. SugarLens.binderContentEntityId & WidgetIds.fromEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]

maybeAddNodeActions ::
    (MonadReader env m, GuiState.HasCursor env, Config.HasConfig env, Applicative am) =>
    Widget.Id -> NearestHoles -> Sugar.NodeActions name im am ->
    m (Responsive (am GuiState.Update) -> Responsive (am GuiState.Update))
maybeAddNodeActions partId nearestHoles nodeActions =
    do
        isSelected <- Lens.view GuiState.cursor <&> (== partId)
        if isSelected
            then
                ExprEventMap.addWith ExprEventMap.defaultOptions
                ExprEventMap.ExprInfo
                { ExprEventMap.exprInfoActions = nodeActions
                , ExprEventMap.exprInfoNearestHoles = nearestHoles
                , ExprEventMap.exprInfoIsHoleResult = False
                , ExprEventMap.exprInfoMinOpPrec = 0
                , ExprEventMap.exprInfoIsSelected = True
                }
            else
                pure id

make ::
    (Monad im, Monad am) =>
    Maybe (im (Property am Meta.PresentationMode)) ->
    EventMap (am GuiState.Update) ->
    Sugar.Tag (Name am) im am -> Lens.Getter TextColors Draw.Color ->
    Sugar.Binder (Name am) im am (ExprGui.SugarExpr im am) ->
    Widget.Id ->
    ExprGuiM im am (ExpressionGui am)
make pMode lhsEventMap tag color binder myId =
    do
        Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            makeParts ExprGui.UnlimitedFuncApply binder myId myId
        rhsJumperEquals <- jumpToRHS bodyId
        mPresentationEdit <-
            pMode & sequenceA & ExprGuiM.im
            >>= traverse
                (PresentationModeEdit.make presentationChoiceId (binder ^. Sugar.bParams))
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        defNameEdit <-
            makeBinderNameEdit myId (binder ^. Sugar.bActions) rhsJumperEquals
            tag color
            <&> (/-/ fromMaybe Element.empty mPresentationEdit)
            <&> Responsive.fromWithTextPos
            <&> Widget.weakerEvents jumpHolesEventMap
        mParamEdit <-
            case mParamsEdit of
            Nothing -> pure Nothing
            Just paramsEdit ->
                Responsive.vboxSpaced
                ?? (paramsEdit : fmap Responsive.fromWidget mScopeEdit ^.. Lens._Just)
                <&> Widget.strongerEvents rhsJumperEquals
                <&> Just
        equals <- TextView.makeLabel "="
        addWholeBinderActions <-
            maybe
            (pure id)
            (maybeAddNodeActions wholeBinderId nearestHoles)
            (binder ^. Sugar.bActions . Sugar.baMNodeActions)
        let layoutWithBody hbox =
                hbox
                [ hbox (defNameEdit : (mParamEdit ^.. Lens._Just) ++ [Responsive.fromTextView equals])
                    & Widget.weakerEvents lhsEventMap
                , bodyEdit
                ]
        parentDelegator wholeBinderId
            <*> (Options.boxSpaced ?? Options.disambiguationNone <&> layoutWithBody)
            <&> addWholeBinderActions
            <&> Widget.weakerEvents eventMap
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    & case binder ^. Sugar.bLamId of
        Nothing -> id
        Just lamId ->
            GuiState.assignCursorPrefix (WidgetIds.fromEntityId lamId) (const bodyId)
    & GuiState.assignCursor (WidgetIds.newDest myId) (WidgetIds.tagHoleId nameId)
    & GuiState.assignCursor myId nameId
    where
        wholeBinderId = Widget.joinId myId ["whole binder"]
        nameId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        presentationChoiceId = Widget.joinId myId ["presentation"]
        body = binder ^. Sugar.bBody . Sugar.bbContent
        bodyId = body ^. SugarLens.binderContentEntityId & WidgetIds.fromEntityId
        nearestHoles = binderContentNearestHoles body

makeLetEdit ::
    (Monad im, Monad am) =>
    Sugar.Let (Name am) im am (ExprGui.SugarExpr im am) ->
    ExprGuiM im am (ExpressionGui am)
makeLetEdit item =
    do
        config <- Lens.view Config.config
        theme <- Lens.view Theme.theme
        let eventMap =
                foldMap
                ( E.keysEventMapMovesCursor (config ^. Config.extractKeys)
                    (E.Doc ["Edit", "Let clause", "Extract to outer scope"])
                    . fmap ExprEventMap.extractCursor
                ) (item ^? Sugar.lValue . Sugar.bActions . Sugar.baMNodeActions . Lens._Just . Sugar.extract)
                <>
                E.keysEventMapMovesCursor (Config.delKeys config)
                (E.Doc ["Edit", "Let clause", "Delete"])
                (bodyId <$ item ^. Sugar.lActions . Sugar.laDelete)
                <>
                foldMap
                ( E.keysEventMapMovesCursor (config ^. Config.inlineKeys)
                    (E.Doc ["Navigation", "Jump to first use"])
                    . pure . WidgetIds.fromEntityId
                ) (item ^? Sugar.lUsages . Lens.ix 0)
        letLabel <- Styled.grammarLabel "let"
        space <- Spacer.stdHSpace
        letEquation <-
            make Nothing mempty (item ^. Sugar.lName) TextColors.letColor binder letId
            <&> Widget.weakerEvents eventMap
            <&> Element.pad (theme ^. Theme.letItemPadding)
        letLabel /|/ space /|/ letEquation & pure
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId letId)
    where
        bodyId =
            item ^. Sugar.lBody . Sugar.bbContent . SugarLens.binderContentEntityId
            & WidgetIds.fromEntityId
        letId =
            item ^. Sugar.lEntityId & WidgetIds.fromEntityId
            & WidgetIds.letBinderId
        binder = item ^. Sugar.lValue

jumpToRHS ::
    (Monad im, Monad am) =>
    Widget.Id -> ExprGuiM im am (EventMap (am GuiState.Update))
jumpToRHS rhsId =
    ExprGuiM.mkPrejumpPosSaver
    <&> Lens.mapped .~ rhsId
    <&> E.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Equal]
        (E.Doc ["Navigation", "Jump to Def Body"])

addLetEventMap ::
    (Monad im, Monad am) =>
    am Sugar.EntityId -> ExprGuiM im am (EventMap (am GuiState.Update))
addLetEventMap addLet =
    do
        config <- Lens.view Config.config
        savePos <- ExprGuiM.mkPrejumpPosSaver
        savePos >> addLet
            <&> WidgetIds.fromEntityId <&> WidgetIds.letBinderId
            & E.keysEventMapMovesCursor (config ^. Config.letAddItemKeys)
                (E.Doc ["Edit", "Let clause", "Add"])
            & pure

makeBinderBodyEdit ::
    (Monad im, Monad am) =>
    Sugar.BinderBody (Name am) im am (ExprGui.SugarExpr im am) ->
    ExprGuiM im am (ExpressionGui am)
makeBinderBodyEdit (Sugar.BinderBody addOuterLet content) =
    do
        newLetEventMap <- addLetEventMap addOuterLet
        makeBinderContentEdit content <&> Widget.weakerEvents newLetEventMap

makeBinderContentEdit ::
    (Monad im, Monad am) =>
    Sugar.BinderContent (Name am) im am (ExprGui.SugarExpr im am) ->
    ExprGuiM im am (ExpressionGui am)
makeBinderContentEdit (Sugar.BinderExpr binderBody) =
    ExprGuiM.makeSubexpression binderBody
makeBinderContentEdit content@(Sugar.BinderLet l) =
    do
        config <- Lens.view Config.config
        let moveToInnerEventMap =
                body
                ^? Sugar.bbContent . Sugar._BinderLet
                . Sugar.lActions . Sugar.laNodeActions . Sugar.extract
                & foldMap
                (E.keysEventMap (config ^. Config.moveLetInwardKeys)
                (E.Doc ["Edit", "Let clause", "Move inwards"]) . void)
        mAddNodeActions <-
            maybeAddNodeActions letEntityId (binderContentNearestHoles content)
            (l ^. Sugar.lActions . Sugar.laNodeActions)
        mOuterScopeId <- ExprGuiM.readMScopeId
        let letBodyScope = liftA2 lookupMKey mOuterScopeId (l ^. Sugar.lBodyScope)
        parentDelegator letEntityId
            <*>
            ( Responsive.vboxSpaced
                <*>
                sequence
                [ makeLetEdit l <&> Widget.weakerEvents moveToInnerEventMap
                , makeBinderBodyEdit body
                & ExprGuiM.withLocalMScopeId letBodyScope
                ]
            )
            <&> mAddNodeActions
    where
        letEntityId = l ^. Sugar.lEntityId & WidgetIds.fromEntityId
        body = l ^. Sugar.lBody

namedParamEditInfo ::
    Widget.Id -> Sugar.FuncParamActions (Name am) im am ->
    WithTextPos (Widget (am GuiState.Update)) ->
    ParamEdit.Info im am
namedParamEditInfo widgetId actions nameEdit =
    ParamEdit.Info
    { ParamEdit.iNameEdit = nameEdit
    , ParamEdit.iAddNext = actions ^. Sugar.fpAddNext & Just
    , ParamEdit.iMOrderBefore = actions ^. Sugar.fpMOrderBefore
    , ParamEdit.iMOrderAfter = actions ^. Sugar.fpMOrderAfter
    , ParamEdit.iDel = actions ^. Sugar.fpDelete
    , ParamEdit.iId = widgetId
    }

nullParamEditInfo ::
    Widget.Id -> WithTextPos (Widget (am GuiState.Update)) ->
    Sugar.NullParamActions am -> ParamEdit.Info im am
nullParamEditInfo widgetId nameEdit mActions =
    ParamEdit.Info
    { ParamEdit.iNameEdit = nameEdit
    , ParamEdit.iAddNext = Nothing
    , ParamEdit.iMOrderBefore = Nothing
    , ParamEdit.iMOrderAfter = Nothing
    , ParamEdit.iDel = mActions ^. Sugar.npDeleteLambda
    , ParamEdit.iId = widgetId
    }

makeParamsEdit ::
    (Monad im, Monad am) =>
    Annotation.EvalAnnotationOptions -> NearestHoles ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Name am) im am ->
    ExprGuiM im am [ExpressionGui am]
makeParamsEdit annotationOpts nearestHoles delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.BinderWithoutParams -> pure []
    Sugar.NullParam p ->
        do
            nullParamGui <-
                (Widget.makeFocusableView ?? nullParamId <&> (Align.tValue %~))
                <*> Styled.grammarLabel "|"
            fromParamList delVarBackwardsId rhsId
                [p & Sugar.fpInfo %~ nullParamEditInfo lhsId nullParamGui]
        where
            nullParamId = Widget.joinId lhsId ["param"]
    Sugar.Params ps ->
        ps
        & traverse . Sugar.fpInfo %%~ onFpInfo
        >>= fromParamList lhsId rhsId
        where
            onFpInfo x =
                TagEdit.makeParamTag (x ^. Sugar.piTag)
                <&> namedParamEditInfo widgetId (x ^. Sugar.piActions)
                where
                    widgetId =
                        x ^. Sugar.piTag . Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
    where
        fromParamList delDestFirst delDestLast paramList =
            do
                jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
                withPrevNext delDestFirst delDestLast
                    (ParamEdit.iId . (^. Sugar.fpInfo)) paramList
                    & traverse mkParam <&> concat
                    <&> traverse . Widget.widget . Widget.eventMapMaker . Lens.mapped <>~ jumpHolesEventMap
            where
                mkParam (prevId, nextId, param) = ParamEdit.make annotationOpts prevId nextId param
