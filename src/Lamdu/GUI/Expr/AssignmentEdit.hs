module Lamdu.GUI.Expr.AssignmentEdit
    ( make
    , Parts(..), makeFunctionParts
    , makeJumpToRhs
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev, current, fallbackToPrev)
import qualified Data.Map as Map
import           Data.Property (Property)
import qualified Data.Property as Property
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.FocusDirection as Direction
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (ModKey(..), noMods)
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.GUI.Annotation as Annotation
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.PresentationModeEdit as PresentationModeEdit
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TaggedList as TaggedList
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrap)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Parts i o = Parts
    { pMParamsEdit :: Maybe (Responsive o)
    , pMScopesEdit :: Maybe (M.Widget o)
    , pBodyEdit :: Responsive o
    , pEventMap :: EventMap (o M.Update)
    , pMLamPayload :: Maybe (ExprGui.Payload i o)
    , pRhsId :: Widget.Id
    }

data ScopeCursor = ScopeCursor
    { sBinderScope :: Sugar.BinderParamScopeId
    , sMPrevParamScope :: Maybe Sugar.BinderParamScopeId
    , sMNextParamScope :: Maybe Sugar.BinderParamScopeId
    }

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

readFunctionChosenScope ::
    Functor i => Sugar.Function v name i o expr -> i (Maybe Sugar.BinderParamScopeId)
readFunctionChosenScope func = func ^. Sugar.fChosenScopeProp <&> Property.value

lookupMKey :: Ord k => Maybe k -> Map k a -> Maybe a
lookupMKey k m = k >>= (`Map.lookup` m)

mkChosenScopeCursor ::
    Monad i =>
    Sugar.Body Sugar.Function v Name i o ->
    GuiM env i o (CurAndPrev (Maybe ScopeCursor))
mkChosenScopeCursor func =
    do
        mOuterScopeId <- GuiM.readMScopeId
        readFunctionChosenScope func & GuiM.im <&>
            \mChosenScope ->
            liftA2 lookupMKey mOuterScopeId (func ^. Sugar.fBodyScopes)
            <&> (>>= scopeCursor mChosenScope)

makeScopeEventMap ::
    _ =>
    env -> ScopeCursor -> (Sugar.BinderParamScopeId -> o ()) -> [ModKey] -> [ModKey] ->
    EventMap (o M.Update)
makeScopeEventMap env cursor setter prevKeys nextKeys =
    mkEventMap (sMPrevParamScope, prevKeys, Texts.prev) ++
    mkEventMap (sMNextParamScope, nextKeys, Texts.next)
    & mconcat
    where
        mkEventMap (cursorField, key, lens) =
            cursorField cursor ^.. Lens._Just
            <&> setter
            <&> E.keysEventMap key (doc lens)
        doc x =
            E.toDoc env
            [ has . Texts.evaluation
            , has . Texts.scope
            , has . x
            ]

makeScopeNavArrow :: _ => (w -> o M.Update) -> Text -> Maybe w -> m (M.TextWidget o)
makeScopeNavArrow setScope arrowText mScopeId =
    do
        theme <- Lens.view has
        Label.make arrowText
            <&> M.tValue %~ Widget.fromView
            <&> M.tValue %~
                Widget.sizedState <. Widget._StateUnfocused . Widget.uMEnter
                .@~ mEnter
            & local
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

blockEventMap :: _ => env -> EventMap (m M.Update)
blockEventMap env =
    pure mempty
    & E.keyPresses dirKeys
    (E.toDoc env
        [ has . MomentuTexts.navigation, has . MomentuTexts.move
        , has . Texts.blocked
        ])
    where
        dirKeys = [ModKey.Key'Left, ModKey.Key'Right] <&> noMods

makeScopeNavEdit ::
    _ =>
    Sugar.Function v name i o expr -> Widget.Id -> ScopeCursor ->
    GuiM env i o
    ( EventMap (o M.Update)
    , Maybe (M.Widget o)
    )
makeScopeNavEdit func myId curCursor =
    do
        evalConfig <- Lens.view (has . Config.eval)
        chosenScopeProp <- func ^. Sugar.fChosenScopeProp & GuiM.im
        let setScope =
                (mempty <$) .
                Property.set chosenScopeProp . Just
        env <- Lens.view id
        let mkScopeEventMap = makeScopeEventMap env curCursor (void . setScope)
        let scopes :: [(Text, Maybe Sugar.BinderParamScopeId)]
            scopes =
                [ (env ^. has . Texts.prevScopeArrow, sMPrevParamScope curCursor)
                , (" ", Nothing)
                , (env ^. has . Texts.nextScopeArrow, sMNextParamScope curCursor)
                ]
        (prevKeys, nextKeys) <-
            Lens.view has <&>
            \case
            Dir.LeftToRight -> (leftKeys, rightKeys)
            Dir.RightToLeft -> (rightKeys, leftKeys)
        Lens.view (has . Settings.sAnnotationMode)
            >>= \case
            Annotations.Evaluation ->
                (Widget.makeFocusableWidget ?? myId)
                <*> ( Glue.hbox <*> traverse (uncurry (makeScopeNavArrow setScope)) scopes
                        <&> (^. M.tValue)
                    )
                <&> Widget.weakerEvents
                    (mkScopeEventMap prevKeys nextKeys <> blockEventMap env)
                <&> Just
                <&> (,) (mkScopeEventMap
                         (evalConfig ^. Config.prevScopeKeys)
                         (evalConfig ^. Config.nextScopeKeys))
            _ -> pure (mempty, Nothing)
    where
        leftKeys = [noMods ModKey.Key'Left]
        rightKeys = [noMods ModKey.Key'Right]

data IsScopeNavFocused = ScopeNavIsFocused | ScopeNavNotFocused
    deriving (Eq, Ord)

makeParamsEdit ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o ->
    GuiM env i o [Responsive o]
makeParamsEdit annotationOpts delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.NullParam (p, actions) ->
        Widget.weakerEvents
        <$> ( Lens.view id <&>
                \env ->
                E.keyPresses (env ^. has . (Config.delForwardKeys <> Config.delBackwardKeys))
                (E.toDoc env [has . MomentuTexts.edit, has . MomentuTexts.delete])
                (GuiState.updateCursor rhsId <$ actions ^. Sugar.npDeleteLambda)
            )
        <*> ( (Widget.makeFocusableView ?? nullParamId <&> (M.tValue %~))
                <*> grammar (label Texts.defer)
                >>= ParamEdit.addAnnotation annotationOpts p nullParamId
            )
        <&> (:[])
        where
            nullParamId = Widget.joinId lhsId ["param"]
    Sugar.RecordParams ps ->
        case ps ^. Sugar.tlItems of
        Nothing -> error "Empty record params?"
        Just items -> ParamEdit.makeParams annotationOpts delVarBackwardsId rhsId items
    Sugar.VarParam (param, pInfo) ->
        do
            eventMap <-
                Lens.view id <&>
                \env ->
                TaggedList.delEventMap (has . Texts.parameter) (pInfo ^. Sugar.vpiDelete) delVarBackwardsId rhsId env <>
                ParamEdit.eventMapAddNextParamOrPickTag widgetId (pInfo ^. Sugar.vpiAddNext) env
            paramEdit <-
                TagEdit.makeParamTag (Just (tag ^. Sugar.oPickAnon)) (tag ^. Sugar.oTag)
                >>= ParamEdit.addAnnotation annotationOpts param widgetId
                <&> M.weakerEvents eventMap
            case pInfo ^? Sugar.vpiAddNext . Sugar._AddNext of
                Nothing -> pure [paramEdit]
                Just addNext -> ParamEdit.addAddParam addNext widgetId paramEdit
        where
            tag = pInfo ^. Sugar.vpiTag
            widgetId = tag ^. Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
    & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods M.Key'Space])

makeMParamsEdit ::
    _ =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    Widget.Id ->
    Sugar.AddParam Name i o ->
    Maybe (Sugar.BinderParams (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o) ->
    GuiM env i o (Maybe (Responsive o))
makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId bodyId addFirstParam mParams =
    do
        isPrepend <- GuiState.isSubCursor ?? prependId
        prependParamEdits <-
            case addFirstParam of
            Sugar.AddNext selection | isPrepend ->
                GuiM.im selection
                >>= TagEdit.makeTagHoleEdit ParamEdit.mkParamPickResult prependId
                & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods M.Key'Space])
                & Styled.withColor TextColors.parameterColor
                <&> Responsive.fromWithTextPos
                <&> (:[])
            _ -> pure []
        paramEdits <-
            case mParams of
            Nothing -> pure []
            Just params ->
                makeParamsEdit annotationMode
                delVarBackwardsId myId bodyId params
                & GuiM.withLocalMScopeId
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
        prependId = TagEdit.addItemId myId
        frame =
            case mParams of
            Just Sugar.RecordParams{} -> Styled.addValFrame
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

makeFunctionParts ::
    _ =>
    Sugar.FuncApplyLimit -> ExprGui.Expr Sugar.Function i o -> Widget.Id -> GuiM env i o (Parts i o)
makeFunctionParts funcApplyLimit (Ann (Const pl) func) delVarBackwardsId =
    do
        mScopeCursor <- mkChosenScopeCursor func
        let binderScopeId = mScopeCursor <&> Lens.mapped %~ (^. Sugar.bParamScopeId) . sBinderScope
        (scopeEventMap, mScopeNavEdit) <-
            do
                guard (funcApplyLimit == Sugar.UnlimitedFuncApply)
                scope <- fallbackToPrev mScopeCursor
                guard $
                    Lens.nullOf (Sugar.fParams . Sugar._NullParam) func ||
                    Lens.has (Lens.traversed . Lens._Just) [sMPrevParamScope scope, sMNextParamScope scope]
                Just scope
                & maybe (pure (mempty, Nothing)) (makeScopeNavEdit func scopesNavId)
        let isScopeNavFocused =
                case mScopeNavEdit of
                Just edit | Widget.isFocused edit -> ScopeNavIsFocused
                _ -> ScopeNavNotFocused
        do
            paramsEdit <-
                makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId
                bodyId (func ^. Sugar.fAddFirstParam) (Just (func ^. Sugar.fParams))
            rhs <- GuiM.makeBinder (func ^. Sugar.fBody)
            Parts paramsEdit mScopeNavEdit rhs scopeEventMap (Just pl) bodyId & pure
            & case mScopeNavEdit of
              Nothing -> GuiState.assignCursorPrefix scopesNavId (const destId)
              Just _ -> id
            & GuiM.withLocalMScopeId binderScopeId
    where
        myId = WidgetIds.fromExprPayload pl
        destId =
            case func ^. Sugar.fParams of
            Sugar.NullParam{} -> bodyId
            Sugar.VarParam (_, pInfo) ->
                pInfo ^. Sugar.vpiTag . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
            Sugar.RecordParams ps ->
                ps ^?! SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagInstance
                & WidgetIds.fromEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]
        bodyId = func ^. Sugar.fBody . annotation & WidgetIds.fromExprPayload

makePlainParts ::
    _ =>
    ExprGui.Expr Sugar.AssignPlain i o -> GuiM env i o (Parts i o)
makePlainParts (Ann (Const pl) assignPlain) =
    assignPlain ^. Sugar.apBody & Ann (Const pl) & GuiM.makeBinder
    <&> \rhs -> Parts Nothing Nothing rhs mempty Nothing myId
    where
        myId = WidgetIds.fromExprPayload pl

makeParts ::
    _ =>
    Sugar.FuncApplyLimit -> ExprGui.Expr Sugar.Assignment i o -> Widget.Id -> GuiM env i o (Parts i o)
makeParts funcApplyLimit (Ann (Const pl) assignmentBody) myId =
    case assignmentBody of
    Sugar.BodyFunction x -> makeFunctionParts funcApplyLimit (Ann (Const pl) x) myId
    Sugar.BodyPlain x -> makePlainParts (Ann (Const pl) x)

makeJumpToRhs :: _ => Widget.Id -> GuiM env i o (EventMap (o M.Update))
makeJumpToRhs rhsId =
    do
        env <- Lens.view id
        GuiM.mkPrejumpPosSaver
            <&> Lens.mapped .~ GuiState.updateCursor rhsId
            <&> const
            <&> E.charGroup Nothing
            (E.toDoc env
                [ has . MomentuTexts.navigation
                , has . Texts.jumpToDefBody
                ])
            "="

eventMapAddFirstParam ::
    _ => Widget.Id -> Sugar.Assignment v name i o a -> m (EventMap (o GuiState.Update))
eventMapAddFirstParam binderId assignment =
    Lens.view id
    <&>
    \env ->
    E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
    (E.toDoc env (has . MomentuTexts.edit : doc)) action
    where
        enterParam = WidgetIds.tagHoleId . WidgetIds.fromEntityId
        (action, doc) =
            case assignment of
            Sugar.BodyPlain x ->
                (x ^. Sugar.apAddFirstParam <&> enterParam, [has . Texts.parameter, has . Texts.add])
            Sugar.BodyFunction f ->
                case f ^. Sugar.fAddFirstParam of
                Sugar.NeedToPickTagToAddNext x ->
                    (pure (enterParam x), [has . Texts.nameFirstParameter])
                Sugar.AddNext{} ->
                    (pure (TagEdit.addItemId binderId), [has . Texts.parameter, has . Texts.add])

make ::
    _ =>
    Maybe (i (Property o Meta.PresentationMode)) ->
    Sugar.OptionalTag Name i o -> Lens.ALens' TextColors M.Color ->
    ExprGui.Expr Sugar.Assignment i o ->
    GuiM env i o (Responsive o)
make pMode tag color assignment =
    makeParts Sugar.UnlimitedFuncApply assignment delParamDest
    >>= \(Parts mParamsEdit mScopeEdit bodyEdit eventMap mLamPl rhsId) ->
    do
        rhsJumperEquals <- makeJumpToRhs rhsId
        mPresentationEdit <-
            case assignmentBody of
            Sugar.BodyPlain{} -> pure Nothing
            Sugar.BodyFunction x ->
                pMode & sequenceA & GuiM.im
                >>= traverse
                    (PresentationModeEdit.make presentationChoiceId (x ^. Sugar.fParams))
        addFirstParamEventMap <-
            eventMapAddFirstParam myId assignmentBody
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        defNameEdit <-
            TagEdit.makeBinderTagEdit color tag
            <&> M.tValue %~ Widget.weakerEvents (rhsJumperEquals <> addFirstParamEventMap)
            <&> (|---| fromMaybe M.empty mPresentationEdit)
            <&> Responsive.fromWithTextPos
        mParamEdit <-
            case mParamsEdit of
            Nothing -> pure Nothing
            Just paramsEdit ->
                Responsive.vboxSpaced
                ?? (paramsEdit : fmap Responsive.fromWidget mScopeEdit ^.. Lens._Just)
                <&> Widget.strongerEvents rhsJumperEquals
                <&> Just
        equals <- grammar (label Texts.assign)
        hbox <- Options.boxSpaced ?? Options.disambiguationNone
        hbox [ defNameEdit :
                mParamEdit ^.. Lens._Just ++
                [Responsive.fromTextView equals]
                & hbox
            , bodyEdit
            ]
            & pure
        & local (M.animIdPrefix .~ Widget.toAnimId myId)
        & maybe id stdWrap mLamPl
        <&> Widget.weakerEvents eventMap
    where
        myId = WidgetIds.fromExprPayload pl
        delParamDest = tag ^. Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
        Ann (Const pl) assignmentBody = assignment
        presentationChoiceId = Widget.joinId myId ["presentation"]
