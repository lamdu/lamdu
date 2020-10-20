module Lamdu.GUI.Expr.AssignmentEdit
    ( make
    , Parts(..), makeFunctionParts
    , makeJumpToRhs
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurAndPrev, current, fallbackToPrev)
import           Data.List.Extended (withPrevNext)
import qualified Data.Map as Map
import           Data.Property (Property)
import qualified Data.Property as Property
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.FocusDirection as Direction
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
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
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.Annotation as Annotation
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.PresentationModeEdit as PresentationModeEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrap)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Parts i o = Parts
    { pMParamsEdit :: Maybe (Responsive o)
    , pMScopesEdit :: Maybe (Widget o)
    , pBodyEdit :: Responsive o
    , pEventMap :: EventMap (o GuiState.Update)
    , pMLamPayload :: Maybe (Sugar.Payload (Sugar.EvaluationScopes Name i) Name i o, ExprGui.Payload)
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
    Sugar.Body Sugar.Function v Name i o ExprGui.Payload ->
    GuiM env i o (CurAndPrev (Maybe ScopeCursor))
mkChosenScopeCursor func =
    do
        mOuterScopeId <- GuiM.readMScopeId
        readFunctionChosenScope func & GuiM.im <&>
            \mChosenScope ->
            liftA2 lookupMKey mOuterScopeId (func ^. Sugar.fBodyScopes)
            <&> (>>= scopeCursor mChosenScope)

makeScopeEventMap ::
    ( Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    env -> [MetaKey] -> [MetaKey] -> ScopeCursor -> (Sugar.BinderParamScopeId -> o ()) ->
    EventMap (o GuiState.Update)
makeScopeEventMap env prevKey nextKey cursor setter =
    mkEventMap (sMPrevParamScope, prevKey, Texts.prev) ++
    mkEventMap (sMNextParamScope, nextKey, Texts.next)
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

makeScopeNavArrow ::
    ( MonadReader env m, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env, Applicative o
    , Has Dir.Layout env
    ) =>
    (w -> o GuiState.Update) -> Text -> Maybe w -> m (TextWidget o)
makeScopeNavArrow setScope arrowText mScopeId =
    do
        theme <- Lens.view has
        Label.make arrowText
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

blockEventMap ::
    ( Has (Texts.Navigation Text) env
    , Has (MomentuTexts.Texts Text) env
    , Applicative m
    ) => env -> EventMap (m GuiState.Update)
blockEventMap env =
    pure mempty
    & E.keyPresses (dirKeys <&> toModKey)
    (E.toDoc env
        [ has . MomentuTexts.navigation, has . MomentuTexts.move
        , has . Texts.blocked
        ])
    where
        dirKeys = [MetaKey.Key'Left, MetaKey.Key'Right] <&> MetaKey noMods

makeScopeNavEdit ::
    ( Monad i, Applicative o
    , Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Glue.HasTexts env
    ) =>
    Sugar.Function v name i o expr -> Widget.Id -> ScopeCursor ->
    GuiM env i o
    ( EventMap (o GuiState.Update)
    , Maybe (Widget o)
    )
makeScopeNavEdit func myId curCursor =
    do
        evalConfig <- Lens.view (has . Config.eval)
        chosenScopeProp <- func ^. Sugar.fChosenScopeProp & GuiM.im
        let setScope =
                (mempty <$) .
                Property.set chosenScopeProp . Just
        env <- Lens.view id
        let mkScopeEventMap l r = makeScopeEventMap env l r curCursor (void . setScope)
        let scopes :: [(Text, Maybe Sugar.BinderParamScopeId)]
            scopes =
                [ (env ^. has . Texts.prevScopeArrow, sMPrevParamScope curCursor)
                , (" ", Nothing)
                , (env ^. has . Texts.nextScopeArrow, sMNextParamScope curCursor)
                ]
        Lens.view (has . Settings.sAnnotationMode)
            >>= \case
            Annotations.Evaluation ->
                (Widget.makeFocusableWidget ?? myId)
                <*> ( Glue.hbox <*> traverse (uncurry (makeScopeNavArrow setScope)) scopes
                        <&> (^. Align.tValue)
                    )
                <&> Widget.weakerEvents
                    (mkScopeEventMap leftKeys rightKeys <> blockEventMap env)
                <&> Just
                <&> (,) (mkScopeEventMap
                         (evalConfig ^. Config.prevScopeKeys)
                         (evalConfig ^. Config.nextScopeKeys))
            _ -> pure (mempty, Nothing)
    where
        leftKeys = [MetaKey noMods MetaKey.Key'Left]
        rightKeys = [MetaKey noMods MetaKey.Key'Right]

data IsScopeNavFocused = ScopeNavIsFocused | ScopeNavNotFocused
    deriving (Eq, Ord)

nullParamEditInfo ::
    Widget.Id -> TextWidget o ->
    Sugar.NullParamActions o -> ParamEdit.Info i o
nullParamEditInfo widgetId nameEdit mActions =
    ParamEdit.Info
    { ParamEdit.iNameEdit = nameEdit
    , ParamEdit.iAddNext = Nothing
    , ParamEdit.iMOrderBefore = Nothing
    , ParamEdit.iMOrderAfter = Nothing
    , ParamEdit.iDel = mActions ^. Sugar.npDeleteLambda
    , ParamEdit.iId = widgetId
    }

namedParamEditInfo ::
    Widget.Id -> Sugar.FuncParamActions Name i o ->
    TextWidget o ->
    ParamEdit.Info i o
namedParamEditInfo widgetId actions nameEdit =
    ParamEdit.Info
    { ParamEdit.iNameEdit = nameEdit
    , ParamEdit.iAddNext = actions ^. Sugar.fpAddNext & Just
    , ParamEdit.iMOrderBefore = actions ^. Sugar.fpMOrderBefore
    , ParamEdit.iMOrderAfter = actions ^. Sugar.fpMOrderAfter
    , ParamEdit.iDel = actions ^. Sugar.fpDelete
    , ParamEdit.iId = widgetId
    }

makeParamsEdit ::
    ( Monad i, Monad o
    , Has (TextEdit.Texts Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.Navigation Text) env
    , Glue.HasTexts env, SearchMenu.HasTexts env
    ) =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Sugar.EvaluationScopes Name i) Name i o ->
    GuiM env i o [Responsive o]
makeParamsEdit annotationOpts delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.NullParam p ->
        do
            nullParamGui <-
                (Widget.makeFocusableView ?? nullParamId <&> (Align.tValue %~))
                <*> grammar (label Texts.defer)
            fromParamList delVarBackwardsId rhsId
                [p & _2 %~ nullParamEditInfo lhsId nullParamGui]
        where
            nullParamId = Widget.joinId lhsId ["param"]
    Sugar.Params ps ->
        ps
        & traverse . _2 %%~ onFpInfo
        >>= fromParamList delVarBackwardsId rhsId
        where
            onFpInfo x =
                TagEdit.makeParamTag (x ^. Sugar.piTag)
                <&> namedParamEditInfo widgetId (x ^. Sugar.piActions)
                where
                    widgetId =
                        x ^. Sugar.piTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
    where
        fromParamList delDestFirst delDestLast paramList =
            withPrevNext delDestFirst delDestLast
            (ParamEdit.iId . snd) paramList
            & traverse mkParam <&> concat
            where
                mkParam (prevId, nextId, param) = ParamEdit.make annotationOpts prevId nextId param

makeMParamsEdit ::
    ( Monad i, Monad o
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    ) =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    Widget.Id ->
    Sugar.AddFirstParam Name i o ->
    Maybe (Sugar.BinderParams (Sugar.EvaluationScopes Name i) Name i o) ->
    GuiM env i o (Maybe (Responsive o))
makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId bodyId addFirstParam mParams =
    do
        isPrepend <- GuiState.isSubCursor ?? prependId
        prependParamEdits <-
            case addFirstParam of
            Sugar.PrependParam selection | isPrepend ->
                TagEdit.makeTagHoleEdit selection ParamEdit.mkParamPickResult prependId
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
        prependId = TagEdit.addParamId myId
        frame =
            case mParams of
            Just (Sugar.Params (_:_:_)) -> Styled.addValFrame
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
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.FuncApplyLimit ->
    Sugar.Expr Sugar.Function (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    Widget.Id ->
    GuiM env i o (Parts i o)
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
        myId = WidgetIds.fromExprPayload (pl ^. _1)
        destId =
            case func ^. Sugar.fParams of
            Sugar.NullParam{} -> bodyId
            Sugar.Params ps ->
                ps ^?!
                traverse . _2 . Sugar.piTag . Sugar.tagRefTag . Sugar.tagInstance
                & WidgetIds.fromEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]
        bodyId = func ^. Sugar.fBody . annotation . _1 & WidgetIds.fromExprPayload

makePlainParts ::
    ( Monad i, Monad o
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    ) =>
    Sugar.Expr Sugar.AssignPlain (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    Widget.Id ->
    GuiM env i o (Parts i o)
makePlainParts (Ann (Const pl) assignPlain) delVarBackwardsId =
    do
        mParamsEdit <-
            makeMParamsEdit (pure Nothing) ScopeNavNotFocused delVarBackwardsId myId myId
            (assignPlain ^. Sugar.apAddFirstParam) Nothing
        rhs <-
            assignPlain ^. Sugar.apBody & Ann (Const pl)
            & GuiM.makeBinder
        Parts mParamsEdit Nothing rhs mempty Nothing myId & pure
    where
        myId = WidgetIds.fromExprPayload (pl ^. _1)

makeParts ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.FuncApplyLimit ->
    Sugar.Expr Sugar.Assignment (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    Widget.Id ->
    GuiM env i o (Parts i o)
makeParts funcApplyLimit (Ann (Const pl) assignmentBody) =
    case assignmentBody of
    Sugar.BodyFunction x -> makeFunctionParts funcApplyLimit (Ann (Const pl) x)
    Sugar.BodyPlain x -> makePlainParts (Ann (Const pl) x)

makeJumpToRhs ::
    ( Monad i, Monad o
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Widget.Id -> GuiM env i o (EventMap (o GuiState.Update))
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

make ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Choice.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Maybe (i (Property o Meta.PresentationMode)) ->
    Sugar.TagRef Name i o -> Lens.ALens' TextColors Draw.Color ->
    Sugar.Expr Sugar.Assignment (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
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
            ParamEdit.eventMapAddFirstParam myId (assignmentBody ^. SugarLens.assignmentBodyAddFirstParam)
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        defNameEdit <-
            TagEdit.makeBinderTagEdit color tag
            <&> Align.tValue %~ Widget.weakerEvents (rhsJumperEquals <> addFirstParamEventMap)
            <&> (|---| fromMaybe Element.empty mPresentationEdit)
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
                (mParamEdit ^.. Lens._Just) ++
                [Responsive.fromTextView equals]
                & hbox
            , bodyEdit
            ]
            & pure
        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
        & maybe id stdWrap mLamPl
        <&> Widget.weakerEvents eventMap
    where
        myId = WidgetIds.fromExprPayload (pl ^. _1)
        delParamDest = tag ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
        Ann (Const pl) assignmentBody = assignment
        presentationChoiceId = Widget.joinId myId ["presentation"]
