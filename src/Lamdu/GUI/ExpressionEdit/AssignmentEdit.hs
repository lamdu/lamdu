module Lamdu.GUI.ExpressionEdit.AssignmentEdit
    ( make
    , Parts(..), makeFunctionParts
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurAndPrev, current, fallbackToPrev)
import           Data.List.Extended (withPrevNext)
import qualified Data.Map as Map
import           Data.Property (Property)
import qualified Data.Property as Property
import           Data.Tree.Diverse (Ann(..), ann)
import           GUI.Momentu.Align (WithTextPos, TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.MetaKey (MetaKey(..), noMods, toModKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap)
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.PresentationModeEdit as PresentationModeEdit
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Settings as Settings
import           Lamdu.Sugar.Convert.Input (AnnotationMode(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Parts o = Parts
    { pMParamsEdit :: Maybe (Gui Responsive o)
    , pMScopesEdit :: Maybe (Gui Widget o)
    , pBodyEdit :: Gui Responsive o
    , pEventMap :: Gui EventMap o
    , pWrap :: Gui Responsive o -> Gui Responsive o
    , pRhsId :: Widget.Id
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

readFunctionChosenScope ::
    Functor i => Sugar.Function name i o expr -> i (Maybe Sugar.BinderParamScopeId)
readFunctionChosenScope func = func ^. Sugar.fChosenScopeProp <&> Property.value

lookupMKey :: Ord k => Maybe k -> Map k a -> Maybe a
lookupMKey k m = k >>= (`Map.lookup` m)

mkChosenScopeCursor ::
    Monad i =>
    Sugar.Function (Name o) i o (Ann (Sugar.Payload name i o ExprGui.Payload)) ->
    ExprGuiM i o (CurAndPrev (Maybe ScopeCursor))
mkChosenScopeCursor func =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        case func ^. Sugar.fBodyScopes of
            Sugar.SameAsParentScope ->
                mOuterScopeId <&> fmap (trivialScopeCursor . Sugar.BinderParamScopeId) & pure
            Sugar.BinderBodyScope assignmentBodyScope ->
                readFunctionChosenScope func & ExprGuiM.im
                <&> \mChosenScope ->
                liftA2 lookupMKey mOuterScopeId assignmentBodyScope
                <&> (>>= scopeCursor mChosenScope)

makeScopeEventMap ::
    Functor o =>
    [MetaKey] -> [MetaKey] -> ScopeCursor -> (Sugar.BinderParamScopeId -> o ()) ->
    Gui EventMap o
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

makeScopeNavArrow ::
    ( MonadReader env m, Theme.HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env, Monoid a, Applicative o
    ) =>
    (w -> o a) -> Text -> Maybe w -> m (WithTextPos (Widget (o a)))
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

blockEventMap :: Applicative m => Gui EventMap m
blockEventMap =
    pure mempty
    & E.keyPresses (dirKeys <&> toModKey)
    (E.Doc ["Navigation", "Move", "(blocked)"])
    where
        dirKeys = [MetaKey.Key'Left, MetaKey.Key'Right] <&> MetaKey noMods

makeScopeNavEdit ::
    (Monad i, Applicative o) =>
    Sugar.Function name i o expr -> Widget.Id -> ScopeCursor ->
    ExprGuiM i o
    ( Gui EventMap o
    , Maybe (Gui Widget o)
    )
makeScopeNavEdit func myId curCursor =
    do
        evalConfig <- Lens.view (Config.config . Config.eval)
        chosenScopeProp <- func ^. Sugar.fChosenScopeProp & ExprGuiM.im
        let setScope =
                (mempty <$) .
                Property.set chosenScopeProp . Just
        let mkScopeEventMap l r = makeScopeEventMap l r curCursor (void . setScope)
        Lens.view (Settings.settings . Settings.sAnnotationMode)
            >>= \case
            Evaluation ->
                (Widget.makeFocusableWidget ?? myId)
                <*> ( traverse (uncurry (makeScopeNavArrow setScope)) scopes
                        <&> Glue.hbox <&> (^. Align.tValue)
                    )
                <&> Widget.weakerEvents
                    (mkScopeEventMap leftKeys rightKeys <> blockEventMap)
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
    Widget.Id -> Sugar.FuncParamActions (Name o) i o ->
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
    (Monad i, Monad o) =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Name o) i o ->
    ExprGuiM i o [Gui Responsive o]
makeParamsEdit annotationOpts delVarBackwardsId lhsId rhsId params =
    case params of
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
        >>= fromParamList delVarBackwardsId rhsId
        where
            onFpInfo x =
                TagEdit.makeParamTag (x ^. Sugar.piTag)
                <&> namedParamEditInfo widgetId (x ^. Sugar.piActions)
                where
                    widgetId =
                        x ^. Sugar.piTag . Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
    where
        fromParamList delDestFirst delDestLast paramList =
            withPrevNext delDestFirst delDestLast
            (ParamEdit.iId . (^. Sugar.fpInfo)) paramList
            & traverse mkParam <&> concat
            where
                mkParam (prevId, nextId, param) = ParamEdit.make annotationOpts prevId nextId param

makeMParamsEdit ::
    (Monad i, Monad o) =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    Widget.Id ->
    Sugar.AddFirstParam (Name o) i o ->
    Maybe (Sugar.BinderParams (Name o) i o) ->
    ExprGuiM i o (Maybe (Gui Responsive o))
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
    (Monad i, Monad o) =>
    Sugar.FuncApplyLimit ->
    Sugar.Function (Name o) i o (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    Widget.Id ->
    ExprGuiM i o (Parts o)
makeFunctionParts funcApplyLimit func pl delVarBackwardsId =
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
            rhs <- ExprGuiM.makeBinder (func ^. Sugar.fBody)
            wrap <- stdWrap pl
            Parts paramsEdit mScopeNavEdit rhs scopeEventMap wrap bodyId & pure
            & case mScopeNavEdit of
              Nothing -> GuiState.assignCursorPrefix scopesNavId (const destId)
              Just _ -> id
            & ExprGuiM.withLocalMScopeId binderScopeId
    where
        myId = WidgetIds.fromExprPayload pl
        destId =
            case func ^. Sugar.fParams of
            Sugar.NullParam{} -> bodyId
            Sugar.Params ps ->
                ps ^?! traverse . Sugar.fpInfo . Sugar.piTag . Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]
        funcPl = func ^. Sugar.fBody . ann
        bodyId = WidgetIds.fromExprPayload funcPl

makePlainParts ::
    (Monad i, Monad o) =>
    Sugar.AssignPlain (Name o) i o (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    Widget.Id ->
    ExprGuiM i o (Parts o)
makePlainParts assignPlain pl delVarBackwardsId =
    do
        mParamsEdit <-
            makeMParamsEdit (pure Nothing) ScopeNavNotFocused delVarBackwardsId myId myId
            (assignPlain ^. Sugar.apAddFirstParam) Nothing
        rhs <-
            assignPlain ^. Sugar.apBody & Ann pl
            & ExprGuiM.makeBinder
        Parts mParamsEdit Nothing rhs mempty id myId & pure
    where
        myId = WidgetIds.fromExprPayload pl

makeParts ::
    (Monad i, Monad o) =>
    Sugar.FuncApplyLimit ->
    Sugar.Assignment (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    Widget.Id ->
    ExprGuiM i o (Parts o)
makeParts funcApplyLimit (Ann pl assignmentBody) =
    case assignmentBody of
    Sugar.BodyFunction x -> makeFunctionParts funcApplyLimit x pl
    Sugar.BodyPlain x -> makePlainParts x pl

make ::
    (Monad i, Monad o) =>
    Maybe (i (Property o Meta.PresentationMode)) ->
    Gui EventMap o ->
    Sugar.Tag (Name o) i o -> Lens.ALens' TextColors Draw.Color ->
    Sugar.Assignment (Name o) i o
    (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    ExprGuiM i o (Gui Responsive o)
make pMode defEventMap tag color assignment =
    do
        Parts mParamsEdit mScopeEdit bodyEdit eventMap wrap rhsId <-
            makeParts Sugar.UnlimitedFuncApply assignment delParamDest
        rhsJumperEquals <-
            ExprGuiM.mkPrejumpPosSaver
            <&> Lens.mapped .~ rhsId
            <&> E.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Equal]
                (E.Doc ["Navigation", "Jump to Def Body"])
        mPresentationEdit <-
            case assignmentBody of
            Sugar.BodyPlain{} -> pure Nothing
            Sugar.BodyFunction x ->
                pMode & sequenceA & ExprGuiM.im
                >>= traverse
                    (PresentationModeEdit.make presentationChoiceId (x ^. Sugar.fParams))
        addFirstParamEventMap <-
            ParamEdit.eventMapAddFirstParam myId (assignmentBody ^. SugarLens.assignmentBodyAddFirstParam)
        defNameEdit <-
            TagEdit.makeBinderTagEdit color tag
            <&> Align.tValue %~ Widget.weakerEvents (rhsJumperEquals <> addFirstParamEventMap)
            <&> (/-/ fromMaybe Element.empty mPresentationEdit)
            <&> Responsive.fromWithTextPos
        mParamEdit <-
            case mParamsEdit of
            Nothing -> pure Nothing
            Just paramsEdit ->
                Responsive.vboxSpaced
                ?? (paramsEdit : fmap Responsive.fromWidget mScopeEdit ^.. Lens._Just)
                <&> Widget.strongerEvents rhsJumperEquals
                <&> Just
        equals <- TextView.makeLabel "="
        hbox <- Options.boxSpaced ?? Options.disambiguationNone
        hbox [ defNameEdit :
                (mParamEdit ^.. Lens._Just) ++
                [Responsive.fromTextView equals]
                & hbox
            , bodyEdit
            ]
            & wrap
            & Widget.weakerEvents (defEventMap <> eventMap)
            & pure
        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = WidgetIds.fromExprPayload pl
        delParamDest = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        Ann pl assignmentBody = assignment
        presentationChoiceId = Widget.joinId myId ["presentation"]
