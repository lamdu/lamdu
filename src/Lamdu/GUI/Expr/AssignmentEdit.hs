module Lamdu.GUI.Expr.AssignmentEdit
    ( make
    , Parts(..), makeFunctionParts
    , makeJumpToRhs
    , layout, makePlainLhsEventMap
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev, fallbackToPrev)
import qualified Data.Map as Map
import qualified Data.Property as Property
import           GUI.Momentu (ModKey(..), noMods)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (subAnimId)
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.FocusDirection as Direction
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.ParamsEdit as ParamsEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (grammar, label)
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

data Parts o = Parts
    { pAddFirstEventMap :: EventMap (o M.Update)
    , pParamsEdit :: Responsive o
    , pMScopesEdit :: Maybe (M.Widget o)
    , pScopeEventMap :: EventMap (o M.Update)
    , pScopeId :: CurAndPrev (Maybe Sugar.ScopeId)
    }

readFunctionChosenScope ::
    Functor i => Sugar.Function v name i o expr -> i (Maybe Sugar.BinderParamScopeId)
readFunctionChosenScope func = func ^. Sugar.fChosenScopeProp <&> Property.value

lookupMKey :: Ord k => Maybe k -> Map k a -> Maybe a
lookupMKey k m = k >>= (`Map.lookup` m)

mkChosenScopeCursor ::
    Monad i =>
    Sugar.Body Sugar.Function v Name i o ->
    GuiM env i o (CurAndPrev (Maybe ParamsEdit.ScopeCursor))
mkChosenScopeCursor func =
    do
        mOuterScopeId <- GuiM.readMScopeId
        readFunctionChosenScope func & GuiM.im <&>
            \mChosenScope ->
            liftA2 lookupMKey mOuterScopeId (func ^. Sugar.fBodyScopes)
            <&> (>>= ParamsEdit.scopeCursor mChosenScope)

makeScopeEventMap ::
    _ =>
    env -> ParamsEdit.ScopeCursor -> (Sugar.BinderParamScopeId -> o ()) -> [ModKey] -> [ModKey] ->
    EventMap (o M.Update)
makeScopeEventMap env cursor setter prevKeys nextKeys =
    mkEventMap (ParamsEdit.sMPrevParamScope, prevKeys, Texts.prev) ++
    mkEventMap (ParamsEdit.sMNextParamScope, nextKeys, Texts.next)
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
    Sugar.Function v name i o expr -> Widget.Id -> ParamsEdit.ScopeCursor ->
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
                [ (env ^. has . Texts.prevScopeArrow, ParamsEdit.sMPrevParamScope curCursor)
                , (" ", Nothing)
                , (env ^. has . Texts.nextScopeArrow, ParamsEdit.sMNextParamScope curCursor)
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

makeFunctionParts ::
    _ =>
    Sugar.FuncApplyLimit -> ExprGui.Expr Sugar.Function i o -> Widget.Id -> GuiM env i o (Parts o)
makeFunctionParts funcApplyLimit (Ann (Const pl) func) delVarBackwardsId =
    do
        mScopeCursor <- mkChosenScopeCursor func
        let binderScopeId = mScopeCursor <&> Lens.mapped %~ (^. Sugar.bParamScopeId) . ParamsEdit.sBinderScope
        (scopeEventMap, mScopeNavEdit) <-
            do
                guard (funcApplyLimit == Sugar.UnlimitedFuncApply)
                scope <- fallbackToPrev mScopeCursor
                guard $
                    Lens.nullOf (Sugar.fParams . Sugar._LhsVar . Sugar.vIsNullParam . Lens.only True) func ||
                    Lens.has (Lens.traversed . Lens._Just) [ParamsEdit.sMPrevParamScope scope, ParamsEdit.sMNextParamScope scope]
                Just scope
                & maybe (pure (mempty, Nothing)) (makeScopeNavEdit func scopesNavId)
        let isScopeNavFocused =
                case mScopeNavEdit of
                Just edit | Widget.isFocused edit -> ParamsEdit.ScopeNavIsFocused
                _ -> ParamsEdit.ScopeNavNotFocused
        do
            (lhsEventMap, paramsEdit) <-
                ParamsEdit.make False mScopeCursor isScopeNavFocused delVarBackwardsId myId
                bodyId (func ^. Sugar.fParams)
            Parts lhsEventMap paramsEdit mScopeNavEdit scopeEventMap binderScopeId & pure
            & case mScopeNavEdit of
              Nothing -> GuiState.assignCursorPrefix scopesNavId (const destId)
              Just _ -> id
    where
        myId = WidgetIds.fromExprPayload pl
        destId =
            case func ^. Sugar.fParams of
            Sugar.LhsVar v
                | v ^. Sugar.vIsNullParam -> bodyId
                | otherwise ->
                    v ^. Sugar.vTag . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
            Sugar.LhsRecord ps ->
                ps ^?! SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagInstance
                & WidgetIds.fromEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]
        bodyId = func ^. Sugar.fBody . annotation & WidgetIds.fromExprPayload

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

layout :: _ => Responsive o -> Responsive o -> m (Responsive o)
layout lhs body =
    do
        space <- Spacer.stdHSpace <&> Responsive.fromView
        indent <- ResponsiveExpr.indent
        indentId <- subAnimId ?? ["assignment-body"]
        hbox <- Options.hbox ?? id ?? id
        Responsive.vboxSpaced
            ?? [lhs, indent indentId body]
            <&> Options.tryWideLayout hbox [lhs, space, body]

makePlainLhsEventMap ::
    _ => o Sugar.EntityId -> Widget.Id -> GuiM env i o (EventMap (o GuiState.Update))
makePlainLhsEventMap addFirstParam rhsId =
    do
        env <- Lens.view id
        let addParam =
                E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.parameter, has . Texts.add])
                (addFirstParam <&> WidgetIds.tagHoleId . WidgetIds.fromEntityId)
        makeJumpToRhs rhsId <&> (<> addParam)

-- The given nameEdit may represent an LhsRecord, hence it is a Responsive and not a simple widget
make ::
    _ =>
    Widget.Id ->
    ExprGui.Expr Sugar.Assignment i o ->
    Responsive o ->
    GuiM env i o (Responsive o)
make delParamDest assignment nameEdit =
    do
        case assignmentBody of
            Sugar.BodyPlain x ->
                do
                    lhsEventMap <-
                        makePlainLhsEventMap (x ^. Sugar.apAddFirstParam) (WidgetIds.fromExprPayload pl)
                    equals <- grammar (label Texts.assign) <&> Responsive.fromTextView
                    assignmentPrefix <-
                        Options.boxSpaced ?? Options.disambiguationNone
                        ?? [Widget.weakerEvents lhsEventMap nameEdit, equals]
                    x ^. Sugar.apBody & Ann (Const (assignment ^. annotation)) & GuiM.makeBinder
                        >>= layout assignmentPrefix
            Sugar.BodyFunction x ->
                do
                    Parts lhsEventMap paramsEdit mScopeEdit eventMap scopeId <-
                        makeFunctionParts Sugar.UnlimitedFuncApply (Ann (Const pl) x) delParamDest
                    bodyEdit <- x ^. Sugar.fBody & GuiM.makeBinder & GuiM.withLocalMScopeId scopeId
                    rhsJumperEquals <- x ^. Sugar.fBody . annotation & WidgetIds.fromExprPayload & makeJumpToRhs
                    equals <- grammar (label Texts.assign) <&> Responsive.fromTextView
                    paramScopeEdit <-
                        Responsive.vboxSpaced
                        ?? (paramsEdit : fmap Responsive.fromWidget mScopeEdit ^.. Lens._Just)
                        <&> Widget.strongerEvents rhsJumperEquals
                    assignmentPrefix <-
                        Options.boxSpaced ?? Options.disambiguationNone
                        ?? [Widget.weakerEvents (rhsJumperEquals <> lhsEventMap) nameEdit, paramScopeEdit, equals]
                    layout assignmentPrefix bodyEdit
                        & stdWrap pl
                        <&> Widget.weakerEvents eventMap
    & local (M.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = WidgetIds.fromExprPayload pl
        Ann (Const pl) assignmentBody = assignment
