{-# LANGUAGE RecordWildCards, OverloadedStrings, PatternGuards #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make, diveToNameEdit
    , Parts(..), makeParts
    ) where

import           Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, join)
import           Control.MonadA (MonadA)
import           Data.List.Utils (nonEmptyAll)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Traversable (traverse)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.CharClassification (operatorChars)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Eval.Val (ScopeId)
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), NameSource(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

nonOperatorName :: Name m -> Bool
nonOperatorName (Name NameSourceStored _ _ x) =
    nonEmptyAll (`notElem` operatorChars) x
nonOperatorName _ = False

makeBinderNameEdit ::
    MonadA m =>
    Maybe (Sugar.BinderActions m) ->
    Widget.EventHandlers (Transaction m) ->
    (String, ExprGuiT.SugarExpr m) ->
    Name m -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeBinderNameEdit mBinderActions rhsJumperEquals rhs name myId =
    do
        config <- ExprGuiM.readConfig
        rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
        ExpressionGui.makeNameOriginEdit name myId
            <&> jumpToRHSViaEquals name
            <&> Widget.weakerEvents
                (ParamEdit.eventMapAddFirstParam config mAddFirstParam <> rhsJumper)
            <&> ExpressionGui.fromValueWidget
    where
        mAddFirstParam = mBinderActions ^? Lens._Just . Sugar.baAddFirstParam
        jumpToRHSViaEquals n widget
            | nonOperatorName n =
                widget
                & Widget.eventMap %~ E.filterChars (/= '=')
                & Widget.weakerEvents rhsJumperEquals
            | otherwise = widget

makeWheres ::
    MonadA m =>
    [Sugar.WhereItem (Name m) m (ExprGuiT.SugarExpr m)] -> Widget.Id ->
    ExprGuiM m (Maybe (Widget (T m)))
makeWheres [] _ = return Nothing
makeWheres whereItems myId =
    do
        whereLabel <- ExpressionGui.grammarLabel "where" (Widget.toAnimId myId)
        itemEdits <-
            reverse whereItems
            & ExpressionGui.listWithDelDests myId myId wiCursor
            & traverse makeWhereItemEdit
        ExpressionGui.hboxSpaced
            [ whereLabel
            , ExpressionGui.vboxTopFocal itemEdits
            ]
            <&> Just . (^. ExpressionGui.egWidget)
    where
        wiCursor = WidgetIds.fromEntityId . (^. Sugar.wiEntityId)

presentationModeChoiceConfig :: Config -> Choice.Config
presentationModeChoiceConfig config = Choice.Config
    { Choice.cwcFDConfig =
        FocusDelegator.Config
        { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'Enter]
        , FocusDelegator.focusChildDoc = E.Doc ["Presentation Mode", "Select"]
        , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Enter]
        , FocusDelegator.focusParentDoc = E.Doc ["Presentation Mode", "Choose selected"]
        }
    , Choice.cwcOrientation = Box.vertical
    , Choice.cwcExpandMode = Choice.ExplicitEntry
    , Choice.cwcBgLayer = Config.layerChoiceBG $ Config.layers config
    }

mkPresentationModeEdit ::
        MonadA m => Widget.Id ->
        Transaction.MkProperty m Sugar.PresentationMode ->
        ExprGuiM m (Widget (T m))
mkPresentationModeEdit myId prop = do
    cur <- ExprGuiM.transaction $ Transaction.getP prop
    config <- ExprGuiM.readConfig
    let mkPair presentationMode = do
            widget <-
                ExprGuiM.withFgColor (Config.presentationChoiceColor config) .
                ExprGuiM.widgetEnv $
                BWidgets.makeFocusableLabel (show presentationMode) myId
            return (presentationMode, widget)
    pairs <- traverse mkPair [minBound..maxBound]
    fmap (Widget.scale (realToFrac <$> Config.presentationChoiceScaleFactor config)) .
        ExprGuiM.widgetEnv $
        BWidgets.makeChoiceWidget (Transaction.setP prop) pairs cur
        (presentationModeChoiceConfig config) myId

layout ::
    MonadA m =>
    ExpressionGui m -> [ExpressionGui m] ->
    ExpressionGui m -> Maybe (Widget (T m)) ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
layout defNameEdit paramEdits bodyEdit mWheresEdit myId =
    do
        equals <- ExpressionGui.makeLabel "=" (Widget.toAnimId myId)
        paramsEdit <-
            case paramEdits of
            [] -> return []
            [x] -> return [x]
            xs ->
                xs
                & ExpressionGui.vboxTopFocalSpaced
                >>= ExpressionGui.addValFrame myId
                <&> (:[])
        defNameEdit : paramsEdit ++ [ equals, bodyEdit ]
            & ExpressionGui.hboxSpaced
            <&> ExpressionGui.addBelow 0 (mWheresEdit ^.. Lens._Just <&> (,) 0)

data Parts m = Parts
    { pParamEdits :: [ExpressionGui m]
    , pBodyEdit :: ExpressionGui m
    , pMWheresEdit :: Maybe (Widget (T m))
    , pEventMap :: Widget.EventHandlers (T m)
    }

data ScopeCursor = ScopeCursor
    { sParamScope :: ScopeId
    , sBodyScope :: ScopeId
    , sMPrevParamScope :: Maybe ScopeId
    , sMNextParamScope :: Maybe ScopeId
    }

scopeCursor :: Maybe ScopeId -> [(ScopeId, ScopeId)] -> Maybe ScopeCursor
scopeCursor mChosenScope scopes =
    do
        chosenScope <- mChosenScope
        (prevs, it:nexts) <- break ((== chosenScope) . fst) scopes & Just
        Just ScopeCursor
            { sParamScope = fst it
            , sBodyScope = snd it
            , sMPrevParamScope = reverse prevs ^? Lens.traversed . _1
            , sMNextParamScope = nexts ^? Lens.traversed . _1
            }
    <|> (scopes ^? Lens.traversed <&> def)
    where
        def (paramScope, bodyScope) =
            ScopeCursor
            { sParamScope = paramScope
            , sBodyScope = bodyScope
            , sMPrevParamScope = Nothing
            , sMNextParamScope = scopes ^? Lens.ix 1 . _1
            }

mkScopeCursor ::
    MonadA m =>
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (Maybe ScopeCursor)
mkScopeCursor binder =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        mChosenScope <-
            binder ^. Sugar.bMChosenScopeProp
            & Lens._Just %%~ Transaction.getP
            & ExprGuiM.transaction
            <&> join
        mOuterScopeId
            >>= (`Map.lookup` (binder ^. Sugar.bScopes))
            >>= scopeCursor mChosenScope
            & return

makeScopeEventMap ::
    MonadA m =>
    [ModKey] -> [ModKey] -> (ScopeId -> T m ()) -> ScopeCursor ->
    Widget.EventHandlers (T m)
makeScopeEventMap prevKey nextKey setter cursor =
    do
        (key, doc, scope) <-
            (sMPrevParamScope cursor ^.. Lens._Just <&> (,,) prevKey prevDoc) ++
            (sMNextParamScope cursor ^.. Lens._Just <&> (,,) nextKey nextDoc)
        [setter scope & Widget.keysEventMap key doc]
    & mconcat
    where
        prevDoc = E.Doc ["Evaluation", "Scope", "Previous"]
        nextDoc = E.Doc ["Evaluation", "Scope", "Next"]

makeScopeNavEdit ::
    MonadA m =>
    Widget.Id -> (ScopeId -> T m ()) -> ScopeCursor ->
    ExprGuiM m (Maybe (ExpressionGui m))
makeScopeNavEdit myId setter cursor
    | null scopes = return Nothing
    | otherwise =
        scopes
        <&> fst
        & mapM (`ExpressionGui.grammarLabel` Widget.toAnimId myId)
        >>= ExpressionGui.hboxSpaced
        >>= ExpressionGui.makeFocusableView myId
        <&> ExpressionGui.egWidget %~ Widget.weakerEvents
            (mappend
                (makeScopeEventMap leftKeys rightKeys setter cursor)
                blockEventMap)
        <&> Just
    where
        leftKeys = [ModKey mempty GLFW.Key'Left]
        rightKeys = [ModKey mempty GLFW.Key'Right]
        scopes :: [(String, ScopeId)]
        scopes =
            (sMPrevParamScope cursor ^.. Lens._Just <&> (,) "◀") ++
            (sMNextParamScope cursor ^.. Lens._Just <&> (,) "▶")
        blockEventMap =
            E.keyPresses
            (leftKeys ++ rightKeys)
            (E.Doc ["Navigation", "Move", "(blocked)"]) $
            return mempty

makeParts ::
    MonadA m =>
    ExprGuiT.ShowAnnotation ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) -> Widget.Id ->
    ExprGuiM m (Parts m)
makeParts showAnnotation binder myId =
    do
        mScopeCursor <- mkScopeCursor binder
        let mSetScope =
                binder ^. Sugar.bMChosenScopeProp
                <&> Transaction.setP
                <&> (. Just)
        settings <- ExprGuiM.readSettings
        mScopeNavEdit <-
            guard (settings ^. CESettings.sInfoMode == CESettings.Evaluation) >>
            makeScopeNavEdit scopesNavId <$> mSetScope <*> mScopeCursor
            & Lens.sequenceOf Lens._Just <&> join
        maybe takeNavCursor (const id) mScopeNavEdit $
            do
                let annotationMode =
                        do
                            mScopeNavEdit ^?
                                Lens._Just . ExpressionGui.egWidget . Widget.isFocused
                                >>= guard
                            ExpressionGui.WithNeighbouringAnnotations
                                <$> (mScopeCursor <&> sMPrevParamScope)
                                <*> (mScopeCursor <&> sMNextParamScope)
                        & fromMaybe ExpressionGui.NormalAnnotation
                paramEdits <-
                    makeParamsEdit annotationMode showAnnotation
                    (ExprGuiT.nextHolesBefore body) myId params
                    & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sParamScope)
                bodyEdit <-
                    makeResultEdit (binder ^. Sugar.bMActions) params body
                    & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sBodyScope)
                wheresEdit <-
                    makeWheres (binder ^. Sugar.bWhereItems) myId
                    & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sParamScope)
                config <- ExprGuiM.readConfig <&> Config.eval
                let scopeEventMap =
                        case settings ^. CESettings.sInfoMode of
                        CESettings.Evaluation ->
                            makeScopeEventMap
                            (Config.prevScopeKeys config) (Config.nextScopeKeys config)
                            <$> mSetScope
                            <*> mScopeCursor
                            & fromMaybe mempty
                        _ -> mempty
                Parts (paramEdits ++ (mScopeNavEdit ^.. Lens.traversed))
                    bodyEdit wheresEdit scopeEventMap
                    & return
    where
        takeNavCursor = ExprGuiM.assignCursorPrefix scopesNavId (const destId)
        destId =
            params ^? SugarLens.binderParams . Sugar.fpId
            & fromMaybe bodyId
            & WidgetIds.fromEntityId
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody
        bodyId = body ^. Sugar.rPayload . Sugar.plEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]

make ::
    MonadA m =>
    Name m ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
make name binder myId =
    do
        Parts paramEdits bodyEdit mWheresEdit eventMap <-
            makeParts ExprGuiT.ShowAnnotation binder myId
        rhsJumperEquals <- jumpToRHS [ModKey mempty GLFW.Key'Equal] rhs
        presentationEdits <-
            binder ^.. Sugar.bMPresentationModeProp . Lens._Just
            & traverse (mkPresentationModeEdit presentationChoiceId)
        defNameEdit <-
            makeBinderNameEdit (binder ^. Sugar.bMActions)
            rhsJumperEquals rhs name myId
            <&> ExpressionGui.addBelow 0 (map ((,) 0) presentationEdits)
        layout defNameEdit
            (paramEdits & Lens.mapped . ExpressionGui.egWidget
                %~ Widget.weakerEvents rhsJumperEquals)
            bodyEdit mWheresEdit myId
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
    where
        presentationChoiceId = Widget.joinId myId ["presentation"]
        rhs = ("Def Body", body)
        body = binder ^. Sugar.bBody

makeWhereItemEdit ::
    MonadA m =>
    (Widget.Id, Widget.Id, Sugar.WhereItem (Name m) m (ExprGuiT.SugarExpr m)) ->
    ExprGuiM m (ExpressionGui m)
makeWhereItemEdit (_prevId, nextId, item) =
    do
        config <- ExprGuiM.readConfig
        let eventMap
                | Just wiActions <- item ^. Sugar.wiActions =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.delKeys config)
                    (E.Doc ["Edit", "Where clause", "Delete"]) $
                    nextId <$ wiActions ^. Sugar.wiDelete
                , Widget.keysEventMapMovesCursor
                    (Config.whereAddItemKeys config)
                    (E.Doc ["Edit", "Where clause", "Add next"]) $
                    WidgetIds.fromEntityId <$>
                    wiActions ^. Sugar.wiAddNext
                , Widget.keysEventMapMovesCursor (Config.extractKeys config)
                    (E.Doc ["Edit", "Where clause", "Extract to outer scope"]) $
                    WidgetIds.fromEntityId <$>
                    wiActions ^. Sugar.wiExtract
                ]
                | otherwise = mempty
        mBodyScopeId <- ExprGuiM.readMScopeId
        make
            (item ^. Sugar.wiName)
            (item ^. Sugar.wiValue)
            (WidgetIds.fromEntityId (item ^. Sugar.wiEntityId))
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
            <&> ExpressionGui.pad
                (Config.whereItemPadding config <&> realToFrac)
            & ExprGuiM.withLocalMScopeId
                (mBodyScopeId >>= (`Map.lookup` (item ^. Sugar.wiScopes)))

jumpToRHS ::
    (MonadA m, MonadA f) =>
    [ModKey] -> (String, ExprGuiT.SugarExpr m) ->
    ExprGuiM f (Widget.EventHandlers (T f))
jumpToRHS keys (rhsDoc, rhs) = do
    savePos <- ExprGuiM.mkPrejumpPosSaver
    return $
        Widget.keysEventMapMovesCursor keys (E.Doc ["Navigation", "Jump to " ++ rhsDoc]) $
            rhsId <$ savePos
    where
        rhsId = WidgetIds.fromExprPayload $ rhs ^. Sugar.rPayload

makeResultEdit ::
    MonadA m =>
    Maybe (Sugar.BinderActions m) ->
    Sugar.BinderParams name m ->
    ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
makeResultEdit mActions params result = do
    savePos <- ExprGuiM.mkPrejumpPosSaver
    config <- ExprGuiM.readConfig
    let jumpToLhsEventMap =
            case params of
            Sugar.DefintionWithoutParams -> mempty
            Sugar.NullParam _ -> mempty
            Sugar.VarParam param ->
                Widget.keysEventMapMovesCursor
                (Config.jumpRHStoLHSKeys config) (E.Doc ["Navigation", "Jump to param"]) $
                WidgetIds.fromEntityId (param ^. Sugar.fpId) <$ savePos
            Sugar.FieldParams ps ->
                Widget.keysEventMapMovesCursor
                (Config.jumpRHStoLHSKeys config) (E.Doc ["Navigation", "Jump to last param"]) $
                WidgetIds.fromEntityId (last ps ^. _2 . Sugar.fpId) <$ savePos
        addWhereItemEventMap actions =
            Widget.keysEventMapMovesCursor (Config.whereAddItemKeys config)
            (E.Doc ["Edit", "Where clause", "Add first"]) .
            fmap (diveToNameEdit . WidgetIds.fromEntityId) $
            savePos >> actions ^. Sugar.baAddInnermostWhereItem
    ExprGuiM.makeSubexpression 0 result
        <&> ExpressionGui.egWidget %~
                Widget.weakerEvents
                (jumpToLhsEventMap <> maybe mempty addWhereItemEventMap mActions)

makeNullLambdaActions ::
    MonadA m =>
    Widget.Id -> Sugar.NullParamActions m ->
    ExprGuiM m (Widget.EventHandlers (Transaction m))
makeNullLambdaActions dstId actions =
    do
        config <- ExprGuiM.readConfig
        actions ^. Sugar.npDeleteLambda
            & Lens.mapped .~ dstId
            & Widget.keysEventMapMovesCursor
                (Config.delKeys config) (E.Doc ["Edit", "Delete lambda"])
            & return

makeParamsEdit ::
    MonadA m =>
    ExpressionGui.AnnotationOptions -> ExprGuiT.ShowAnnotation ->
    NearestHoles -> Widget.Id -> Sugar.BinderParams (Name m) m ->
    ExprGuiM m [ExpressionGui m]
makeParamsEdit annotationOpts showAnnotation nearestHoles lhsId params =
    case params of
    Sugar.DefintionWithoutParams -> return []
    Sugar.NullParam mActions ->
        do
            actions <-
                maybe (return mempty) (makeNullLambdaActions lhsId) mActions
            ExpressionGui.grammarLabel "◗" (Widget.toAnimId lhsId)
                >>= ExpressionGui.makeFocusableView (Widget.joinId lhsId ["param"])
                <&> ExpressionGui.egWidget %~ Widget.weakerEvents actions
                <&> (:[])
    Sugar.VarParam p -> fromParamList [p]
    Sugar.FieldParams ps -> ps ^.. Lens.traversed . _2 & fromParamList
    where
        fromParamList paramList =
            do
                jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
                let mkParam (prevId, nextId, param) =
                        ParamEdit.make annotationOpts showAnnotation prevId nextId param
                        <&> ExpressionGui.egWidget
                        %~ Widget.weakerEvents jumpHolesEventMap
                ExpressionGui.listWithDelDests lhsId lhsId
                    (WidgetIds.fromEntityId . (^. Sugar.fpId)) paramList
                    & traverse mkParam

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = ExpressionGui.diveToNameEdit -- Name editor
