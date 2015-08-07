{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, PatternGuards #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make, diveToNameEdit
    , Parts(..), makeParts
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, join)
import           Control.MonadA (MonadA)
import           Data.List.Utils (nonEmptyAll)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
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

import           Prelude.Compat

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

makeLets ::
    MonadA m =>
    [Sugar.LetItem (Name m) m (ExprGuiT.SugarExpr m)] -> Widget.Id ->
    ExprGuiM m [ExpressionGui m]
makeLets letItems myId =
    ExpressionGui.listWithDelDests myId myId liCursor letItems
    & traverse makeLetItemEdit
    where
        liCursor = WidgetIds.fromEntityId . (^. Sugar.liEntityId)

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
    pairs <- traverse mkPair [Sugar.OO, Sugar.Verbose, Sugar.Infix 5]
    fmap (Widget.scale (realToFrac <$> Config.presentationChoiceScaleFactor config)) .
        ExprGuiM.widgetEnv $
        BWidgets.makeChoiceWidget (Transaction.setP prop) pairs cur
        (presentationModeChoiceConfig config) myId

layout ::
    MonadA m =>
    ExpressionGui m -> Maybe (ExpressionGui m) -> ExpressionGui m -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
layout defNameEdit mParamsEdit bodyEdit myId =
    do
        equals <- ExpressionGui.makeLabel "=" (Widget.toAnimId myId)
        defNameEdit : (mParamsEdit ^.. Lens._Just) ++ [equals, bodyEdit]
            & ExpressionGui.hboxSpaced

data Parts m = Parts
    { pMParamsEdit :: Maybe (ExpressionGui m)
    , pBodyEdit :: ExpressionGui m
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
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Widget.Id -> Widget.Id ->
    ExprGuiM m (Parts m)
makeParts showAnnotation binder delVarBackwardsId myId =
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
                    makeParamsEdit annotationMode showAnnotation nearestHoles
                    delVarBackwardsId myId (WidgetIds.fromEntityId bodyId)
                    params
                    & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sParamScope)
                    <&> (++ (mScopeNavEdit ^.. Lens.traversed))
                mParamsEdit <-
                    case paramEdits of
                    [] -> return Nothing
                    _ ->
                        paramEdits
                        <&> ExpressionGui.egAlignment . _1 .~ 0.5
                        & ExpressionGui.vboxTopFocalSpaced
                        >>= case params of
                            Sugar.FieldParams{} -> ExpressionGui.addValFrame myId
                            _ -> return
                        <&> Just
                bodyEdit <-
                    makeResultEdit (binder ^. Sugar.bMActions) params body
                    & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sBodyScope)
                letEdits <-
                    makeLets (binder ^. Sugar.bLetItems) myId
                    & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sParamScope)
                rhs <-
                    letEdits ++ [bodyEdit]
                    <&> ExpressionGui.egAlignment . _1 .~ 0
                    & ExpressionGui.vboxTopFocalSpaced
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
                Parts mParamsEdit rhs scopeEventMap & return
    where
        nearestHoles = ExprGuiT.nextHolesBefore body
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
        Parts mParamsEdit bodyEdit eventMap <-
            makeParts ExprGuiT.ShowAnnotation binder myId myId
        rhsJumperEquals <- jumpToRHS [ModKey mempty GLFW.Key'Equal] rhs
        presentationEdits <-
            binder ^.. Sugar.bMPresentationModeProp . Lens._Just
            & traverse (mkPresentationModeEdit presentationChoiceId)
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        defNameEdit <-
            makeBinderNameEdit (binder ^. Sugar.bMActions)
            rhsJumperEquals rhs name myId
            <&> ExpressionGui.addBelow 0 (map ((,) 0) presentationEdits)
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents jumpHolesEventMap
        layout defNameEdit
            (mParamsEdit & Lens.mapped . ExpressionGui.egWidget
                %~ Widget.weakerEvents rhsJumperEquals)
            bodyEdit myId
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
    where
        nearestHoles = ExprGuiT.nextHolesBefore (binder ^. Sugar.bBody)
        presentationChoiceId = Widget.joinId myId ["presentation"]
        rhs = ("Def Body", body)
        body = binder ^. Sugar.bBody

makeLetItemEdit ::
    MonadA m =>
    (Widget.Id, Widget.Id, Sugar.LetItem (Name m) m (ExprGuiT.SugarExpr m)) ->
    ExprGuiM m (ExpressionGui m)
makeLetItemEdit (_prevId, nextId, item) =
    do
        config <- ExprGuiM.readConfig
        let eventMap
                | Just liActions <- item ^. Sugar.liActions =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.delKeys config)
                    (E.Doc ["Edit", "Let clause", "Delete"]) $
                    nextId <$ liActions ^. Sugar.liDelete
                , Widget.keysEventMapMovesCursor
                    (Config.whereAddItemKeys config)
                    (E.Doc ["Edit", "Let clause", "Add next"]) $
                    WidgetIds.fromEntityId <$>
                    liActions ^. Sugar.liAddNext
                , Widget.keysEventMapMovesCursor (Config.extractKeys config)
                    (E.Doc ["Edit", "Let clause", "Extract to outer scope"]) $
                    WidgetIds.fromEntityId <$>
                    liActions ^. Sugar.liExtract
                ]
                | otherwise = mempty
        mBodyScopeId <- ExprGuiM.readMScopeId
        jumpHolesEventMap <-
            binder ^. Sugar.bBody
            & ExprGuiT.nextHolesBefore & ExprEventMap.jumpHolesEventMap
        edit <-
            make
            (item ^. Sugar.liName)
            binder
            myId
            <&> ExpressionGui.egWidget
                %~ Widget.weakerEvents (mappend jumpHolesEventMap eventMap)
            <&> ExpressionGui.pad
                (Config.letItemPadding config <&> realToFrac)
            & ExprGuiM.withLocalMScopeId
                (mBodyScopeId >>= (`Map.lookup` (item ^. Sugar.liScopes)))
            <&> ExpressionGui.egAlignment . _1 .~ 0
        letLabel <- ExpressionGui.grammarLabel "let" (Widget.toAnimId myId)
        ExpressionGui.hboxSpaced [letLabel, edit]
    where
        myId = item ^. Sugar.liEntityId & WidgetIds.fromEntityId
        binder = item ^. Sugar.liValue

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
        addLetItemEventMap actions =
            Widget.keysEventMapMovesCursor (Config.whereAddItemKeys config)
            (E.Doc ["Edit", "Let clause", "Add first"]) .
            fmap (diveToNameEdit . WidgetIds.fromEntityId) $
            savePos >> actions ^. Sugar.baAddInnermostLetItem
    ExprGuiM.makeSubexpression 0 result
        <&> ExpressionGui.egWidget %~
                Widget.weakerEvents
                (jumpToLhsEventMap <> maybe mempty addLetItemEventMap mActions)

namedParamEditInfo :: MonadA m => Sugar.NamedParamInfo (Name m) m -> ParamEdit.Info m
namedParamEditInfo paramInfo =
    ParamEdit.Info
    { ParamEdit.iMakeNameEdit =
      ExpressionGui.makeNameOriginEdit (paramInfo ^. Sugar.npiName)
      <&> Lens.mapped %~ ExpressionGui.fromValueWidget
    , ParamEdit.iMAddNext =
      paramInfo ^? Sugar.npiMActions . Lens._Just . Sugar.fpAddNext
    , ParamEdit.iMDel =
      paramInfo ^? Sugar.npiMActions . Lens._Just . Sugar.fpDelete
    }

nullParamEditInfo :: MonadA m => Sugar.NullParamInfo m -> ParamEdit.Info m
nullParamEditInfo (Sugar.NullParamInfo mActions) =
    ParamEdit.Info
    { ParamEdit.iMakeNameEdit =
      \myId ->
      ExpressionGui.grammarLabel "◗" (Widget.toAnimId myId)
      >>= ExpressionGui.makeFocusableView myId
    , ParamEdit.iMAddNext = Nothing
    , ParamEdit.iMDel =
      mActions ^? Lens._Just . Sugar.npDeleteLambda
      <&> Lens.mapped .~ Sugar.ParamDelResultDelVar
    }

makeParamsEdit ::
    MonadA m =>
    ExpressionGui.AnnotationOptions -> ExprGuiT.ShowAnnotation -> NearestHoles ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Name m) m ->
    ExprGuiM m [ExpressionGui m]
makeParamsEdit annotationOpts showAnnotation nearestHoles delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.DefintionWithoutParams -> return []
    Sugar.NullParam p ->
        fromParamList delVarBackwardsId rhsId [p & Sugar.fpInfo %~ nullParamEditInfo]
    Sugar.VarParam p ->
        fromParamList delVarBackwardsId rhsId [p & Sugar.fpInfo %~ namedParamEditInfo]
    Sugar.FieldParams ps ->
        ps ^.. Lens.traversed . _2
        & traverse . Sugar.fpInfo %~ namedParamEditInfo
        & fromParamList lhsId rhsId
    where
        fromParamList delDestFirst delDestLast paramList =
            do
                jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
                let mkParam (prevId, nextId, param) =
                        ParamEdit.make annotationOpts showAnnotation prevId nextId param
                        <&> ExpressionGui.egWidget
                        %~ Widget.weakerEvents jumpHolesEventMap
                ExpressionGui.listWithDelDests delDestFirst delDestLast
                    (WidgetIds.fromEntityId . (^. Sugar.fpId)) paramList
                    & traverse mkParam

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = ExpressionGui.diveToNameEdit -- Name editor
