{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, PatternGuards, LambdaCase #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make
    , Parts(..), makeParts
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, join)
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev, current)
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
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
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
    CurAndPrev (Maybe Sugar.BinderParamScopeId) ->
    [Sugar.LetItem (Name m) m (ExprGuiT.SugarExpr m)] -> Widget.Id ->
    ExprGuiM m [ExpressionGui m]
makeLets mBinderParamScopeId letItems myId =
    ExpressionGui.listWithDelDests myId myId liCursor letItems
    & traverse (makeLetItemEdit mBinderParamScopeId)
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
    , pMScopesEdit :: Maybe (ExpressionGui m)
    , pBodyEdit :: ExpressionGui m
    , pEventMap :: Widget.EventHandlers (T m)
    }

data ScopeCursor = ScopeCursor
    { sBinderScopes :: Sugar.BinderScopes
    , sMPrevParamScope :: Maybe Sugar.BinderParamScopeId
    , sMNextParamScope :: Maybe Sugar.BinderParamScopeId
    }

scopeCursor :: Maybe Sugar.BinderParamScopeId -> [Sugar.BinderScopes] -> Maybe ScopeCursor
scopeCursor mChosenScope scopes =
    do
        chosenScope <- mChosenScope
        (prevs, it:nexts) <- break ((== chosenScope) . (^. Sugar.bsParamScope)) scopes & Just
        Just ScopeCursor
            { sBinderScopes = it
            , sMPrevParamScope = reverse prevs ^? Lens.traversed . Sugar.bsParamScope
            , sMNextParamScope = nexts ^? Lens.traversed . Sugar.bsParamScope
            }
    <|> (scopes ^? Lens.traversed <&> def)
    where
        def binderScopes =
            ScopeCursor
            { sBinderScopes = binderScopes
            , sMPrevParamScope = Nothing
            , sMNextParamScope = scopes ^? Lens.ix 1 . Sugar.bsParamScope
            }

mkScopeCursor ::
    MonadA m =>
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (CurAndPrev (Maybe ScopeCursor))
mkScopeCursor binder =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        mChosenScope <-
            binder ^. Sugar.bMChosenScopeProp
            & Lens._Just %%~ Transaction.getP
            & ExprGuiM.transaction
            <&> join
        liftA2 Map.lookup
            <$> mOuterScopeId
            <*> (binder ^. Sugar.bScopes <&> Just)
            <&> join
            <&> (>>= scopeCursor mChosenScope)
            & return

makeScopeEventMap ::
    MonadA m =>
    [ModKey] -> [ModKey] -> (Sugar.BinderParamScopeId -> T m ()) -> ScopeCursor ->
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
    Widget.Id -> Maybe (Sugar.BinderParamScopeId -> T m (), ScopeCursor) ->
    ExprGuiM m (ExpressionGui m)
makeScopeNavEdit myId mSetterAndCursor =
    mapM mkArrow scopes
    >>= ExpressionGui.hboxSpaced
    >>= ExpressionGui.makeFocusableView myId
    <&>
    case mSetterAndCursor of
    Nothing -> id
    Just (setter, cursor) ->
        ExpressionGui.egWidget %~ Widget.weakerEvents
        (mappend
            (makeScopeEventMap leftKeys rightKeys setter cursor)
            blockEventMap)
    where
        mCursor = mSetterAndCursor ^? Lens._Just . _2
        mkArrow (txt, mScopeId) =
            do
                config <- ExprGuiM.readConfig
                ExpressionGui.makeLabel txt (Widget.toAnimId myId)
                    & ExprGuiM.localEnv
                    ( WE.setTextSizeColor
                        (Config.navArrowsSize config)
                        ( case mScopeId of
                            Nothing -> Config.disabledColor config
                            Just _ -> Config.grammarColor config
                        )
                    )
        leftKeys = [ModKey mempty GLFW.Key'Left]
        rightKeys = [ModKey mempty GLFW.Key'Right]
        scopes :: [(String, Maybe Sugar.BinderParamScopeId)]
        scopes =
            [ ("◀", mCursor >>= sMPrevParamScope)
            , ("▶", mCursor >>= sMNextParamScope)
            ]
        blockEventMap =
            E.keyPresses
            (leftKeys ++ rightKeys)
            (E.Doc ["Navigation", "Move", "(blocked)"]) $
            return mempty

sParamScope :: ScopeCursor -> Sugar.BinderParamScopeId
sParamScope = (^. Sugar.bsParamScope) . sBinderScopes

sBodyScope :: ScopeCursor -> ScopeId
sBodyScope = (^. Sugar.bsBodyScope) . sBinderScopes

makeMParamsEdit ::
    MonadA m =>
    CurAndPrev (Maybe ScopeCursor) -> Maybe (ExpressionGui n) ->
    Widget.Id -> Widget.Id ->
    ExprGuiT.SugarExpr m -> Sugar.BinderParams (Name m) m ->
    ExprGuiM m (Maybe (ExpressionGui m))
makeMParamsEdit mScopeCursor mScopeNavEdit delVarBackwardsId myId body params =
    params
    & makeParamsEdit annotationMode nearestHoles
      delVarBackwardsId myId (WidgetIds.fromEntityId bodyId)
    & ExprGuiM.withLocalMScopeId
      ( mScopeCursor
      <&> Lens.traversed %~ (^. Sugar.bParamScopeId) . sParamScope
      )
    >>= \case
    [] -> return Nothing
    paramEdits ->
        paramEdits
        <&> ExpressionGui.egAlignment . _1 .~ 0.5
        & ExpressionGui.vboxTopFocalSpaced
        >>= case params of
            Sugar.FieldParams{} -> ExpressionGui.addValFrame myId
            _ -> return
        <&> Just
    where
        bodyId = body ^. Sugar.rPayload . Sugar.plEntityId
        curCursor = mScopeCursor ^. current
        nearestHoles = ExprGuiT.nextHolesBefore body
        annotationMode =
            do
                mScopeNavEdit ^?
                    Lens._Just . ExpressionGui.egWidget . Widget.isFocused
                    >>= guard
                ExpressionGui.NeighborVals
                    <$> (curCursor <&> sMPrevParamScope)
                    <*> (curCursor <&> sMNextParamScope)
                    <&> ExpressionGui.WithNeighbouringEvalAnnotations
            & fromMaybe ExpressionGui.NormalEvalAnnotation

makeParts ::
    MonadA m =>
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Widget.Id -> Widget.Id ->
    ExprGuiM m (Parts m)
makeParts binder delVarBackwardsId myId =
    do
        mScopeCursor <- mkScopeCursor binder
        let mSetScope =
                binder ^. Sugar.bMChosenScopeProp
                <&> Transaction.setP
                <&> (. Just)
        settings <- ExprGuiM.readSettings
        let curCursor = mScopeCursor ^. current
        mScopeNavEdit <-
            case settings ^. CESettings.sInfoMode of
            CESettings.Evaluation ->
                makeScopeNavEdit scopesNavId ((,) <$> mSetScope <*> curCursor)
                <&> Just
            _ -> return Nothing
        maybe takeNavCursor (const id) mScopeNavEdit $
            do
                mParamsEdit <- makeMParamsEdit mScopeCursor mScopeNavEdit delVarBackwardsId myId body params
                bodyEdit <-
                    makeResultEdit (binder ^. Sugar.bMActions) params body
                    & ExprGuiM.withLocalMScopeId (mScopeCursor <&> Lens.traversed %~ sBodyScope)
                letEdits <- makeLets (mScopeCursor <&> Lens.traversed %~ sParamScope) (binder ^. Sugar.bLetItems) myId
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
                            <*> curCursor
                            & fromMaybe mempty
                        _ -> mempty
                Parts mParamsEdit mScopeNavEdit rhs scopeEventMap & return
    where
        takeNavCursor = ExprGuiM.assignCursorPrefix scopesNavId (const destId)
        destId =
            params ^? SugarLens.binderNamedParams . Sugar.fpId
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
        Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            makeParts binder myId myId
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
        mLhsEdit <-
            mParamsEdit
            <&> (: mScopeEdit ^.. Lens._Just)
            <&> Lens.traversed . ExpressionGui.egAlignment . _1 .~ 0.5
            & Lens._Just ExpressionGui.vboxTopFocalSpaced
            <&> Lens._Just . ExpressionGui.egWidget
                %~ Widget.weakerEvents rhsJumperEquals
        layout defNameEdit mLhsEdit bodyEdit myId
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
    where
        nearestHoles = ExprGuiT.nextHolesBefore (binder ^. Sugar.bBody)
        presentationChoiceId = Widget.joinId myId ["presentation"]
        rhs = ("Def Body", body)
        body = binder ^. Sugar.bBody

makeLetItemEdit ::
    MonadA m =>
    CurAndPrev (Maybe Sugar.BinderParamScopeId) ->
    (Widget.Id, Widget.Id, Sugar.LetItem (Name m) m (ExprGuiT.SugarExpr m)) ->
    ExprGuiM m (ExpressionGui m)
makeLetItemEdit mBinderParamScopeId (_prevId, nextId, item) =
    do
        config <- ExprGuiM.readConfig
        let eventMap
                | Just liActions <- item ^. Sugar.liActions =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.delKeys config)
                    (E.Doc ["Edit", "Let clause", "Delete"]) $
                    nextId <$ liActions ^. Sugar.liDelete
                , Widget.keysEventMapMovesCursor
                    (Config.letAddItemKeys config)
                    (E.Doc ["Edit", "Let clause", "Add next"]) $
                    WidgetIds.fromEntityId <$>
                    liActions ^. Sugar.liAddNext
                , Widget.keysEventMapMovesCursor (Config.extractKeys config)
                    (E.Doc ["Edit", "Let clause", "Extract to outer scope"]) $
                    WidgetIds.fromEntityId <$>
                    liActions ^. Sugar.liExtract
                ]
                | otherwise = mempty
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
                ( liftA2 Map.lookup
                <$> mBinderParamScopeId
                <*> (item ^. Sugar.liScopes <&> Just)
                <&> join
                )
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
            Widget.keysEventMapMovesCursor (Config.letAddItemKeys config)
            (E.Doc ["Edit", "Let clause", "Add first"]) .
            fmap (WidgetIds.nameEditOf . WidgetIds.fromEntityId) $
            savePos >> actions ^. Sugar.baAddInnermostLetItem
    ExprGuiM.makeSubexpression (const 0) result
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
    ExpressionGui.EvalAnnotationOptions -> NearestHoles ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Name m) m ->
    ExprGuiM m [ExpressionGui m]
makeParamsEdit annotationOpts nearestHoles delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.DefintionWithoutParams -> return []
    Sugar.NullParam p ->
        fromParamList ExprGuiT.showAnnotationWhenVerbose delVarBackwardsId rhsId
        [p & Sugar.fpInfo %~ nullParamEditInfo]
    Sugar.VarParam p ->
        fromParamList ExprGuiT.alwaysShowAnnotations delVarBackwardsId rhsId [p & Sugar.fpInfo %~ namedParamEditInfo]
    Sugar.FieldParams ps ->
        ps ^.. Lens.traversed . _2
        & traverse . Sugar.fpInfo %~ namedParamEditInfo
        & fromParamList ExprGuiT.alwaysShowAnnotations lhsId rhsId
    where
        fromParamList showParamAnnotation delDestFirst delDestLast paramList =
            do
                jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
                let mkParam (prevId, nextId, param) =
                        ParamEdit.make annotationOpts showParamAnnotation prevId nextId param
                        <&> ExpressionGui.egWidget
                        %~ Widget.weakerEvents jumpHolesEventMap
                ExpressionGui.listWithDelDests delDestFirst delDestLast
                    (WidgetIds.fromEntityId . (^. Sugar.fpId)) paramList
                    & traverse mkParam
