{-# LANGUAGE NamedFieldPuns, RecordWildCards, NoImplicitPrelude, OverloadedStrings, PatternGuards, LambdaCase #-}
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
import           Data.Map (Map)
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
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..), NameSource(..))
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
    (String, Sugar.EntityId) ->
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
    MonadA m =>
    Sugar.Binder name m expr -> ExprGuiM m (Maybe Sugar.BinderParamScopeId)
readBinderChosenScope binder =
    binder ^. Sugar.bMChosenScopeProp
    & Lens._Just %%~ Transaction.getP
    & ExprGuiM.transaction
    <&> join

mkChosenScopeCursor ::
    MonadA m =>
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (CurAndPrev (Maybe ScopeCursor))
mkChosenScopeCursor binder =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        case binder ^. Sugar.bBodyScopes of
            Sugar.SameAsParentScope ->
                mOuterScopeId <&> fmap (trivialScopeCursor . Sugar.BinderParamScopeId) & return
            Sugar.BinderBodyScope binderBodyScope ->
                do
                    mChosenScope <- readBinderChosenScope binder
                    liftA2 lookupMKey mOuterScopeId binderBodyScope
                        <&> (>>= scopeCursor mChosenScope) & return

makeScopeEventMap ::
    MonadA m =>
    [ModKey] -> [ModKey] -> ScopeCursor -> (Sugar.BinderParamScopeId -> T m ()) ->
    Widget.EventHandlers (T m)
makeScopeEventMap prevKey nextKey cursor setter =
    do
        (key, doc, scope) <-
            (sMPrevParamScope cursor ^.. Lens._Just <&> (,,) prevKey prevDoc) ++
            (sMNextParamScope cursor ^.. Lens._Just <&> (,,) nextKey nextDoc)
        [setter scope & Widget.keysEventMap key doc]
    & mconcat
    where
        prevDoc = E.Doc ["Evaluation", "Scope", "Previous"]
        nextDoc = E.Doc ["Evaluation", "Scope", "Next"]

blockEventMap :: MonadA m => Widget.EventHandlers (T m)
blockEventMap =
    return mempty
    & E.keyPresses dirKeys
    (E.Doc ["Navigation", "Move", "(blocked)"])
    where
        dirKeys = [GLFW.Key'Left, GLFW.Key'Right] <&> ModKey mempty

makeScopeNavEdit ::
    MonadA m =>
    Sugar.Binder name m expr -> Widget.Id -> ScopeCursor ->
    ExprGuiM m (Widget.EventHandlers (T m), Maybe (ExpressionGui m))
makeScopeNavEdit binder myId curCursor =
    do
        config <- ExprGuiM.readConfig
        let mkArrow (txt, mScopeId) =
                ExpressionGui.makeLabel txt (Widget.toAnimId myId)
                & ExprGuiM.localEnv
                ( WE.setTextSizeColor
                    (Config.navArrowsSize config)
                    ( case mScopeId of
                        Nothing -> Config.disabledColor config
                        Just _ -> Config.grammarColor config
                    )
                )
        let Config.Eval{..} = Config.eval config
        settings <- ExprGuiM.readSettings
        case settings ^. CESettings.sInfoMode of
            CESettings.Evaluation ->
                mapM mkArrow scopes
                >>= ExpressionGui.hboxSpaced
                >>= ExpressionGui.makeFocusableView myId
                <&>
                ExpressionGui.egWidget %~ Widget.weakerEvents
                ( mkScopeEventMap leftKeys rightKeys
                  & maybe mempty (`mappend` blockEventMap)
                )
                <&> Just
                <&> (,)
                    (mkScopeEventMap prevScopeKeys nextScopeKeys
                     & fromMaybe mempty)
            _ -> return (mempty, Nothing)
    where
        mkScopeEventMap l r = makeScopeEventMap l r curCursor <$> mSetScope
        leftKeys = [ModKey mempty GLFW.Key'Left]
        rightKeys = [ModKey mempty GLFW.Key'Right]
        scopes :: [(String, Maybe Sugar.BinderParamScopeId)]
        scopes =
            [ ("◀", sMPrevParamScope curCursor)
            , ("▶", sMNextParamScope curCursor)
            ]
        mSetScope =
            binder ^. Sugar.bMChosenScopeProp
            <&> Transaction.setP
            <&> (. Just)

makeMParamsEdit ::
    MonadA m =>
    CurAndPrev (Maybe ScopeCursor) -> Maybe (ExpressionGui n) ->
    Widget.Id -> Widget.Id ->
    NearestHoles -> Widget.Id -> Sugar.BinderParams (Name m) m ->
    ExprGuiM m (Maybe (ExpressionGui m))
makeMParamsEdit mScopeCursor mScopeNavEdit delVarBackwardsId myId nearestHoles bodyId params =
    params
    & makeParamsEdit annotationMode nearestHoles
      delVarBackwardsId myId bodyId
    & ExprGuiM.withLocalMScopeId
      ( mScopeCursor
      <&> Lens.traversed %~ (^. Sugar.bParamScopeId) . sBinderScope
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
        mCurCursor = mScopeCursor ^. current
        annotationMode =
            do
                mScopeNavEdit ^?
                    Lens._Just . ExpressionGui.egWidget . Widget.isFocused
                    >>= guard
                ExpressionGui.NeighborVals
                    <$> (mCurCursor <&> sMPrevParamScope)
                    <*> (mCurCursor <&> sMNextParamScope)
                    <&> ExpressionGui.WithNeighbouringEvalAnnotations
            & fromMaybe ExpressionGui.NormalEvalAnnotation

binderContentNearestHoles :: Sugar.BinderContent name m (ExprGuiT.SugarExpr m) -> NearestHoles
binderContentNearestHoles body =
    body ^? Lens.traverse
    & fromMaybe (error "We have at least a body expression inside the binder")
    & ExprGuiT.nextHolesBefore

makeParts ::
    MonadA m =>
    ExprGuiT.FuncApplyLimit ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Widget.Id -> Widget.Id ->
    ExprGuiM m (Parts m)
makeParts funcApplyLimit binder delVarBackwardsId myId =
    do
        mScopeCursor <- mkChosenScopeCursor binder
        let binderScopeId = mScopeCursor <&> Lens.mapped %~ (^. Sugar.bParamScopeId) . sBinderScope
        (scopeEventMap, mScopeNavEdit) <-
            do
                guard (funcApplyLimit == ExprGuiT.UnlimitedFuncApply)
                currentScope <- mScopeCursor ^. current
                guard $
                    Lens.nullOf (Sugar.bParams . Sugar._NullParam) binder ||
                    Lens.has (Lens.traversed . Lens._Just) [sMPrevParamScope currentScope, sMNextParamScope currentScope]
                Just currentScope
            & maybe (return (mempty, Nothing)) (makeScopeNavEdit binder scopesNavId)
        do
            mParamsEdit <-
                makeMParamsEdit mScopeCursor mScopeNavEdit delVarBackwardsId myId
                (binderContentNearestHoles bodyContent) bodyId params
            rhs <- makeBinderBodyEdit params body
            Parts mParamsEdit mScopeNavEdit rhs scopeEventMap & return
            & case mScopeNavEdit of
              Nothing -> ExprGuiM.assignCursorPrefix scopesNavId (const destId)
              Just _ -> id
            & ExprGuiM.withLocalMScopeId binderScopeId
    where
        destId =
            params ^? SugarLens.binderNamedParams . Sugar.fpId
            <&> WidgetIds.fromEntityId
            & fromMaybe bodyId
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody
        bodyContent = body ^. Sugar.bbContent
        bodyId = bodyContent ^. SugarLens.binderContentEntityId & WidgetIds.fromEntityId
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
            makeParts ExprGuiT.UnlimitedFuncApply binder myId myId
        rhsJumperEquals <- jumpToRHS [ModKey mempty GLFW.Key'Equal] rhs
        presentationEdits <-
            binder ^.. Sugar.bMPresentationModeProp . Lens._Just
            & traverse (mkPresentationModeEdit presentationChoiceId)
        jumpHolesEventMap <-
            ExprEventMap.jumpHolesEventMap (binderContentNearestHoles body)
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
        presentationChoiceId = Widget.joinId myId ["presentation"]
        rhs = ("Def Body", body ^. SugarLens.binderContentEntityId)
        body = binder ^. Sugar.bBody . Sugar.bbContent

makeLetEdit ::
    MonadA m =>
    Sugar.Let (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeLetEdit item =
    do
        config <- ExprGuiM.readConfig
        let eventMap
                | Just lActions <- item ^. Sugar.lActions =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.delKeys config)
                  (E.Doc ["Edit", "Let clause", "Delete"]) $
                  bodyId <$ lActions ^. Sugar.laSetToInner
                , Widget.keysEventMapMovesCursor (Config.extractKeys config)
                  (E.Doc ["Edit", "Let clause", "Extract to outer scope"]) $
                  WidgetIds.fromEntityId <$>
                  lActions ^. Sugar.laExtract
                ]
                | otherwise = mempty
        edit <-
            make (item ^. Sugar.lName) binder myId
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
            <&> ExpressionGui.pad
                (Config.letItemPadding config <&> realToFrac)
            <&> ExpressionGui.egAlignment . _1 .~ 0
        letLabel <- ExpressionGui.grammarLabel "let" (Widget.toAnimId myId)
        ExpressionGui.hboxSpaced [letLabel, edit]
    where
        bodyId =
            item ^. Sugar.lBody . Sugar.bbContent . SugarLens.binderContentEntityId
            & WidgetIds.fromEntityId
        myId = item ^. Sugar.lEntityId & WidgetIds.fromEntityId
        binder = item ^. Sugar.lValue

jumpToRHS ::
    MonadA f => [ModKey] -> (String, Sugar.EntityId) ->
    ExprGuiM f (Widget.EventHandlers (T f))
jumpToRHS keys (rhsDoc, rhsId) = do
    savePos <- ExprGuiM.mkPrejumpPosSaver
    return $
        Widget.keysEventMapMovesCursor keys (E.Doc ["Navigation", "Jump to " ++ rhsDoc]) $
            WidgetIds.fromEntityId rhsId <$ savePos

makeBinderBodyEdit ::
    MonadA m =>
    Sugar.BinderParams name m ->
    Sugar.BinderBody (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderBodyEdit params (Sugar.BinderBody mActions content) =
    do
        config <- ExprGuiM.readConfig
        savePos <- ExprGuiM.mkPrejumpPosSaver
        let newLetEventMap =
                case mActions of
                Nothing -> mempty
                Just Sugar.BinderBodyActions { _bbaAddOuterLet } ->
                    do
                        savePos
                        _bbaAddOuterLet
                        <&> WidgetIds.fromEntityId <&> WidgetIds.nameEditOf
                    & Widget.keysEventMapMovesCursor (Config.letAddItemKeys config)
                      (E.Doc ["Edit", "Let clause", "Add"])
        makeBinderContentEdit params content
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents newLetEventMap

makeBinderContentEdit ::
    MonadA m =>
    Sugar.BinderParams name m ->
    Sugar.BinderContent (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderContentEdit params (Sugar.BinderLet l) =
    do
        config <- ExprGuiM.readConfig
        let delEventMap =
                maybe mempty
                (Widget.keysEventMapMovesCursor (Config.delKeys config)
                 (E.Doc ["Edit", "Delete let expression"])
                 . fmap WidgetIds.fromEntityId . (^. Sugar.laSetToHole))
                (l ^. Sugar.lActions)
        mOuterScopeId <- ExprGuiM.readMScopeId
        let letBodyScope = liftA2 lookupMKey mOuterScopeId (l ^. Sugar.lBodyScope)
        [ makeLetEdit l
            , makeBinderBodyEdit params body
              & ExprGuiM.withLocalMScopeId letBodyScope
            ] & sequence
            <&> map (ExpressionGui.egAlignment . _1 .~ 0)
            >>= ExpressionGui.vboxTopFocalSpaced
            >>= ExpressionGui.parentDelegator letEntityId
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents delEventMap
    where
        letEntityId = l ^. Sugar.lEntityId & WidgetIds.fromEntityId
        body = l ^. Sugar.lBody
makeBinderContentEdit params (Sugar.BinderExpr binderBody) =
    do
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
        ExprGuiM.makeSubexpression (const 0) binderBody
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents jumpToLhsEventMap

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
