{-# LANGUAGE NamedFieldPuns, RecordWildCards, NoImplicitPrelude, OverloadedStrings, LambdaCase #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make
    , Parts(..), makeParts
    , nonOperatorName
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev, current)
import           Data.List.Utils (nonEmptyAll)
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.CharClassification (operatorChars)
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

import           Lamdu.Prelude

type T = Transaction

nonOperatorName :: Name m -> Bool
nonOperatorName (Name NameSourceStored _ _ x) =
    nonEmptyAll (`notElem` operatorChars) (Text.unpack x)
nonOperatorName _ = False

makeBinderNameEdit ::
    Monad m =>
    Sugar.BinderActions m ->
    Widget.EventMap (T m Widget.EventResult) ->
    (Text, Sugar.EntityId) ->
    Name m -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeBinderNameEdit binderActions rhsJumperEquals rhs name myId =
    do
        config <- ExprGuiM.readConfig
        rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
        ExpressionGui.makeNameOriginEdit name myId
            <&> jumpToRHSViaEquals name
            <&> Widget.weakerEvents
                (ParamEdit.eventMapAddFirstParam config
                 (binderActions ^. Sugar.baAddFirstParam) <>
                 rhsJumper)
            <&> TreeLayout.fromCenteredWidget
    where
        jumpToRHSViaEquals n widget
            | nonOperatorName n =
                widget
                & Widget.eventMap %~ E.filterChars (/= '=')
                & Widget.weakerEvents rhsJumperEquals
            | otherwise = widget

presentationModeChoiceConfig :: Choice.Config
presentationModeChoiceConfig = Choice.Config
    { Choice.cwcFDConfig =
        FocusDelegator.Config
        { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'Enter]
        , FocusDelegator.focusChildDoc = E.Doc ["Presentation Mode", "Select"]
        , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Enter]
        , FocusDelegator.focusParentDoc = E.Doc ["Presentation Mode", "Choose selected"]
        }
    , Choice.cwcOrientation = Box.Vertical
    , Choice.cwcExpandMode = Choice.ExplicitEntry
    }

mkPresentationModeEdit ::
    Monad m => Widget.Id ->
    Transaction.MkProperty m Sugar.PresentationMode ->
    ExprGuiM m (Widget (T m Widget.EventResult))
mkPresentationModeEdit myId prop = do
    cur <- ExprGuiM.transaction $ Transaction.getP prop
    config <- ExprGuiM.readConfig
    let mkPair presentationMode = do
            widget <-
                ExprGuiM.withFgColor (Config.presentationChoiceColor config) .
                ExprGuiM.widgetEnv $
                BWidgets.makeFocusableLabel (Text.pack (show presentationMode)) myId
            return (presentationMode, widget)
    pairs <- traverse mkPair [Sugar.OO, Sugar.Verbose, Sugar.Infix 5]
    BWidgets.makeChoiceWidget (Transaction.setP prop) pairs cur
        presentationModeChoiceConfig myId
        & ExprGuiM.widgetEnv
        <&> Widget.scale (realToFrac <$> Config.presentationChoiceScaleFactor config)

data Parts m = Parts
    { pMParamsEdit :: Maybe (ExpressionGui m)
    , pMScopesEdit :: Maybe (AlignedWidget (T m Widget.EventResult))
    , pBodyEdit :: ExpressionGui m
    , pEventMap :: Widget.EventMap (T m Widget.EventResult)
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
    Monad m =>
    Sugar.Binder name m expr -> ExprGuiM m (Maybe Sugar.BinderParamScopeId)
readBinderChosenScope binder =
    binder ^. Sugar.bChosenScopeProp & Transaction.getP & ExprGuiM.transaction

mkChosenScopeCursor ::
    Monad m =>
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
    Monad m =>
    [ModKey] -> [ModKey] -> ScopeCursor -> (Sugar.BinderParamScopeId -> m ()) ->
    Widget.EventMap (m Widget.EventResult)
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

blockEventMap :: Monad m => Widget.EventMap (m Widget.EventResult)
blockEventMap =
    return mempty
    & E.keyPresses dirKeys
    (E.Doc ["Navigation", "Move", "(blocked)"])
    where
        dirKeys = [GLFW.Key'Left, GLFW.Key'Right] <&> ModKey mempty

makeScopeNavEdit ::
    Monad m =>
    Sugar.Binder name m expr -> Widget.Id -> ScopeCursor ->
    ExprGuiM m
    ( Widget.EventMap (T m Widget.EventResult)
    , Maybe (AlignedWidget (T m Widget.EventResult))
    )
makeScopeNavEdit binder myId curCursor =
    do
        config <- ExprGuiM.readConfig
        let mkArrow (txt, mScopeId) =
                ExpressionGui.makeLabel txt (Widget.toAnimId myId)
                & ExprGuiM.localEnv
                ( case mScopeId of
                  Nothing -> Config.disabledColor config
                  Just _ -> Config.grammarColor config
                  & WE.setTextColor
                )
        let Config.Eval{..} = Config.eval config
        settings <- ExprGuiM.readSettings
        case settings ^. CESettings.sInfoMode of
            CESettings.Evaluation ->
                ExprGuiM.widgetEnv (BWidgets.makeFocusableView myId <&> (AlignedWidget.widget %~))
                <*> (mapM mkArrow scopes <&> AlignedWidget.hbox 0.5)
                <&> AlignedWidget.widget %~ Widget.weakerEvents
                    (mkScopeEventMap leftKeys rightKeys `mappend` blockEventMap)
                <&> Just
                <&> (,) (mkScopeEventMap prevScopeKeys nextScopeKeys)
            _ -> return (mempty, Nothing)
    where
        mkScopeEventMap l r = makeScopeEventMap l r curCursor setScope
        leftKeys = [ModKey mempty GLFW.Key'Left]
        rightKeys = [ModKey mempty GLFW.Key'Right]
        scopes :: [(Text, Maybe Sugar.BinderParamScopeId)]
        scopes =
            [ ("◀", sMPrevParamScope curCursor)
            , (" ", Nothing)
            , ("▶", sMNextParamScope curCursor)
            ]
        setScope = Transaction.setP (binder ^. Sugar.bChosenScopeProp) . Just

data IsScopeNavFocused = ScopeNavIsFocused | ScopeNavNotFocused
    deriving (Eq, Ord)

makeMParamsEdit ::
    Monad m =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    NearestHoles -> Widget.Id -> Sugar.BinderParams (Name m) m ->
    ExprGuiM m (Maybe (ExpressionGui m))
makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId nearestHoles bodyId params =
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
        ExpressionGui.combineSpaced Nothing
        ?? (paramEdits <&> TreeLayout.alignment . _1 .~ 0.5)
        & case params of
          Sugar.FieldParams{} -> (ExpressionGui.addValFrame myId <*>)
          _ -> id
        <&> Just
    where
        mCurCursor = mScopeCursor ^. current
        annotationMode =
            do
                ScopeNavIsFocused == isScopeNavFocused & guard
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
    Monad m =>
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
        let isScopeNavFocused =
                case mScopeNavEdit of
                Just layout
                    | Widget.isFocused (layout ^. AlignedWidget.widget) -> ScopeNavIsFocused
                _ -> ScopeNavNotFocused
        do
            mParamsEdit <-
                makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId
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
    Monad m =>
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
            makeBinderNameEdit (binder ^. Sugar.bActions) rhsJumperEquals rhs
            name myId
            <&> TreeLayout.alignment . _1 .~ 0
            <&> TreeLayout.alignedWidget %~
                AlignedWidget.addAfter AlignedWidget.Vertical
                (presentationEdits
                <&> AlignedWidget.fromCenteredWidget
                <&> AlignedWidget.alignment . _1 .~ 0)
            <&> TreeLayout.widget %~ Widget.weakerEvents jumpHolesEventMap
        mLhsEdit <-
            case mParamsEdit of
            Nothing -> return Nothing
            Just paramsEdit ->
                ExpressionGui.vboxTopFocalSpaced
                ?? (paramsEdit : fmap TreeLayout.fromAlignedWidget mScopeEdit ^.. Lens._Just
                    <&> TreeLayout.alignment . _1 .~ 0.5)
                <&> TreeLayout.widget %~ Widget.weakerEvents rhsJumperEquals
                <&> Just
        equals <- ExpressionGui.makeLabel "=" (Widget.toAnimId myId)
        ExpressionGui.combineSpaced Nothing
            <&>
            (\hbox ->
            hbox
            [ hbox (defNameEdit : (mLhsEdit ^.. Lens._Just) ++ [TreeLayout.fromAlignedWidget equals])
            , bodyEdit
            ] )
            <&> TreeLayout.widget %~ Widget.weakerEvents eventMap
    where
        presentationChoiceId = Widget.joinId myId ["presentation"]
        rhs = ("Def Body", body ^. SugarLens.binderContentEntityId)
        body = binder ^. Sugar.bBody . Sugar.bbContent

makeLetEdit ::
    Monad m =>
    Sugar.Let (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeLetEdit item =
    do
        config <- ExprGuiM.readConfig
        let actionsEventMap =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.delKeys config)
                  (E.Doc ["Edit", "Let clause", "Delete"]) $
                  bodyId <$ item ^. Sugar.lActions . Sugar.laSetToInner
                , Widget.keysEventMapMovesCursor (Config.extractKeys config)
                  (E.Doc ["Edit", "Let clause", "Extract to outer scope"]) $
                  WidgetIds.fromEntityId . Sugar.lfrNewEntity <$>
                  item ^. Sugar.lActions . Sugar.laFloat
                ]
        let usageEventMap =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.inlineKeys config)
                  (E.Doc ["Navigation", "Jump to first use"])
                  (return (WidgetIds.fromEntityId usage))
                | usage <- take 1 (item ^. Sugar.lUsages)
                ]
        let eventMap = mappend actionsEventMap usageEventMap
        ExpressionGui.tagItem
            <*> ExpressionGui.grammarLabel "let" (Widget.toAnimId myId)
            <*> (make (item ^. Sugar.lName) binder myId
                <&> TreeLayout.widget %~ Widget.weakerEvents eventMap
                <&> TreeLayout.pad
                    (Config.letItemPadding config <&> realToFrac)
                )
    where
        bodyId =
            item ^. Sugar.lBody . Sugar.bbContent . SugarLens.binderContentEntityId
            & WidgetIds.fromEntityId
        myId = item ^. Sugar.lEntityId & WidgetIds.fromEntityId
        binder = item ^. Sugar.lValue

jumpToRHS ::
    Monad f =>
    [ModKey] -> (Text, Sugar.EntityId) ->
    ExprGuiM f (Widget.EventMap (T f Widget.EventResult))
jumpToRHS keys (rhsDoc, rhsId) = do
    savePos <- ExprGuiM.mkPrejumpPosSaver
    return $
        Widget.keysEventMapMovesCursor keys (E.Doc ["Navigation", "Jump to " <> rhsDoc]) $
            WidgetIds.fromEntityId rhsId <$ savePos

makeBinderBodyEdit ::
    Monad m =>
    Sugar.BinderParams name m ->
    Sugar.BinderBody (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderBodyEdit params (Sugar.BinderBody addOuterLet content) =
    do
        config <- ExprGuiM.readConfig
        savePos <- ExprGuiM.mkPrejumpPosSaver
        let newLetEventMap =
                savePos >> addOuterLet
                <&> WidgetIds.fromEntityId <&> WidgetIds.nameEditOf
                & Widget.keysEventMapMovesCursor (Config.letAddItemKeys config)
                  (E.Doc ["Edit", "Let clause", "Add"])
        makeBinderContentEdit params content
            <&> TreeLayout.widget %~ Widget.weakerEvents newLetEventMap

makeBinderContentEdit ::
    Monad m =>
    Sugar.BinderParams name m ->
    Sugar.BinderContent (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderContentEdit params (Sugar.BinderLet l) =
    do
        config <- ExprGuiM.readConfig
        let delEventMap =
                l ^. Sugar.lActions . Sugar.laSetToHole
                <&> WidgetIds.fromEntityId
                & Widget.keysEventMapMovesCursor (Config.delKeys config)
                  (E.Doc ["Edit", "Delete let expression"])
        let moveToInnerEventMap =
                body
                ^? Sugar.bbContent . Sugar._BinderLet
                . Sugar.lActions . Sugar.laFloat
                & maybe mempty
                (Widget.keysEventMap (Config.moveLetInwardKeys config)
                (E.Doc ["Edit", "Let clause", "Move inwards"]) . void)
        mOuterScopeId <- ExprGuiM.readMScopeId
        let letBodyScope = liftA2 lookupMKey mOuterScopeId (l ^. Sugar.lBodyScope)
        ExpressionGui.parentDelegator letEntityId
            <*> ( ExpressionGui.vboxTopFocalSpaced
                  <*>
                  ( sequence
                    [ makeLetEdit l
                      <&> TreeLayout.widget %~ Widget.weakerEvents moveToInnerEventMap
                    , makeBinderBodyEdit params body
                      & ExprGuiM.withLocalMScopeId letBodyScope
                    ] <&> map (TreeLayout.alignment . _1 .~ 0)
                  )
                )
            <&> TreeLayout.widget %~ Widget.weakerEvents delEventMap
    where
        letEntityId = l ^. Sugar.lEntityId & WidgetIds.fromEntityId
        body = l ^. Sugar.lBody
makeBinderContentEdit params (Sugar.BinderExpr binderBody) =
    do
        savePos <- ExprGuiM.mkPrejumpPosSaver
        config <- ExprGuiM.readConfig
        let jumpToLhsEventMap =
                case params of
                Sugar.BinderWithoutParams -> mempty
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
            <&> TreeLayout.widget %~ Widget.weakerEvents jumpToLhsEventMap

namedParamEditInfo :: Monad m => Sugar.NamedParamInfo (Name m) m -> ParamEdit.Info m
namedParamEditInfo paramInfo =
    ParamEdit.Info
    { ParamEdit.iMakeNameEdit =
      ExpressionGui.makeNameOriginEdit (paramInfo ^. Sugar.npiName)
      <&> Lens.mapped %~ TreeLayout.fromCenteredWidget
    , ParamEdit.iMAddNext = paramInfo ^. Sugar.npiActions . Sugar.fpAddNext & Just
    , ParamEdit.iMOrderBefore = paramInfo ^. Sugar.npiActions . Sugar.fpMOrderBefore
    , ParamEdit.iMOrderAfter = paramInfo ^. Sugar.npiActions . Sugar.fpMOrderAfter
    , ParamEdit.iDel = paramInfo ^. Sugar.npiActions . Sugar.fpDelete
    }

nullParamEditInfo :: Monad m => Sugar.NullParamInfo m -> ParamEdit.Info m
nullParamEditInfo (Sugar.NullParamInfo mActions) =
    ParamEdit.Info
    { ParamEdit.iMakeNameEdit =
      \myId ->
      ExpressionGui.makeFocusableView myId
      <*> ExpressionGui.grammarLabel "◗" (Widget.toAnimId myId)
      <&> TreeLayout.fromAlignedWidget
    , ParamEdit.iMAddNext = Nothing
    , ParamEdit.iMOrderBefore = Nothing
    , ParamEdit.iMOrderAfter = Nothing
    , ParamEdit.iDel = Sugar.ParamDelResultDelVar <$ mActions ^. Sugar.npDeleteLambda
    }

makeParamsEdit ::
    Monad m =>
    ExpressionGui.EvalAnnotationOptions -> NearestHoles ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Name m) m ->
    ExprGuiM m [ExpressionGui m]
makeParamsEdit annotationOpts nearestHoles delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.BinderWithoutParams -> return []
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
                        <&> TreeLayout.widget
                        %~ Widget.weakerEvents jumpHolesEventMap
                ExpressionGui.listWithDelDests delDestFirst delDestLast
                    (WidgetIds.fromEntityId . (^. Sugar.fpId)) paramList
                    & traverse mkParam
