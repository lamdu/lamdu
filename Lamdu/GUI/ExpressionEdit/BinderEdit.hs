{-# LANGUAGE RecordWildCards, OverloadedStrings, PatternGuards #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make, diveToNameEdit
    , Parts(..), makeParts
    ) where

import           Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join)
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
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), NameSource(..))
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
                xs <&> ExpressionGui.egAlignment . _1 .~ 0.5
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
    }

data ScopeCursor = ScopeCursor
    { sParamScope :: ScopeId
    , sBodyScope :: ScopeId
    , sMPrevParamScope :: Maybe ScopeId
    , sMNextParamScope :: Maybe ScopeId
    }

getScopeCursor :: Maybe ScopeId -> [(ScopeId, ScopeId)] -> Maybe ScopeCursor
getScopeCursor mChosenScope scopes =
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

makeScopeEventMap ::
    MonadA m =>
    Config.Eval -> (Maybe ScopeId -> T m ()) -> ScopeCursor ->
    Widget.EventHandlers (T m)
makeScopeEventMap Config.Eval{..} setter cursor =
    do
        (key, doc, scope) <-
            (sMPrevParamScope cursor ^.. Lens._Just <&> (,,) prevKey prevDoc) ++
            (sMNextParamScope cursor ^.. Lens._Just <&> (,,) nextKey nextDoc)
        [Just scope & setter & Widget.keysEventMap key doc]
    & mconcat
    where
        prevKey = prevScopeKeys
        prevDoc = E.Doc ["Evaluation", "Scope", "Previous"]
        nextKey = nextScopeKeys
        nextDoc = E.Doc ["Evaluation", "Scope", "Next"]

makeParts ::
    MonadA m =>
    ExprGuiT.ShowAnnotation ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) -> Widget.Id ->
    ExprGuiM m (Parts m)
makeParts showAnnotation binder myId =
    do
        config <- ExprGuiM.readConfig
        mOuterScopeId <- ExprGuiM.readMScopeId
        mChosenScope <-
            binder ^. Sugar.bMChosenScopeProp
            & Lens._Just %%~ Transaction.getP
            & ExprGuiM.transaction
            <&> join
        let mScopeCursor =
                mOuterScopeId
                >>= (`Map.lookup` (binder ^. Sugar.bScopes))
                >>= getScopeCursor mChosenScope
        let scopeEventMap =
                makeScopeEventMap (Config.eval config)
                <$> (binder ^. Sugar.bMChosenScopeProp <&> Transaction.setP)
                <*> mScopeCursor
                & fromMaybe mempty
        paramEdits <-
            makeParamsEdit showAnnotation
            (ExprGuiT.nextHolesBefore body) myId params
            & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sParamScope)
            <&> Lens.mapped . ExpressionGui.egWidget
                %~ Widget.weakerEvents scopeEventMap
        bodyEdit <-
            makeResultEdit (binder ^. Sugar.bMActions) params body
            & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sBodyScope)
        wheresEdit <-
            makeWheres (binder ^. Sugar.bWhereItems) myId
            & ExprGuiM.withLocalMScopeId (mScopeCursor <&> sParamScope)
        return $ Parts paramEdits bodyEdit wheresEdit
    where
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody

make ::
    MonadA m =>
    Name m ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
make name binder myId =
    do
        Parts paramEdits bodyEdit mWheresEdit <-
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
                        nextId <$ wiActions ^. Sugar.itemDelete
                    , Widget.keysEventMapMovesCursor
                        (Config.whereAddItemKeys config)
                        (E.Doc ["Edit", "Where clause", "Add next"]) $
                        WidgetIds.fromEntityId <$>
                        wiActions ^. Sugar.itemAddNext
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
            Sugar.NoParams -> mempty
            Sugar.VarParam param ->
                Widget.keysEventMapMovesCursor
                (Config.jumpRHStoLHSKeys config) (E.Doc ["Navigation", "Jump to param"]) $
                WidgetIds.fromEntityId (param ^. Sugar.fpId) <$ savePos
            Sugar.FieldParams ps ->
                Widget.keysEventMapMovesCursor
                (Config.jumpRHStoLHSKeys config) (E.Doc ["Navigation", "Jump to last param"]) $
                WidgetIds.fromEntityId (last ps ^. Sugar.fpId) <$ savePos
        addWhereItemEventMap actions =
            Widget.keysEventMapMovesCursor (Config.whereAddItemKeys config)
            (E.Doc ["Edit", "Where clause", "Add first"]) .
            fmap (diveToNameEdit . WidgetIds.fromEntityId) $
            savePos >> actions ^. Sugar.baAddInnermostWhereItem
    ExprGuiM.makeSubexpression 0 result
        <&> ExpressionGui.egWidget %~
                Widget.weakerEvents
                (jumpToLhsEventMap <> maybe mempty addWhereItemEventMap mActions)

makeParamsEdit ::
    MonadA m =>
    ExprGuiT.ShowAnnotation ->
    NearestHoles ->
    Widget.Id ->
    Sugar.BinderParams (Name m) m ->
    ExprGuiM m [ExpressionGui m]
makeParamsEdit showAnnotation nearestHoles lhsId params =
    do
        jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
        let mkParam (prevId, nextId, param) =
                ParamEdit.make showAnnotation prevId nextId param
                <&> ExpressionGui.egWidget
                %~ Widget.weakerEvents jumpHolesEventMap
        ExpressionGui.listWithDelDests lhsId lhsId
            (WidgetIds.fromEntityId . (^. Sugar.fpId)) paramList
            & traverse mkParam
    where
        paramList =
            case params of
            Sugar.NoParams -> []
            Sugar.VarParam p -> [p]
            Sugar.FieldParams ps -> ps & Lens.traversed . Sugar.fpVarInfo .~ ()

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = ExpressionGui.diveToNameEdit -- Name editor
