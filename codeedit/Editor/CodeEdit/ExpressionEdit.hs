{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Monad ((<=<), liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Control.Lens as Lens
import qualified Editor.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.ExpressionEdit.AtomEdit as AtomEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.ExpressionEdit.InferredEdit as InferredEdit
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.PiEdit as PiEdit
import qualified Editor.CodeEdit.ExpressionEdit.PolymorphicEdit as PolymorphicEdit
import qualified Editor.CodeEdit.ExpressionEdit.SectionEdit as SectionEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.Settings as Settings
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetEnvT as WE
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data IsHole = NotAHole | IsAHole

pasteEventMap
  :: MonadF m
  => Sugar.Hole m -> Widget.EventHandlers (Transaction ViewTag m)
pasteEventMap =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   Config.pasteKeys "Paste" .
   liftM WidgetIds.fromGuid) .
  (Sugar.holePaste <=< Sugar.holeMActions)

make :: MonadF m => Sugar.Expression m -> ExprGuiM m (ExpressionGui m)
make sExpr = do
  (holePicker, widget) <- makeEditor sExpr exprId
  typeEdits <- mapM make $ Sugar.plInferredTypes payload
  let onReadOnly = Widget.doesntTakeFocus
  exprEventMap <- expressionEventMap exprGuid holePicker $ Sugar.rPayload sExpr
  settings <- ExprGuiM.readSettings
  let
    addInferredTypes =
      case Lens.view Settings.sInfoMode settings of
      Settings.InfoNone -> id
      Settings.InfoTypes ->
        ExpressionGui.addType ExpressionGui.Background exprId
        (map
         ( Widget.tint Config.inferredTypeTint
         . Widget.scale Config.typeScaleFactor
         . Lens.view ExpressionGui.egWidget
         ) typeEdits)
      Settings.InfoExamples -> -- TODO:
        id
  return .
    Lens.over ExpressionGui.egWidget
    ( maybe onReadOnly (const id) (Sugar.plActions payload)
    . Widget.weakerEvents exprEventMap
    ) .
    addInferredTypes $
    widget
  where
    payload = Sugar.rPayload sExpr
    exprId = WidgetIds.fromGuid exprGuid
    exprGuid = Sugar.rGuid sExpr

makeEditor
  :: MonadF m
  => Sugar.Expression m
  -> Widget.Id
  -> ExprGuiM m (IsHole, ExpressionGui m)
makeEditor sExpr =
  case Sugar.rExpressionBody sExpr of
  Sugar.ExpressionFunc hasParens f ->
    notAHole $ FuncEdit.make hasParens f
  Sugar.ExpressionInferred i ->
    isAHole (Sugar.iHole i) . InferredEdit.make i $ Sugar.rGuid sExpr
  Sugar.ExpressionPolymorphic poly ->
    notAHole $ PolymorphicEdit.make poly
  Sugar.ExpressionHole hole ->
    isAHole hole . HoleEdit.make hole mNextHole $ Sugar.rGuid sExpr
  Sugar.ExpressionGetVariable varRef ->
    notAHole $ VarEdit.make varRef
  Sugar.ExpressionApply hasParens apply ->
    notAHole $ ApplyEdit.make hasParens apply
  Sugar.ExpressionPi hasParens funcType ->
    notAHole $ PiEdit.make hasParens funcType
  Sugar.ExpressionSection hasParens section ->
    notAHole $ SectionEdit.make hasParens section
  Sugar.ExpressionLiteralInteger integer ->
    notAHole $ LiteralEdit.makeInt integer
  Sugar.ExpressionAtom atom ->
    notAHole $ AtomEdit.make atom
  where
    isAHole hole =
      (fmap . liftM)
      ((,) IsAHole .
       (Lens.over ExpressionGui.egWidget . Widget.weakerEvents) (pasteEventMap hole))
    notAHole = (fmap . liftM) ((,) NotAHole)
    mNextHole = Sugar.plNextHole $ Sugar.rPayload sExpr

expressionEventMap ::
  MonadF m =>
  Guid -> IsHole ->
  Sugar.Payload m ->
  ExprGuiM m (EventHandlers (Transaction ViewTag m))
expressionEventMap exprGuid holePicker payload =
  maybe (return mempty) (actionsEventMap exprGuid holePicker) $
  Sugar.plActions payload

actionsEventMap ::
  MonadF m =>
  Guid -> IsHole -> Sugar.Actions m ->
  ExprGuiM m (EventHandlers (Transaction ViewTag m))
actionsEventMap exprGuid holePicker actions = do
  isSelected <-
    ExprGuiM.widgetEnv . WE.isSubCursor $
    WidgetIds.fromGuid exprGuid
  let
    replace
      | isSelected =
        if isHole then mempty else
        mkEventMap delKeys "Replace" FocusDelegator.delegatingId $
        Sugar.replace actions
      | otherwise =
        mkEventMap delKeys "Select parent" FocusDelegator.notDelegatingId $ return exprGuid
  return $ mconcat
    [ giveAsArg
    , addOperator
    , replace
    , cut
    ]
  where
    delKeys = concat [Config.replaceKeys, Config.delForwardKeys, Config.delBackwordKeys]
    giveAsArg =
      moveUnlessOnHole .
      Widget.keysEventMapMovesCursor
      Config.giveAsArgumentKeys "Give as argument" . liftM WidgetIds.fromGuid $
      Sugar.giveAsArg actions
    addOperator =
      (fmap . fmap) Widget.eventResultFromCursor .
      E.charGroup "Operator" "Apply operator" Config.operatorChars .
      fmap const $
      liftM (HoleEdit.searchTermWidgetId . WidgetIds.fromGuid) .
      Sugar.giveAsArgToOperator actions . (:[])
    cut =
      if isHole then mempty else
      mkEventMap Config.cutKeys "Cut" id $
      Sugar.cut actions
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      liftM (f . WidgetIds.fromGuid)

    moveUnlessOnHole = ifHole $ (fmap . liftM . Lens.set Widget.eCursor) Nothing
    isHole =
      case holePicker of
      NotAHole -> False
      IsAHole -> True
    ifHole whenHole =
      case holePicker of
      NotAHole -> id
      IsAHole -> whenHole
