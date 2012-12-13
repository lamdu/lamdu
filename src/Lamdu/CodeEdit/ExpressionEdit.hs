{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit(make) where

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Anchors (ViewM)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Control.Lens as Lens
import qualified Lamdu.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.AtomEdit as AtomEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.InferredEdit as InferredEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.PiEdit as PiEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.PolymorphicEdit as PolymorphicEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.SectionEdit as SectionEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Lamdu.CodeEdit.Settings as Settings
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data IsHole = NotAHole | IsAHole

pasteEventMap
  :: MonadA m
  => Sugar.Hole m -> Widget.EventHandlers (Transaction m)
pasteEventMap =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   Config.pasteKeys (E.Doc ["Edit", "Paste"]) .
   fmap WidgetIds.fromGuid) .
  (Sugar.holePaste <=< Sugar.holeMActions)

make :: m ~ ViewM => Sugar.Expression m -> ExprGuiM m (ExpressionGui m)
make sExpr = do
  (holePicker, widget) <- makeEditor sExpr exprId
  typeEdits <- traverse make $ payload ^. Sugar.plInferredTypes
  let onReadOnly = Widget.doesntTakeFocus
  exprEventMap <- expressionEventMap exprGuid holePicker $ sExpr ^. Sugar.rPayload
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
    ( maybe onReadOnly (const id) (payload ^. Sugar.plActions)
    . Widget.weakerEvents exprEventMap
    ) .
    addInferredTypes $
    widget
  where
    payload = sExpr ^. Sugar.rPayload
    exprId = WidgetIds.fromGuid exprGuid
    exprGuid = sExpr ^. Sugar.rGuid

makeEditor
  :: m ~ ViewM
  => Sugar.Expression m
  -> Widget.Id
  -> ExprGuiM m (IsHole, ExpressionGui m)
makeEditor sExpr =
  case sExpr ^. Sugar.rExpressionBody of
  Sugar.ExpressionFunc hasParens f ->
    notAHole $ FuncEdit.make hasParens f
  Sugar.ExpressionInferred i ->
    isAHole (Sugar.iHole i) . InferredEdit.make i $ sExpr ^. Sugar.rGuid
  Sugar.ExpressionPolymorphic poly ->
    notAHole $ PolymorphicEdit.make poly
  Sugar.ExpressionHole hole ->
    isAHole hole . HoleEdit.make hole mNextHole $ sExpr ^. Sugar.rGuid
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
      (fmap . fmap)
      ((,) IsAHole .
       (Lens.over ExpressionGui.egWidget . Widget.weakerEvents) (pasteEventMap hole))
    notAHole = (fmap . fmap) ((,) NotAHole)
    mNextHole = sExpr ^. Sugar.rPayload . Sugar.plNextHole

expressionEventMap ::
  MonadA m =>
  Guid -> IsHole ->
  Sugar.Payload m ->
  ExprGuiM m (EventHandlers (Transaction m))
expressionEventMap exprGuid holePicker payload =
  maybe (return mempty) (actionsEventMap exprGuid holePicker) $
  payload ^. Sugar.plActions

actionsEventMap ::
  MonadA m =>
  Guid -> IsHole -> Sugar.Actions m ->
  ExprGuiM m (EventHandlers (Transaction m))
actionsEventMap exprGuid holePicker actions = do
  isSelected <-
    ExprGuiM.widgetEnv . WE.isSubCursor $
    WidgetIds.fromGuid exprGuid
  let
    replace
      | isSelected =
        if isHole then mempty else
        mkEventMap delKeys (E.Doc ["Edit", "Replace expression"]) FocusDelegator.delegatingId $
        Sugar.replace actions
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"]) FocusDelegator.notDelegatingId $ return exprGuid
  return $ mconcat
    [ giveAsArg
    , addOperator
    , replace
    , cut
    ]
  where
    delKeys = concat [Config.replaceKeys, Config.delForwardKeys, Config.delBackwordKeys]
    giveAsArg =
      Widget.keysEventMapMovesCursor
      Config.giveAsArgumentKeys (E.Doc ["Edit", "Give as argument"]) . fmap WidgetIds.fromGuid $
      Sugar.giveAsArg actions
    addOperator =
      (fmap . fmap) Widget.eventResultFromCursor .
      E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"]) Config.operatorChars .
      fmap const $
      fmap (HoleEdit.searchTermWidgetId . WidgetIds.fromGuid) .
      Sugar.giveAsArgToOperator actions . (:[])
    cut =
      if isHole then mempty else
      mkEventMap Config.cutKeys (E.Doc ["Edit", "Cut"]) id $
      Sugar.cut actions
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      fmap (f . WidgetIds.fromGuid)

    isHole =
      case holePicker of
      NotAHole -> False
      IsAHole -> True
