{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.AtomEdit as AtomEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.InferredEdit as InferredEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ListEdit as ListEdit
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

data IsHole = NotAHole | IsAHole

pasteEventMap ::
  MonadA m => Sugar.Hole m -> Widget.EventHandlers (Transaction m)
pasteEventMap =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   Config.pasteKeys (E.Doc ["Edit", "Paste"]) .
   fmap WidgetIds.fromGuid) .
  (Lens.view Sugar.holePaste <=< Lens.view Sugar.holeMActions)

make :: MonadA m => Sugar.Expression m -> ExprGuiM m (ExpressionGui m)
make sExpr = assignCursor $ do
  (isHole, widget) <- makeEditor sExpr exprId
  typeEdits <- traverse make $ payload ^. Sugar.plInferredTypes
  let onReadOnly = Widget.doesntTakeFocus
  exprEventMap <- expressionEventMap exprGuid isHole $ sExpr ^. Sugar.rPayload
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
    assignCursor f =
      foldr (`ExprGuiM.assignCursor` exprId) f
      (WidgetIds.fromGuid <$> sExpr ^. Sugar.rHiddenGuids)

makeEditor ::
  MonadA m => Sugar.Expression m -> Widget.Id ->
  ExprGuiM m (IsHole, ExpressionGui m)
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
  Sugar.ExpressionList list ->
    notAHole $ ListEdit.make list
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
expressionEventMap exprGuid isHole payload =
  maybe (return mempty) (actionsEventMap exprGuid isHole) $
  payload ^. Sugar.plActions

actionsEventMap ::
  MonadA m =>
  Guid -> IsHole -> Sugar.Actions m ->
  ExprGuiM m (EventHandlers (Transaction m))
actionsEventMap exprGuid isHole actions = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  callWithArgsEventMap <-
    if isSelected
    then
      mconcat <$> sequence
      [ maybe mempty
        (mkEventMap Config.callWithArgumentKeys (E.Doc ["Edit", "Call with argument"])
         FocusDelegator.delegatingId) <$>
        ExprGuiM.transaction (actions ^. Sugar.callWithArg)
      , maybe mempty
        (mkEventMap Config.callWithNextArgumentKeys (E.Doc ["Edit", "Add argument"])
         FocusDelegator.delegatingId) <$>
        ExprGuiM.transaction (actions ^. Sugar.callWithNextArg)
      ]
    else return mempty
  let
    replace
      | isSelected && isHoleBool = mempty
      | isSelected =
        mkEventMap delKeys (E.Doc ["Edit", "Replace expression"])
        FocusDelegator.delegatingId $ actions ^. Sugar.replace
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        FocusDelegator.notDelegatingId $ return exprGuid
  return $ mconcat
    [ callWithArgsEventMap
    , giveAsArg
    , addOperator
    , replace
    , cut
    ]
  where
    delKeys = concat [Config.replaceKeys, Config.delForwardKeys, Config.delBackwordKeys]
    giveAsArg =
      Widget.keysEventMapMovesCursor
      Config.giveAsArgumentKeys (E.Doc ["Edit", "Give as argument"]) . fmap WidgetIds.fromGuid $
      actions ^. Sugar.giveAsArg
    addOperator =
      (fmap . fmap) Widget.eventResultFromCursor .
      E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"])
      Config.operatorChars $ \c _isShifted -> do
        targetGuid <- actions ^. Sugar.giveAsArgToOperator
        HoleEdit.holeCreated targetGuid [c]
    cut
      | isHoleBool = mempty
      | otherwise =
        mkEventMap Config.cutKeys (E.Doc ["Edit", "Cut"]) id $
        actions ^. Sugar.cut
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      fmap (f . WidgetIds.fromGuid)
    isHoleBool =
      case isHole of
      NotAHole -> False
      IsAHole -> True
