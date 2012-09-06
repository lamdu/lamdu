{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first, second)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Editor.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.ExpressionEdit.AtomEdit as AtomEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.ExpressionEdit.InferredEdit as InferredEdit
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.PiEdit as PiEdit
import qualified Editor.CodeEdit.ExpressionEdit.PolymorphicEdit as PolymorphicEdit
import qualified Editor.CodeEdit.ExpressionEdit.SectionEdit as SectionEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.ExpressionEdit.WhereEdit as WhereEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data HoleResultPicker m = NotAHole | IsAHole (Maybe (HoleEdit.ResultPicker m))

pasteEventMap
  :: MonadF m
  => Sugar.Hole m -> Widget.EventHandlers (ITransaction ViewTag m)
pasteEventMap =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   Config.pasteKeys "Paste" .
   liftM WidgetIds.fromGuid .
   IT.transaction) .
  Sugar.holePaste

make :: MonadF m => ExpressionGui.Maker m
make sExpr = do
  (holePicker, widget) <- makeEditor sExpr exprId
  typeEdits <- mapM make $ Sugar.rInferredTypes sExpr
  let onReadOnly = Widget.doesntTakeFocus
  return .
    ExpressionGui.atEgWidget
    ( maybe onReadOnly
      (Widget.weakerEvents . expressionEventMap holePicker)
      (Sugar.rActions sExpr)
    ) .
    ExpressionGui.addType ExpressionGui.Background exprId
    (map
      ( Widget.tint Config.inferredTypeTint
      . Widget.scale Config.typeScaleFactor
      . ExpressionGui.egWidget
      ) typeEdits) $
    widget
  where
    exprId = WidgetIds.fromGuid $ Sugar.rGuid sExpr

makeEditor
  :: MonadF m
  => Sugar.Expression m
  -> Widget.Id
  -> VarAccess m (HoleResultPicker m, ExpressionGui m)
makeEditor sExpr =
  case Sugar.rExpression sExpr of
  Sugar.ExpressionWhere hasParens w ->
    notAHole $ WhereEdit.makeWithBody make hasParens w
  Sugar.ExpressionFunc hasParens f ->
    notAHole $ FuncEdit.make make hasParens f
  Sugar.ExpressionInferred i ->
    isAHole (Sugar.iHole i) . InferredEdit.make make i $ Sugar.rGuid sExpr
  Sugar.ExpressionPolymorphic poly ->
    notAHole $ PolymorphicEdit.make make poly
  Sugar.ExpressionHole hole ->
    isAHole hole . HoleEdit.make make hole $ Sugar.rGuid sExpr
  Sugar.ExpressionGetVariable varRef ->
    notAHole $ VarEdit.make varRef
  Sugar.ExpressionApply hasParens apply ->
    notAHole $ ApplyEdit.make make hasParens apply
  Sugar.ExpressionPi hasParens funcType ->
    notAHole $ PiEdit.make make hasParens funcType
  Sugar.ExpressionSection hasParens section ->
    notAHole $ SectionEdit.make make hasParens section
  Sugar.ExpressionLiteralInteger integer ->
    notAHole $ LiteralEdit.makeInt integer
  Sugar.ExpressionAtom atom ->
    notAHole $ AtomEdit.make atom
  where
    isAHole hole =
      (fmap . liftM)
      (first IsAHole .
       (second . ExpressionGui.atEgWidget . Widget.weakerEvents) (pasteEventMap hole))
    notAHole = (fmap . liftM) ((,) NotAHole)

expressionEventMap
  :: MonadF m
  => HoleResultPicker m
  -> Sugar.Actions m
  -> EventHandlers (ITransaction ViewTag m)
expressionEventMap holePicker actions =
  mconcat
    [ giveAsArg
    , callWithArg
    , addArg
    , replace
    , lambdaWrap
    , addWhereItem
    , cut
    ]
  where
    itrans = liftM WidgetIds.fromGuid . IT.transaction
    giveAsArg =
      moveUnlessOnHole .
      Widget.keysEventMapMovesCursor
      Config.giveAsArgumentKeys "Give as argument" . itrans $
      Sugar.giveAsArg actions
    callWithArg =
      moveUnlessOnHole .
      Widget.keysEventMapMovesCursor
      Config.callWithArgumentKeys "Call with argument" . itrans $
      Sugar.callWithArg actions
    addArg =
      maybeMempty (Sugar.mNextArg actions) moveToIfHole
      -- Move to next arg overrides add arg's keys.
      `mappend`
      (withPickResultFirst Config.addNextArgumentKeys "Add arg" . itrans $
       Sugar.addNextArg actions)
    cut =
      if isHole then mempty else
      mkEventMap Config.cutKeys "Cut" id $
      Sugar.cut actions
    replace =
      if isHole then mempty else
      mkEventMap Config.replaceKeys "Replace" FocusDelegator.delegatingId $
      Sugar.replace actions
    lambdaWrap =
      mkEventMap Config.lambdaWrapKeys "Lambda wrap" FocusDelegator.delegatingId $
      Sugar.lambdaWrap actions
    addWhereItem =
      mkEventMap Config.addWhereItemKeys "Add where item" FocusDelegator.delegatingId $
      Sugar.addWhereItem actions

    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      liftM (f . WidgetIds.fromGuid) . IT.transaction

    withPickResultFirst keys doc action =
      case holePicker of
        IsAHole (Just pickResult) ->
          E.keyPresses keys ("Pick result and " ++ doc) $
          combineActions pickResult action
        _ ->
          Widget.keysEventMapMovesCursor keys doc action
    combineActions pickResult action = do
      eventResult <- pickResult
      cursorId <- action
      return $
        (Widget.atECursor . const . Just) cursorId
        eventResult

    moveUnlessOnHole = ifHole $ (const . fmap . liftM . Widget.atECursor . const) Nothing
    isHole = case holePicker of
      NotAHole -> False
      IsAHole _ -> True
    ifHole whenHole = case holePicker of
      NotAHole -> id
      IsAHole x -> whenHole x
    maybeMempty x f = maybe mempty f x
    moveToIfHole nextArg =
      case Sugar.rExpression nextArg of
      Sugar.ExpressionHole{} ->
        withPickResultFirst Config.addNextArgumentKeys "Move to next arg" .
        return . WidgetIds.fromGuid $ Sugar.rGuid nextArg
      _ -> mempty
