{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first, second)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Control.Lens as Lens
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
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
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
  typeEdits <- mapM make $ Sugar.plInferredTypes payload
  let onReadOnly = Widget.doesntTakeFocus
  exprEventMap <- expressionEventMap exprGuid holePicker $ Sugar.rPayload sExpr
  settings <- VarAccess.otransaction OT.readSettings
  let
    addInferredTypes
      | Lens.view OT.vsShowInferredTypes settings =
        ExpressionGui.addType ExpressionGui.Background exprId
        (map
         ( Widget.tint Config.inferredTypeTint
         . Widget.scale Config.typeScaleFactor
         . ExpressionGui.egWidget
         ) typeEdits)
      | otherwise = id
  return .
    ExpressionGui.atEgWidget
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
  -> VarAccess m (HoleResultPicker m, ExpressionGui m)
makeEditor sExpr =
  case Sugar.rExpressionBody sExpr of
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

expressionEventMap ::
  MonadF m =>
  Guid -> HoleResultPicker m ->
  Sugar.Payload m ->
  VarAccess m (EventHandlers (ITransaction ViewTag m))
expressionEventMap exprGuid holePicker payload =
  liftM mconcat $ sequence
  [ return . holeEvents holePicker $ Sugar.plNextHole payload
  , maybe (return mempty) (actionsEventMap exprGuid holePicker) $ Sugar.plActions payload
  ]

holeEvents ::
  MonadF m =>
  HoleResultPicker m -> Maybe (Sugar.Expression m) ->
  EventHandlers (ITransaction ViewTag m)
holeEvents (IsAHole (Just (False, pickResult))) (Just nextHole) =
  E.keyPresses Config.addNextArgumentKeys
  "Pick result and move to next arg" $ do
    eventResult <- pickResult
    return $
      (Widget.atECursor . const . Just . WidgetIds.fromGuid . Sugar.rGuid) nextHole
      eventResult
holeEvents (IsAHole (Just (_, pickResult))) _ =
  E.keyPresses Config.addNextArgumentKeys
  HoleEdit.pickResultText pickResult
holeEvents _ (Just nextHole) =
  Widget.keysEventMapMovesCursor
  Config.addNextArgumentKeys "Move to next arg" .
  return . WidgetIds.fromGuid $ Sugar.rGuid nextHole
holeEvents _ _ = mempty

actionsEventMap ::
  MonadF m =>
  Guid -> HoleResultPicker m -> Sugar.Actions m ->
  VarAccess m (EventHandlers (ITransaction ViewTag m))
actionsEventMap exprGuid holePicker actions = do
  isSelected <-
    VarAccess.otransaction . OT.isSubCursor $
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
    itrans = liftM WidgetIds.fromGuid . IT.transaction
    giveAsArg =
      moveUnlessOnHole .
      Widget.keysEventMapMovesCursor
      Config.giveAsArgumentKeys "Give as argument" . itrans $
      Sugar.giveAsArg actions
    addOperator =
      (fmap . fmap) Widget.eventResultFromCursor .
      E.filterChars (`elem` Config.operatorChars) .
      E.simpleChars "Operator" "Add operator" $
      itrans . Sugar.giveAsArgToOperator actions . (:[])
    cut =
      if isHole then mempty else
      mkEventMap Config.cutKeys "Cut" id $
      Sugar.cut actions
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      liftM (f . WidgetIds.fromGuid) . IT.transaction

    moveUnlessOnHole = ifHole $ (const . fmap . liftM . Widget.atECursor . const) Nothing
    isHole = case holePicker of
      NotAHole -> False
      IsAHole _ -> True
    ifHole whenHole = case holePicker of
      NotAHole -> id
      IsAHole x -> whenHole x
