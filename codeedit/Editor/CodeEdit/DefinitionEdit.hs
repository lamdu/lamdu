{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make, makeParts) where

import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess, WidgetT)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.List as List
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.BuiltinEdit as BuiltinEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Change parameter name"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Stop changing name"
  }

makeNameEdit ::
  MonadF m => (VarAccess.NameSource, String) -> Widget.Id -> Guid -> VarAccess m (WidgetT m)
makeNameEdit name myId ident =
  BWidgets.wrapDelegatedVA paramFDConfig FocusDelegator.NotDelegating id
  (VarAccess.atEnv (BWidgets.setTextColor Config.definitionOriginColor) .
   BWidgets.makeNameEdit name ident)
  myId

makeEquals :: MonadF m => Widget.Id -> VarAccess m (Widget f)
makeEquals = VarAccess.otransaction . BWidgets.makeLabel "=" . Widget.toAnimId

makeParts
  :: MonadF m
  => ExpressionGui.Maker m
  -> (VarAccess.NameSource, String)
  -> Guid
  -> Sugar.DefinitionExpression m
  -> VarAccess m [ExpressionGui m]
makeParts makeExpressionEdit name guid def = do
  nameEdit <-
    liftM (FuncEdit.addJumpToRHS rhs . Widget.weakerEvents addFirstParamEventMap) $
    makeNameEdit name myId guid
  equals <- makeEquals myId
  (paramsEdits, bodyEdit) <-
    FuncEdit.makeParamsAndResultEdit makeExpressionEdit lhs rhs params
  return .
    List.intersperse (ExpressionGui.fromValueWidget BWidgets.spaceWidget) $
    ExpressionGui.fromValueWidget nameEdit :
    paramsEdits ++
    [ ExpressionGui.fromValueWidget equals
    , bodyEdit
    ]
  where
    lhs = myId : map (WidgetIds.fromGuid . Sugar.fpGuid) params
    rhs = ("Def Body", exprRef)
    params = Sugar.deParameters def
    addFirstParamEventMap =
      maybe mempty
      ( Widget.keysEventMapMovesCursor Config.addNextParamKeys "Add parameter"
      . liftM (FocusDelegator.delegatingId . WidgetIds.fromGuid)
      . IT.transaction
      ) $ case params of
      (p : _) -> fmap Sugar.fpaAddPrevParam $ Sugar.fpMActions p
      _ -> fmap Sugar.lambdaWrap $ Sugar.rActions exprRef
    exprRef = Sugar.deExprRef def
    myId = WidgetIds.fromGuid guid

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.DefinitionRef m
  -> VarAccess m (WidgetT m)
make makeExpressionEdit def =
  case Sugar.drBody def of
  Sugar.DefinitionBodyExpression bodyExpr ->
    makeExprDefinition makeExpressionEdit def bodyExpr
  Sugar.DefinitionBodyBuiltin builtin ->
    makeBuiltinDefinition makeExpressionEdit def builtin

makeBuiltinDefinition
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.DefinitionRef m
  -> Sugar.DefinitionBuiltin m
  -> VarAccess m (WidgetT m)
makeBuiltinDefinition makeExpressionEdit def builtin =
  liftM (BWidgets.vboxAlign 0) $ sequence
  [ liftM BWidgets.hboxCenteredSpaced $ sequence
    [ VarAccess.withName guid $ \name -> makeNameEdit name (Widget.joinId myId ["name"]) guid
    , makeEquals myId
    , BuiltinEdit.make builtin myId
    ]
  , liftM (defTypeScale . ExpressionGui.egWidget) . makeExpressionEdit $
    Sugar.drType def
  ]
  where
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid

defTypeScale :: Widget f -> Widget f
defTypeScale = Widget.scale Config.defTypeBoxSizeFactor

makeExprDefinition ::
  MonadF m =>
  (Sugar.Expression m -> VarAccess m (ExpressionGui m)) ->
  Sugar.DefinitionRef m ->
  Sugar.DefinitionExpression m ->
  VarAccess m (WidgetT m)
makeExprDefinition makeExpressionEdit def bodyExpr = do
  name <- VarAccess.getDefName guid
  bodyWidget <-
    liftM (ExpressionGui.egWidget . ExpressionGui.hbox) $
    makeParts makeExpressionEdit name guid bodyExpr
  let
    mkResult typeWidget =
      BWidgets.vboxAlign 0
      [ defTypeScale typeWidget
      , bodyWidget
      ]
  case Sugar.deMNewType bodyExpr of
    Nothing
      | Sugar.deIsTypeRedundant bodyExpr -> return bodyWidget
      | otherwise -> liftM (mkResult . BWidgets.hboxSpaced) (mkAcceptedRow id)
    Just (Sugar.DefinitionNewType inferredType acceptInferredType) ->
      liftM (mkResult . BWidgets.gridHSpaced) $ sequence
      [ mkAcceptedRow (>>= addAcceptanceArrow acceptInferredType)
      , mkTypeRow id "Inferred type:" inferredType
      ]
  where
    addAcceptanceArrow acceptInferredType label = do
      acceptanceLabel <-
        (liftM . Widget.weakerEvents)
        (Widget.keysEventMapMovesCursor Config.acceptInferredTypeKeys
         "Accept inferred type"
         (IT.transaction acceptInferredType >> return myId)) .
        VarAccess.otransaction .
        BWidgets.makeFocusableTextView "↱" $ Widget.joinId myId ["accept type"]
      return $ BWidgets.hboxCenteredSpaced [acceptanceLabel, label]
    right = Vector2 1 0.5
    center = 0.5
    mkTypeRow onLabel labelText typeExpr = do
      label <-
        onLabel . labelStyle . VarAccess.otransaction .
        BWidgets.makeLabel labelText $ Widget.toAnimId myId
      typeGui <- makeExpressionEdit typeExpr
      return
        [ (right, label)
        , (center, (Widget.doesntTakeFocus . ExpressionGui.egWidget) typeGui)
        ]
    mkAcceptedRow onLabel = mkTypeRow onLabel "Type:" $ Sugar.drType def
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid
    labelStyle =
      VarAccess.atEnv $ OT.setTextSizeColor Config.defTypeLabelTextSize Config.defTypeLabelColor
