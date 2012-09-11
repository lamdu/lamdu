{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make) where

import Control.Monad (liftM)
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
  (VarAccess.atEnv (OT.setTextColor Config.definitionOriginColor) .
   BWidgets.makeNameEdit name ident)
  myId

makeEquals :: MonadF m => Widget.Id -> VarAccess m (Widget f)
makeEquals = VarAccess.otransaction . BWidgets.makeLabel "=" . Widget.toAnimId

makeParts
  :: MonadF m
  => ExpressionGui.Maker m
  -> (VarAccess.NameSource, String)
  -> Guid
  -> Sugar.DefinitionContent m
  -> VarAccess m [ExpressionGui m]
makeParts makeExpressionEdit name guid def = do
  nameEdit <-
    liftM (FuncEdit.addJumpToRHS rhs . Widget.weakerEvents addFirstParamEventMap) $
    makeNameEdit name myId guid
  equals <- makeEquals myId
  (paramsEdits, bodyEdit) <-
    FuncEdit.makeParamsAndResultEdit makeExpressionEdit lhs rhs myId params
  return .
    List.intersperse (ExpressionGui.fromValueWidget BWidgets.spaceWidget) $
    ExpressionGui.fromValueWidget nameEdit :
    paramsEdits ++
    [ ExpressionGui.fromValueWidget equals
    , bodyEdit
    ]
  where
    lhs = myId : map (WidgetIds.fromGuid . Sugar.fpGuid) params
    rhs = ("Def Body", body)
    params = Sugar.dParameters def
    addFirstParamEventMap =
      Widget.keysEventMapMovesCursor Config.addNextParamKeys "Add parameter" .
      liftM (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
      IT.transaction $
      Sugar.dAddFirstParam def
    body = Sugar.dBody def
    myId = WidgetIds.fromGuid guid

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Definition m
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
  -> Sugar.Definition m
  -> Sugar.DefinitionBuiltin m
  -> VarAccess m (WidgetT m)
makeBuiltinDefinition makeExpressionEdit def builtin =
  liftM (BWidgets.vboxAlign 0) $ sequence
  [ liftM BWidgets.hboxCenteredSpaced $ sequence
    [ VarAccess.withParamName guid $ \name -> makeNameEdit name (Widget.joinId myId ["name"]) guid
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

makeWhereItemEdit ::
  MonadF m =>
  ExpressionGui.Maker m ->
  Sugar.WhereItem m -> VarAccess m (WidgetT m)
makeWhereItemEdit makeExpressionEdit item =
  liftM (Widget.weakerEvents deleteEventMap) . assignCursor $
  makeDefBodyEdit makeExpressionEdit (Sugar.wiGuid item) (Sugar.wiValue item)
  where
    assignCursor =
      foldr ((.) . (`VarAccess.assignCursor` myId) . WidgetIds.fromGuid) id $
      Sugar.wiHiddenGuids item
    myId = WidgetIds.fromGuid $ Sugar.wiGuid item
    deleteEventMap =
      Widget.keysEventMapMovesCursor (Config.delForwardKeys ++ Config.delBackwordKeys)
      "Delete where item" .
      liftM WidgetIds.fromGuid .
      IT.transaction $ Sugar.wiDelete item

makeDefBodyEdit ::
  MonadF m =>
  ExpressionGui.Maker m ->
  Guid -> Sugar.DefinitionContent m ->
  VarAccess m (WidgetT m)
makeDefBodyEdit makeExpressionEdit guid content = do
  name <- VarAccess.getDefName guid
  body <- liftM (ExpressionGui.egWidget . ExpressionGui.hbox) $
    makeParts makeExpressionEdit name guid content
  wheres <-
    case Sugar.dWhereItems content of
    [] -> return []
    whereItems -> do
      whereLabel <-
        (liftM . Widget.scale) Config.whereLabelScaleFactor .
        VarAccess.otransaction . BWidgets.makeLabel "where" $ Widget.toAnimId myId
      itemEdits <- mapM (makeWhereItemEdit makeExpressionEdit) $ reverse whereItems
      return
        [ BWidgets.hboxSpaced
          [ (0, whereLabel)
          , (0, Widget.scale Config.whereScaleFactor $ BWidgets.vboxAlign 0 itemEdits)
          ]
        ]
  return . BWidgets.vboxAlign 0 $ body : wheres
  where
    myId = WidgetIds.fromGuid guid

makeExprDefinition ::
  MonadF m =>
  (Sugar.Expression m -> VarAccess m (ExpressionGui m)) ->
  Sugar.Definition m ->
  Sugar.DefinitionExpression m ->
  VarAccess m (WidgetT m)
makeExprDefinition makeExpressionEdit def bodyExpr = do
  typeWidgets <-
    case Sugar.deMNewType bodyExpr of
    Nothing
      | Sugar.deIsTypeRedundant bodyExpr -> return []
      | otherwise -> liftM ((:[]) . defTypeScale . BWidgets.hboxSpaced) (mkAcceptedRow id)
    Just (Sugar.DefinitionNewType inferredType acceptInferredType) ->
      liftM ((:[]) . defTypeScale . BWidgets.gridHSpaced) $ sequence
      [ mkAcceptedRow (>>= addAcceptanceArrow acceptInferredType)
      , mkTypeRow id "Inferred type:" inferredType
      ]
  bodyWidget <-
    makeDefBodyEdit makeExpressionEdit guid $ Sugar.deContent bodyExpr
  return . BWidgets.vboxAlign 0 $ typeWidgets ++ [bodyWidget]
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
