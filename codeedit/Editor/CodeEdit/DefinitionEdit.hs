{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make, makeParts) where

import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, WidgetT)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.BuiltinEdit as BuiltinEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
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

makeNameEdit :: MonadF m => Widget.Id -> Guid -> OTransaction t m (WidgetT t m)
makeNameEdit myId ident =
  BWidgets.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (OT.atEnv (BWidgets.setTextColor Config.definitionOriginColor) .
   BWidgets.makeNameEdit ident)
  myId

makeLHSEdit
  :: MonadF m
  => ExpressionGui.Maker m
  -> Widget.Id
  -> Guid
  -> Maybe (Transaction ViewTag m Guid)
  -> (E.Doc, Sugar.ExpressionRef m)
  -> [Sugar.FuncParam m]
  -> OTransaction ViewTag m (ExpressionGui m)
makeLHSEdit makeExpressionEdit myId ident mAddFirstParameter rhs params = do
  nameEdit <-
    liftM (FuncEdit.addJumpToRHS rhs . Widget.weakerEvents addFirstParamEventMap) $
    makeNameEdit myId ident
  liftM (ExpressionGui.hboxSpaced . (ExpressionGui.fromValueWidget nameEdit :)) .
    mapM (FuncEdit.makeParamEdit makeExpressionEdit rhs) $ params
  where
    addFirstParamEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys
       "Add parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
       IT.transaction)
      mAddFirstParameter

makeEquals :: MonadF m => Widget.Id -> OTransaction ViewTag m (Widget f)
makeEquals = BWidgets.makeLabel "=" . Widget.toAnimId

makeParts
  :: MonadF m
  => ExpressionGui.Maker m
  -> Guid
  -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m [ExpressionGui m]
makeParts makeExpressionEdit guid exprRef = do
  lhsEdit <-
    makeLHSEdit makeExpressionEdit myId guid
    ((fmap Sugar.lambdaWrap . Sugar.rActions) exprRef)
    ("Def Body", Sugar.fBody func) $ Sugar.fParams func
  equals <- makeEquals myId
  let
    lhs = myId : map (WidgetIds.fromGuid . Sugar.fpGuid) (Sugar.fParams func)
  rhsEdit <-
    FuncEdit.makeBodyEdit makeExpressionEdit lhs $ Sugar.fBody func
  return
    [ lhsEdit
    , ExpressionGui.fromValueWidget BWidgets.spaceWidget
    , ExpressionGui.fromValueWidget equals
    , ExpressionGui.fromValueWidget BWidgets.spaceWidget
    , rhsEdit
    ]
  where
    myId = WidgetIds.fromGuid guid
    sExpr = Sugar.rExpression exprRef
    func =
      case sExpr of
      Sugar.ExpressionFunc _ x -> x
      _ -> Sugar.Func [] exprRef

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.DefinitionRef m
  -> OTransaction ViewTag m (WidgetT ViewTag m)
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
  -> OTransaction ViewTag m (WidgetT ViewTag m)
makeBuiltinDefinition makeExpressionEdit def builtin =
  liftM (BWidgets.vboxAlign 0) $ sequence
  [ liftM BWidgets.hboxCenteredSpaced $ sequence
    [ makeNameEdit (Widget.joinId myId ["name"]) guid
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
  (Sugar.ExpressionRef m -> OTransaction ViewTag m (ExpressionGui m)) ->
  Sugar.DefinitionRef m ->
  Sugar.DefinitionExpression m ->
  OTransaction ViewTag m (OT.WidgetT ViewTag m)
makeExprDefinition makeExpressionEdit def bodyExpr = do
  bodyWidget <-
    liftM (ExpressionGui.egWidget . ExpressionGui.hbox) .
    makeParts makeExpressionEdit guid . Sugar.deExprRef $ bodyExpr
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
        BWidgets.makeFocusableTextView "↱" $ Widget.joinId myId ["accept type"]
      return $ BWidgets.hboxCenteredSpaced [acceptanceLabel, label]
    right = Vector2 1 0.5
    center = 0.5
    mkTypeRow onLabel labelText typeExpr = do
      label <- onLabel . labelStyle . BWidgets.makeLabel labelText $ Widget.toAnimId myId
      typeGui <- makeExpressionEdit typeExpr
      return
        [ (right, label)
        , (center, (Widget.doesntTakeFocus . ExpressionGui.egWidget) typeGui)
        ]
    mkAcceptedRow onLabel = mkTypeRow onLabel "Type:" $ Sugar.drType def
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid
    labelStyle = OT.atEnv $ OT.setTextSizeColor Config.defTypeLabelTextSize Config.defTypeLabelColor
