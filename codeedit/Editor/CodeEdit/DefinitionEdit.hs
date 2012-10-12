{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make) where

import Control.Monad (liftM)
import Data.List.Utils (nonEmptyAll)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ExprGuiM, WidgetT)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.List as List
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.BuiltinEdit as BuiltinEdit
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Change parameter name"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Stop changing name"
  }

makeNameEdit ::
  MonadF m => (ExprGuiM.NameSource, String) -> Widget.Id -> Guid -> ExprGuiM m (WidgetT m)
makeNameEdit name myId ident =
  ExpressionGui.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (ExprGuiM.atEnv (OT.setTextColor Config.definitionOriginColor) .
   ExpressionGui.makeNameEdit name ident)
  myId

makeEquals :: MonadF m => Widget.Id -> ExprGuiM m (Widget f)
makeEquals = ExprGuiM.otransaction . BWidgets.makeLabel "=" . Widget.toAnimId

nonOperatorName :: (ExprGuiM.NameSource, String) -> Bool
nonOperatorName (ExprGuiM.StoredName, x) = nonEmptyAll (`notElem` Config.operatorChars) x
nonOperatorName _ = False

makeParts
  :: MonadF m
  => (ExprGuiM.NameSource, String)
  -> Guid
  -> Sugar.DefinitionContent m
  -> ExprGuiM m [ExpressionGui m]
makeParts name guid def = do
  nameEdit <-
    liftM
    (Widget.weakerEvents
     (FuncEdit.jumpToRHS Config.jumpLHStoRHSKeys rhs
      `mappend` addFirstParamEventMap) .
     jumpToRHSViaEquals name) $
    makeNameEdit name myId guid
  equals <- makeEquals myId
  (paramsEdits, bodyEdit) <-
    FuncEdit.makeParamsAndResultEdit jumpToRHSViaEquals lhs rhs myId params
  return .
    List.intersperse (ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget) $
    ExpressionGui.fromValueWidget nameEdit :
    paramsEdits ++
    [ ExpressionGui.fromValueWidget equals
    , ExpressionGui.atEgWidget
      (Widget.weakerEvents addWhereItemEventMap)
      bodyEdit
    ]
  where
    jumpToRHSViaEquals n
      | nonOperatorName n =
        Widget.weakerEvents
        (FuncEdit.jumpToRHS [E.ModKey E.noMods (E.charKey '=')] rhs) .
        Widget.atWEventMap (E.filterChars (/= '='))
      | otherwise = id
    lhs = myId : map (WidgetIds.fromGuid . Sugar.fpGuid) params
    rhs = ("Def Body", body)
    params = Sugar.dParameters def
    addWhereItemEventMap =
      Widget.keysEventMapMovesCursor Config.addWhereItemKeys "Add where item" .
      toEventMapAction $ Sugar.dAddInnermostWhereItem def
    addFirstParamEventMap =
      Widget.keysEventMapMovesCursor Config.addNextParamKeys "Add parameter" .
      toEventMapAction $ Sugar.dAddFirstParam def
    toEventMapAction =
      liftM (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
      IT.transaction
    body = Sugar.dBody def
    myId = WidgetIds.fromGuid guid

make
  :: MonadF m
  => Sugar.Definition m
  -> ExprGuiM m (WidgetT m)
make def =
  case Sugar.drBody def of
  Sugar.DefinitionBodyExpression bodyExpr ->
    makeExprDefinition def bodyExpr
  Sugar.DefinitionBodyBuiltin builtin ->
    makeBuiltinDefinition def builtin

makeBuiltinDefinition
  :: MonadF m
  => Sugar.Definition m
  -> Sugar.DefinitionBuiltin m
  -> ExprGuiM m (WidgetT m)
makeBuiltinDefinition def builtin =
  liftM (Box.vboxAlign 0) $ sequence
  [ liftM BWidgets.hboxCenteredSpaced $ sequence
    [ ExprGuiM.withParamName guid $ \name -> makeNameEdit name (Widget.joinId myId ["name"]) guid
    , makeEquals myId
    , BuiltinEdit.make builtin myId
    ]
  , liftM (defTypeScale . ExpressionGui.egWidget) . ExpressionGui.makeSubexpresion $
    Sugar.drType def
  ]
  where
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid

defTypeScale :: Widget f -> Widget f
defTypeScale = Widget.scale Config.defTypeBoxSizeFactor

makeWhereItemEdit :: MonadF m => Sugar.WhereItem m -> ExprGuiM m (WidgetT m)
makeWhereItemEdit item =
  liftM (Widget.weakerEvents eventMap) . assignCursor $
  makeDefBodyEdit (Sugar.wiGuid item) (Sugar.wiValue item)
  where
    assignCursor =
      foldr ((.) . (`ExprGuiM.assignCursor` myId) . WidgetIds.fromGuid) id $
      Sugar.wiHiddenGuids item
    myId = WidgetIds.fromGuid $ Sugar.wiGuid item
    eventMap =
      mconcat
      [ Widget.keysEventMapMovesCursor (Config.delForwardKeys ++ Config.delBackwordKeys)
        "Delete where item" .
        liftM WidgetIds.fromGuid .
        IT.transaction . Sugar.itemDelete $ Sugar.wiActions item
      , Widget.keysEventMapMovesCursor Config.addWhereItemKeys
        "Add outer where item" .
        liftM WidgetIds.fromGuid .
        IT.transaction . Sugar.itemAddNext $ Sugar.wiActions item
      ]

makeDefBodyEdit ::
  MonadF m => Guid -> Sugar.DefinitionContent m -> ExprGuiM m (WidgetT m)
makeDefBodyEdit guid content = do
  name <- ExprGuiM.getDefName guid
  body <- liftM (ExpressionGui.egWidget . ExpressionGui.hbox) $
    makeParts name guid content
  wheres <-
    case Sugar.dWhereItems content of
    [] -> return []
    whereItems -> do
      whereLabel <-
        (liftM . Widget.scale) Config.whereLabelScaleFactor .
        ExprGuiM.otransaction . BWidgets.makeLabel "where" $ Widget.toAnimId myId
      itemEdits <- mapM makeWhereItemEdit $ reverse whereItems
      return
        [ BWidgets.hboxSpaced
          [ (0, whereLabel)
          , (0, Widget.scale Config.whereScaleFactor $ Box.vboxAlign 0 itemEdits)
          ]
        ]
  return . Box.vboxAlign 0 $ body : wheres
  where
    myId = WidgetIds.fromGuid guid

makeExprDefinition ::
  MonadF m => Sugar.Definition m -> Sugar.DefinitionExpression m ->
  ExprGuiM m (WidgetT m)
makeExprDefinition def bodyExpr = do
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
    makeDefBodyEdit guid $ Sugar.deContent bodyExpr
  return . Box.vboxAlign 0 $ typeWidgets ++ [bodyWidget]
  where
    addAcceptanceArrow acceptInferredType label = do
      acceptanceLabel <-
        (liftM . Widget.weakerEvents)
        (Widget.keysEventMapMovesCursor Config.acceptInferredTypeKeys
         "Accept inferred type"
         (IT.transaction acceptInferredType >> return myId)) .
        ExprGuiM.otransaction .
        BWidgets.makeFocusableTextView "↱" $ Widget.joinId myId ["accept type"]
      return $ BWidgets.hboxCenteredSpaced [acceptanceLabel, label]
    right = Vector2 1 0.5
    center = 0.5
    mkTypeRow onLabel labelText typeExpr = do
      label <-
        onLabel . labelStyle . ExprGuiM.otransaction .
        BWidgets.makeLabel labelText $ Widget.toAnimId myId
      typeGui <- ExpressionGui.makeSubexpresion typeExpr
      return
        [ (right, label)
        , (center, (Widget.doesntTakeFocus . ExpressionGui.egWidget) typeGui)
        ]
    mkAcceptedRow onLabel = mkTypeRow onLabel "Type:" $ Sugar.drType def
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid
    labelStyle =
      ExprGuiM.atEnv $ OT.setTextSizeColor Config.defTypeLabelTextSize Config.defTypeLabelColor
