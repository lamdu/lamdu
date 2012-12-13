{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Lamdu.CodeEdit.ExpressionEdit.DefinitionEdit(make, diveToNameEdit) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse, sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, Collapser(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename definition"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeNameEdit ::
  MonadA m => (ExprGuiM.NameSource, String) ->
  Widget.Id -> Guid -> ExprGuiM m (WidgetT m)
makeNameEdit name myId ident =
  ExprGuiM.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (ExpressionGui.makeNameEdit name ident)
  myId

makeEquals :: MonadA m => Widget.Id -> ExprGuiM m (Widget f)
makeEquals = ExprGuiM.widgetEnv . BWidgets.makeLabel "=" . Widget.toAnimId

nonOperatorName :: (ExprGuiM.NameSource, String) -> Bool
nonOperatorName (ExprGuiM.StoredName, x) = nonEmptyAll (`notElem` Config.operatorChars) x
nonOperatorName _ = False

polyNameFDConfig :: FocusDelegator.Config
polyNameFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.polymorphicExpandKey
  , FocusDelegator.startDelegatingDoc = E.Doc ["View", "Expand polymorphic"]
  , FocusDelegator.stopDelegatingKey = Config.polymorphicCollapseKey
  , FocusDelegator.stopDelegatingDoc = E.Doc ["View", "Collapse polymorphic"]
  }

makePolyNameEdit ::
  MonadA m =>
  (ExprGuiM.NameSource, String) -> Guid -> [ExpressionGui m] -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makePolyNameEdit name guid depParamsEdits =
  case depParamsEdits of
  [] -> makeNameGui Config.monomorphicDefOriginForegroundColor
  _ -> ExpressionGui.makeCollapser polyNameFDConfig f
  where
    f myId =
      Collapser
      { cMakeExpanded =
        ExpressionGui.hboxSpaced . (: depParamsEdits) <$>
        nameGui Config.monomorphicDefOriginForegroundColor
      , cOnFocusedExpanded =
        ExpressionGui.withBgColor Layers.polymorphicExpandedBG
        Config.polymorphicExpandedBGColor bgId
      , cMakeFocusedCompact =
        nameGui Config.polymorphicDefOriginForegroundColor
      }
      where
        nameGui color = makeNameGui color myId
        bgId = Widget.toAnimId myId ++ ["bg"]
    makeNameGui color myId =
      ExprGuiM.withFgColor color $
      ExpressionGui.fromValueWidget <$> makeNameEdit name myId guid

makeParts
  :: MonadA m
  => (ExprGuiM.NameSource, String)
  -> Guid
  -> Sugar.DefinitionContent m
  -> ExprGuiM m [ExpressionGui m]
makeParts name guid def = do
  equals <- makeEquals myId
  (depParamsEdits, paramsEdits, bodyEdit) <-
    FuncEdit.makeParamsAndResultEdit
    jumpToRHSViaEquals lhs rhs myId depParams params
  polyNameEdit <-
    Lens.over ExpressionGui.egWidget
    (Widget.weakerEvents nameEditEventMap . jumpToRHSViaEquals name) <$>
    makePolyNameEdit name guid depParamsEdits myId
  return $
    polyNameEdit : paramsEdits ++
    [ ExpressionGui.fromValueWidget equals
    , Lens.over ExpressionGui.egWidget
      (Widget.weakerEvents addWhereItemEventMap)
      bodyEdit
    ]
  where
    jumpToRHSViaEquals n
      | nonOperatorName n =
        Widget.weakerEvents
        (FuncEdit.jumpToRHS [E.ModKey E.noMods (E.charKey '=')] rhs) .
        Lens.over Widget.wEventMap (E.filterChars (/= '='))
      | otherwise = id
    lhs = myId : map (WidgetIds.fromGuid . Lens.view Sugar.fpGuid) allParams
    rhs = ("Def Body", body)
    allParams = depParams ++ params
    Sugar.Func depParams params body = Sugar.dFunc def
    addWhereItemEventMap =
      Widget.keysEventMapMovesCursor Config.addWhereItemKeys (E.Doc ["Edit", "Add where item"]) .
      toEventMapAction $ Sugar.dAddInnermostWhereItem def
    addFirstParamEventMap =
      Widget.keysEventMapMovesCursor Config.addNextParamKeys (E.Doc ["Edit", "Add parameter"]) .
      toEventMapAction $ Sugar.dAddFirstParam def
    nameEditEventMap =
      mappend addFirstParamEventMap $
      FuncEdit.jumpToRHS Config.jumpLHStoRHSKeys rhs
    toEventMapAction =
      fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid)
    myId = WidgetIds.fromGuid guid

make
  :: MonadA m
  => Sugar.Definition m
  -> ExprGuiM m (WidgetT m)
make def =
  case Sugar.drBody def of
  Sugar.DefinitionBodyExpression bodyExpr ->
    makeExprDefinition def bodyExpr
  Sugar.DefinitionBodyBuiltin builtin ->
    makeBuiltinDefinition def builtin

makeBuiltinDefinition
  :: MonadA m
  => Sugar.Definition m
  -> Sugar.DefinitionBuiltin m
  -> ExprGuiM m (WidgetT m)
makeBuiltinDefinition def builtin =
  fmap (Box.vboxAlign 0) $ sequenceA
  [ fmap BWidgets.hboxCenteredSpaced $ sequenceA
    [ ExprGuiM.withParamName guid $
      \name ->
      ExprGuiM.atEnv (WE.setTextColor Config.builtinOriginNameColor) $
      makeNameEdit name (Widget.joinId myId ["name"]) guid
    , makeEquals myId
    , BuiltinEdit.make builtin myId
    ]
  , fmap (defTypeScale . Lens.view ExpressionGui.egWidget) .
    ExprGuiM.makeSubexpresion $ Sugar.drType def
  ]
  where
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid

defTypeScale :: Widget f -> Widget f
defTypeScale = Widget.scale Config.defTypeBoxSizeFactor

makeWhereItemEdit :: MonadA m => Sugar.WhereItem m -> ExprGuiM m (WidgetT m)
makeWhereItemEdit item =
  fmap (Widget.weakerEvents eventMap) . assignCursor $
  makeDefContentEdit (Sugar.wiGuid item) (Sugar.wiValue item)
  where
    assignCursor =
      foldr ((.) . (`ExprGuiM.assignCursor` myId) . WidgetIds.fromGuid) id $
      Sugar.wiHiddenGuids item
    myId = WidgetIds.fromGuid $ Sugar.wiGuid item
    eventMap
      | Just wiActions <- Sugar.wiActions item =
      mconcat
      [ Widget.keysEventMapMovesCursor (Config.delForwardKeys ++ Config.delBackwordKeys)
        (E.Doc ["Edit", "Where item", "Delete"]) .
        fmap WidgetIds.fromGuid $
        Lens.view Sugar.itemDelete wiActions
      , Widget.keysEventMapMovesCursor Config.addWhereItemKeys
        (E.Doc ["Edit", "Where item", "Add"]) .
        fmap WidgetIds.fromGuid $
        Lens.view Sugar.itemAddNext wiActions
      ]
      | otherwise = mempty

makeDefContentEdit ::
  MonadA m => Guid -> Sugar.DefinitionContent m -> ExprGuiM m (WidgetT m)
makeDefContentEdit guid content = do
  name <- ExprGuiM.getDefName guid
  body <-
    fmap (Lens.view ExpressionGui.egWidget . ExpressionGui.hboxSpaced) $
    makeParts name guid content
  wheres <-
    case Sugar.dWhereItems content of
    [] -> return []
    whereItems -> do
      whereLabel <-
        (fmap . Widget.scale) Config.whereLabelScaleFactor .
        ExprGuiM.widgetEnv . BWidgets.makeLabel "where" $ Widget.toAnimId myId
      itemEdits <- traverse makeWhereItemEdit $ reverse whereItems
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
  MonadA m => Sugar.Definition m -> Sugar.DefinitionExpression m ->
  ExprGuiM m (WidgetT m)
makeExprDefinition def bodyExpr = do
  typeWidgets <-
    case Sugar.deMNewType bodyExpr of
    Nothing
      | Sugar.deIsTypeRedundant bodyExpr -> return []
      | otherwise -> fmap ((:[]) . defTypeScale . BWidgets.hboxSpaced) (mkAcceptedRow id)
    Just (Sugar.DefinitionNewType inferredType acceptInferredType) ->
      fmap ((:[]) . defTypeScale . BWidgets.gridHSpaced) $ sequenceA
      [ mkAcceptedRow (>>= addAcceptanceArrow acceptInferredType)
      , mkTypeRow id "Inferred type:" inferredType
      ]
  bodyWidget <-
    makeDefContentEdit guid $ Sugar.deContent bodyExpr
  return . Box.vboxAlign 0 $ typeWidgets ++ [bodyWidget]
  where
    addAcceptanceArrow acceptInferredType label = do
      acceptanceLabel <-
        (fmap . Widget.weakerEvents)
        (Widget.keysEventMapMovesCursor Config.acceptInferredTypeKeys
         (E.Doc ["Edit", "Accept inferred type"]) (acceptInferredType >> return myId)) .
        ExprGuiM.widgetEnv .
        BWidgets.makeFocusableTextView "↱" $ Widget.joinId myId ["accept type"]
      return $ BWidgets.hboxCenteredSpaced [acceptanceLabel, label]
    right = Vector2 1 0.5
    center = 0.5
    mkTypeRow onLabel labelText typeExpr = do
      label <-
        onLabel . labelStyle . ExprGuiM.widgetEnv .
        BWidgets.makeLabel labelText $ Widget.toAnimId myId
      typeGui <- ExprGuiM.makeSubexpresion typeExpr
      return
        [ (right, label)
        , (center, (Widget.doesntTakeFocus . Lens.view ExpressionGui.egWidget) typeGui)
        ]
    mkAcceptedRow onLabel = mkTypeRow onLabel "Type:" $ Sugar.drType def
    guid = Sugar.drGuid def
    myId = WidgetIds.fromGuid guid
    labelStyle =
      ExprGuiM.atEnv $ WE.setTextSizeColor Config.defTypeLabelTextSize Config.defTypeLabelColor

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit =
  -- If we delegate too deep (e.g: No polymorphic params) that's
  -- handled OK. So we may as well assume we're always wrapped by a
  -- polymorphic wrapper:
  FocusDelegator.delegatingId . -- Polymorphic wrapper
  FocusDelegator.delegatingId -- Name editor
