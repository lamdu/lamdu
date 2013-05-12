{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Lamdu.CodeEdit.ExpressionEdit.DefinitionEdit(make, diveToNameEdit) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((%~), (&), (^.))
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
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

type T = Transaction

defFDConfig :: FocusDelegator.Config
defFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename definition"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeNameEdit ::
  MonadA m => Sugar.Name ->
  Widget.Id -> Guid -> ExprGuiM m (WidgetT m)
makeNameEdit name myId ident =
  ExprGuiM.wrapDelegated defFDConfig FocusDelegator.NotDelegating id
  (ExpressionGui.makeNameEdit name ident)
  myId

makeEquals :: MonadA m => Widget.Id -> ExprGuiM m (Widget f)
makeEquals = ExprGuiM.widgetEnv . BWidgets.makeLabel "=" . Widget.toAnimId

nonOperatorName :: Sugar.Name -> Bool
nonOperatorName (Sugar.Name Sugar.StoredName x) = nonEmptyAll (`notElem` Config.operatorChars) x
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
  Sugar.Name -> Guid -> [ExpressionGui m] -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makePolyNameEdit name guid depParamsEdits =
  case depParamsEdits of
  [] -> makeNameGui Config.monomorphicDefOriginForegroundColor
  _ -> ExpressionGui.makeCollapser polyNameFDConfig f
  where
    f myId =
      Collapser
      { cMakeExpanded =
        ExpressionGui.withBgColor Layers.polymorphicExpandedBG
        Config.polymorphicExpandedBGColor bgId .
        ExpressionGui.hboxSpaced . (: depParamsEdits) <$>
        nameGui Config.monomorphicDefOriginForegroundColor
      , cMakeFocusedCompact =
        nameGui Config.polymorphicDefOriginForegroundColor
      }
      where
        nameGui color = makeNameGui color myId
        bgId = Widget.toAnimId myId ++ ["bg"]
    makeNameGui color myId =
      ExprGuiM.withFgColor color $
      ExpressionGui.fromValueWidget <$> makeNameEdit name myId guid

makeWheres ::
  MonadA m =>
  [Sugar.WhereItem Sugar.Name m] -> Widget.Id ->
  ExprGuiM m [Widget (T m)]
makeWheres [] _ = return []
makeWheres whereItems myId = do
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

makeParts
  :: MonadA m
  => Sugar.Name
  -> Guid
  -> Sugar.DefinitionContent Sugar.Name m
  -> ExprGuiM m ([ExpressionGui m], [Widget (T m)])
makeParts name guid content = do
  equals <- makeEquals myId
  rhsJumperEquals <- FuncEdit.jumpToRHS [E.ModKey E.noMods (E.charKey '=')] rhs
  let
    jumpToRHSViaEquals n
      | nonOperatorName n =
        Widget.weakerEvents rhsJumperEquals .
        Lens.over Widget.wEventMap (E.filterSChars (curry (/= ('=', E.NotShifted))))
      | otherwise = id
  (depParamsEdits, paramsEdits, (wheres, bodyEdit)) <-
    FuncEdit.makeNestedParams
    jumpToRHSViaEquals rhs myId depParams params $
    (,)
    <$> makeWheres (Sugar.dWhereItems content) myId
    <*> FuncEdit.makeResultEdit lhs body
  rhsJumper <- FuncEdit.jumpToRHS Config.jumpLHStoRHSKeys rhs
  let nameEditEventMap = mappend addFirstParamEventMap rhsJumper
  polyNameEdit <-
    Lens.over ExpressionGui.egWidget
    (Widget.weakerEvents nameEditEventMap . jumpToRHSViaEquals name) <$>
    makePolyNameEdit name guid depParamsEdits myId
  savePos <- ExprGuiM.mkPrejumpPosSaver
  let
    addWhereItemEventMap =
      Widget.keysEventMapMovesCursor Config.addWhereItemKeys (E.Doc ["Edit", "Add where item"]) .
      toEventMapAction $ do
        savePos
        Sugar.dAddInnermostWhereItem content
  return
    ( polyNameEdit : paramsEdits ++
      [ ExpressionGui.fromValueWidget equals
      , Lens.over ExpressionGui.egWidget
        (Widget.weakerEvents addWhereItemEventMap)
        bodyEdit
      ]
    , wheres
    )
  where
    lhs = myId : map (WidgetIds.fromGuid . Lens.view Sugar.fpId) allParams
    rhs = ("Def Body", body)
    allParams = depParams ++ params
    depParams = Sugar.dDepParams content
    params = Sugar.dParams content
    body = Sugar.dBody content
    addFirstParamEventMap =
      Widget.keysEventMapMovesCursor Config.addNextParamKeys (E.Doc ["Edit", "Add parameter"]) .
      toEventMapAction $ Sugar.dAddFirstParam content
    toEventMapAction =
      fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid)
    myId = WidgetIds.fromGuid guid

make
  :: MonadA m
  => Sugar.Definition Sugar.Name m
  -> ExprGuiM m (WidgetT m)
make def =
  case def ^. Sugar.drBody of
  Sugar.DefinitionBodyExpression bodyExpr ->
    makeExprDefinition def bodyExpr
  Sugar.DefinitionBodyBuiltin builtin ->
    makeBuiltinDefinition def builtin

makeBuiltinDefinition
  :: MonadA m
  => Sugar.Definition Sugar.Name m
  -> Sugar.DefinitionBuiltin m
  -> ExprGuiM m (WidgetT m)
makeBuiltinDefinition def builtin =
  Box.vboxAlign 0 <$> sequenceA
    [ fmap BWidgets.hboxCenteredSpaced $ sequenceA
      [ ExprGuiM.withFgColor Config.builtinOriginNameColor $
        makeNameEdit name (Widget.joinId myId ["name"]) guid
      , makeEquals myId
      , BuiltinEdit.make builtin myId
      ]
    , fmap (defTypeScale . Lens.view ExpressionGui.egWidget) $
      ExprGuiM.makeSubexpresion typ
    ]
  where
    Sugar.Definition guid name typ _ = def
    myId = WidgetIds.fromGuid guid

defTypeScale :: Widget f -> Widget f
defTypeScale = Widget.scale Config.defTypeBoxSizeFactor

makeWhereItemEdit :: MonadA m => Sugar.WhereItem Sugar.Name m -> ExprGuiM m (WidgetT m)
makeWhereItemEdit item =
  fmap (Widget.weakerEvents eventMap) . assignCursor $
  makeDefContentEdit (Sugar.wiGuid item) (Sugar.wiName item) (Sugar.wiValue item)
  where
    assignCursor =
      foldr ((.) . (`ExprGuiM.assignCursor` myId) . WidgetIds.fromGuid) id $
      Sugar.wiHiddenGuids item
    myId = WidgetIds.fromGuid $ Sugar.wiGuid item
    eventMap
      | Just wiActions <- Sugar.wiActions item =
      mconcat
      [ Widget.keysEventMapMovesCursor Config.delKeys
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
  MonadA m => Guid -> Sugar.Name -> Sugar.DefinitionContent Sugar.Name m -> ExprGuiM m (WidgetT m)
makeDefContentEdit guid name content = do
  (body, wheres) <-
    makeParts name guid content
    & Lens.mapped . Lens._1 %~
      (^. ExpressionGui.egWidget) . ExpressionGui.hboxSpaced
  return . Box.vboxAlign 0 $ body : wheres

makeExprDefinition ::
  MonadA m => Sugar.Definition Sugar.Name m ->
  Sugar.DefinitionExpression Sugar.Name m ->
  ExprGuiM m (WidgetT m)
makeExprDefinition def bodyExpr = do
  typeWidgets <-
    case bodyExpr ^. Sugar.deMNewType of
    Nothing
      | bodyExpr ^. Sugar.deIsTypeRedundant -> return []
      | otherwise -> fmap ((:[]) . defTypeScale . BWidgets.hboxSpaced) (mkAcceptedRow id)
    Just (Sugar.DefinitionNewType inferredType acceptInferredType) ->
      fmap ((:[]) . defTypeScale . BWidgets.gridHSpaced) $ sequenceA
      [ mkAcceptedRow (>>= addAcceptanceArrow acceptInferredType)
      , mkTypeRow id "Inferred type:" inferredType
      ]
  bodyWidget <-
    makeDefContentEdit guid name $ bodyExpr ^. Sugar.deContent
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
    mkAcceptedRow onLabel = mkTypeRow onLabel "Type:" typ
    Sugar.Definition guid name typ _ = def
    myId = WidgetIds.fromGuid guid
    labelStyle =
      ExprGuiM.atEnv $ WE.setTextSizeColor Config.defTypeLabelTextSize Config.defTypeLabelColor

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit =
  -- If we delegate too deep (e.g: No polymorphic params) that's
  -- handled OK. So we may as well assume we're always wrapped by a
  -- polymorphic wrapper:
  FocusDelegator.delegatingId . -- Collapsed wrapper
  FocusDelegator.delegatingId -- Name editor
