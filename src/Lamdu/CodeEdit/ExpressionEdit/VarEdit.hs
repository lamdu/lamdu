{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit.VarEdit(make, makeView) where

import Control.MonadA (MonadA)
import Data.Store.IRef (Tag)
import Lamdu.Anchors (ViewM)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Config as Config
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

colorOf :: Data.VariableRef def -> Draw.Color
colorOf (Data.DefinitionRef _) = Config.definitionColor
colorOf (Data.ParameterRef _) = Config.parameterColor

-- Color should be determined on the outside!
makeView
  :: MonadA m
  => Data.VariableRef (DataIRef.DefI (Tag m))
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeView var myId = ExprGuiM.withNameFromVarRef var $ \(nameSrc, name) ->
  fmap
  (ExpressionGui.fromValueWidget .
   ExpressionGui.nameSrcTint nameSrc) .
  ExprGuiM.widgetEnv $
  BWidgets.makeFocusableTextView name myId

make
  :: m ~ ViewM
  => Data.VariableRef (DataIRef.DefI (Tag m))
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make getVar myId = do
  case getVar of
    Data.ParameterRef guid -> ExprGuiM.markVariablesAsUsed [guid]
    _ -> return ()
  getVarView <-
    ExprGuiM.atEnv (WE.setTextColor (colorOf getVar)) $
    makeView getVar myId
  let
    jumpToDefinitionEventMap =
      Widget.keysEventMapMovesCursor Config.jumpToDefinitionKeys (E.Doc ["Navigation", "Jump to definition"])
      jumpToDefinition
    jumpToDefinition =
      case getVar of
        Data.DefinitionRef defI -> do
          DataOps.newPane defI
          DataOps.savePreJumpPosition myId
          return $ WidgetIds.fromIRef defI
        Data.ParameterRef paramGuid -> do
          DataOps.savePreJumpPosition myId
          return $ WidgetIds.fromGuid paramGuid
  return $
    Lens.over ExpressionGui.egWidget
    (Widget.weakerEvents jumpToDefinitionEventMap)
    getVarView
