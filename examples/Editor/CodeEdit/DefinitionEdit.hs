{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make) where

import Control.Monad (liftM)
import Data.List.Utils(enumerate)
import Data.Store.IRef (IRef)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

make :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
make definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor myId nameEditAnimId $
    BWidgets.makeNameEdit "<unnamed>" definitionI nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Widget.joinId myId ["equals"]
  (expressionEdit, _) <- ExpressionEdit.make ETypes.Root definitionI bodyRef
  paramsEdits <- mapM makeParamEdit $ enumerate params

  let
    replaceEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Replace" .
      ETypes.diveIn $ DataOps.replace bodyRef
  return .
    Widget.weakerEvents eventMap . BWidgets.hboxSpaced $
    [nameEdit] ++ paramsEdits ++ [equals, Widget.weakerEvents replaceEventMap expressionEdit]
  where
    makeParamEdit (i, paramI) =
      (liftM . Widget.weakerEvents) (paramEventMap paramI) .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (BWidgets.setTextColor Config.parameterColor .
       BWidgets.makeNameEdit ("<unnamed param " ++ show i ++ ">") paramI) $
      WidgetIds.fromIRef paramI
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    paramEventMap paramI =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete Parameter" .
      (liftM . const) myId $
      DataOps.delParameter definitionRef paramI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add Parameter" .
      liftM (WidgetIds.delegating . WidgetIds.fromIRef) $
      DataOps.addParameter definitionRef
    nameEditAnimId = Widget.joinId myId ["name"]
    myId = WidgetIds.fromIRef definitionI

