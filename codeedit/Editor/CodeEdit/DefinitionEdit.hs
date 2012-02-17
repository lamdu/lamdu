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
  Data.Definition params bodyIRef <- getP definitionRef
  nameEdit <-
    assignCursor myId nameEditAnimId .
    BWidgets.setTextColor Config.definitionColor $
    BWidgets.makeNameEdit Config.unnamedStr definitionI nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Widget.joinId myId ["equals"]
  expressionEdit <- ExpressionEdit.make [] definitionI bodyRef

  let
    replaceEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Replace" .
      ETypes.diveIn $ DataOps.replace bodyRef
    paramEventMap i paramI =
      Widget.actionEventMapMovesCursor Config.delKeys
      "Delete parameter" $ do
        DataOps.delParameter definitionRef paramI
        Data.Definition newParams _ <- Property.get definitionRef
        return $
          (myId : map WidgetIds.fromIRef newParams)
          !! min (1+i) (length newParams)
    makeParamEdit (i, paramI) =
      (liftM . Widget.weakerEvents) (paramEventMap i paramI) .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (BWidgets.setTextColor Config.parameterColor .
       BWidgets.makeNameEdit ("<unnamed param " ++ show i ++ ">") paramI) $
      WidgetIds.fromIRef paramI

  paramsEdits <- mapM makeParamEdit $ enumerate params

  let
    jumpToExpressionEventMap =
      Widget.actionEventMapMovesCursor Config.jumpToExpressionKeys
      "Jump to expression" . return $ WidgetIds.fromIRef bodyIRef
    lhs =
      Widget.strongerEvents jumpToExpressionEventMap .
      BWidgets.hboxSpaced $ nameEdit : paramsEdits

  return .
    Widget.weakerEvents eventMap . BWidgets.hboxSpaced $
    [lhs, equals, Widget.weakerEvents replaceEventMap expressionEdit]
  where
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add parameter" .
      liftM (WidgetIds.delegating . WidgetIds.fromIRef) $
      DataOps.addParameter definitionRef
    nameEditAnimId = Widget.joinId myId ["name"]
    myId = WidgetIds.fromIRef definitionI

