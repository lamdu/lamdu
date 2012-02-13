{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit(makePanesEdit) where

import Control.Monad (liftM)
import Data.List.Utils(enumerate, removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor, transaction)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

makeDefinitionEdit :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
makeDefinitionEdit definitionI = do
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

newDefinition :: Monad m => Transaction.Transaction ViewTag m Widget.Id
newDefinition = do
  newDefI <- Anchors.makeDefinition
  Anchors.newPane newDefI
  return $ WidgetIds.fromIRef newDefI

makePanesEdit :: MonadF m => TWidget ViewTag m
makePanesEdit = do
  panes <- getP panesRef

  let
    delPane i = do
      let newPanes = removeAt i panes
      Property.set panesRef newPanes
      return . WidgetIds.fromIRef . Anchors.paneDefinition . last $
        take (i+1) newPanes

    paneEventMap (_:_:_) i =
      Widget.actionEventMapMovesCursor Config.closePaneKeys
        "Close Pane" $ delPane i
    paneEventMap _ _ = mempty

    makePaneWidget (i, pane) =
      (liftM . Widget.weakerEvents) (paneEventMap panes i) .
      makeDefinitionEdit $ Anchors.paneDefinition pane

  panesWidget <-
    case panes of
      [] -> BWidgets.makeFocusableTextView "<No panes>" myId
      (firstPane:_) ->
        assignCursor myId
          (WidgetIds.fromIRef (Anchors.paneDefinition firstPane)) $ do
            definitionEdits <- mapM makePaneWidget $ enumerate panes
            return $ BWidgets.vboxAlign 0 definitionEdits

  canJumpBack <- transaction $ Anchors.canJumpBack
  let
    panesEventMap =
      mconcat $ concat
      [ [ Widget.actionEventMapMovesCursor Config.newDefinitionKeys
          "New Definition" newDefinition
        ]
      , [ Widget.actionEventMapMovesCursor Config.previousCursor
          "Go to previous position" $ liftM (fromMaybe myId) Anchors.jumpBack
        | canJumpBack
        ]
      ]

  return $ Widget.weakerEvents panesEventMap panesWidget
  where
    panesRef = Transaction.fromIRef Anchors.rootIRef
    myId = WidgetIds.fromIRef Anchors.rootIRef
