{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.WhereEdit(make) where

import Control.Monad (liftM)
import Data.Store.IRef (IRef)
import Data.Store.Property(Property)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, getP, atTextSizeColor, assignCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget(Widget)
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ParamEdit as ParamEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

handleFold
  :: MonadF m
  => Bool
  -> Property (Transaction ViewTag m) Bool
  -> TWidget ViewTag m
  -> IRef a
  -> Widget.Id
  -> CTransaction ViewTag m
     (Widget (Transaction ViewTag m),
      Widget.EventHandlers (Transaction ViewTag m),
      Widget.EventHandlers (Transaction ViewTag m))
handleFold False foldedRef makeWheres bodyI _ = do
  wheres <- makeWheres
  let
    eventMapOnBody =
      Widget.actionEventMap Config.foldKeys "Fold where" $
      Property.set foldedRef True
    eventMapOnItems =
      Widget.actionEventMapMovesCursor Config.foldKeys "Fold where" $ do
        Property.set foldedRef True
        return $ WidgetIds.fromIRef bodyI
  return (wheres, eventMapOnBody, eventMapOnItems)

handleFold True foldedRef _ _ myId = do
  whereEdits <- BWidgets.makeLabel "[...]" myId
  let
    eventMap =
      Widget.actionEventMap Config.unfoldKeys "Unfold where" $
      Property.set foldedRef False
  return (whereEdits, eventMap, eventMap)

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> Sugar.Where m
  -> Widget.Id -> TWidget ViewTag m
make makeExpressionEdit ancestry w@(Sugar.Where items bodyPtr) myId = do
  bodyI <- getP bodyPtr
  let foldedRef = Anchors.aDataRef "folded" False bodyI -- TODO: bodyI is a bad place to attach this!
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    whereLabel <-
      atTextSizeColor Config.whereTextSize Config.whereColor $
      BWidgets.makeLabel "where" myId
    bodyEdit <- makeExpressionEdit bodyAncestry bodyPtr
    folded <- getP foldedRef
    (whereEdits, eventMapOnBody, eventMapOnItems) <-
      handleFold folded foldedRef makeWhereItemsGrid bodyI myId
    return .
      BWidgets.vbox $ Widget.weakerEvents eventMapOnBody bodyEdit :
        (map . Widget.weakerEvents) eventMapOnItems
        [ whereLabel
        , whereEdits
        ]
  where
    makeWhereItemsGrid = liftM (Grid.toWidget . Grid.make) $ mapM makeWhereItemEdits items
    makeAncestry role = ETypes.AncestryItemWhere (ETypes.WhereParent w role) : ancestry
    bodyAncestry = makeAncestry ETypes.WhereBody
    witemAncestry = makeAncestry . ETypes.WhereDef
    makeWhereItemEdits item =
      (mapM . liftM . Widget.weakerEvents) (whereItemDeleteEventMap item)
      [ (liftM . Widget.align) (Vector2 1 0.5) . ParamEdit.make $ Sugar.wiParamI item
      , (liftM . Widget.align) (Vector2 0.5 0.5) . BWidgets.makeLabel "=" . WidgetIds.fromIRef $ Sugar.wiParamI item
      , (liftM . Widget.align) (Vector2 0 0.5) . makeExpressionEdit (witemAncestry (Sugar.wiParamI item)) $ Sugar.wiExprPtr item
      ]
    whereItemDeleteEventMap item =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete variable" .
      liftM WidgetIds.fromIRef $ Sugar.wiRemoveItem item
