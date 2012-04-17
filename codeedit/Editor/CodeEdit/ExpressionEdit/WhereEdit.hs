{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.WhereEdit(make) where

import Control.Monad (liftM)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, atTextSizeColor, assignCursor)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> Sugar.Where m
  -> Widget.Id -> TWidget ViewTag m
make makeExpressionEdit ancestry w@(Sugar.Where items bodyPtr) myId = do
  bodyI <- getP bodyPtr
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    whereLabel <-
      atTextSizeColor Config.whereTextSize Config.whereColor $
      BWidgets.makeLabel "where" myId
    bodyEdit <- makeExpressionEdit bodyAncestry bodyPtr
    whereEdits <- makeWhereItemsGrid
    return . BWidgets.vbox $
      [ bodyEdit
      , whereLabel
      , Widget.scale Config.whereScaleFactor whereEdits
      ]
  where
    makeWhereItemsGrid = liftM (Grid.toWidget . Grid.make) $ mapM makeWhereItemEdits items
    makeAncestry role = ETypes.AncestryItemWhere (ETypes.WhereParent w role) : ancestry
    bodyAncestry = makeAncestry ETypes.WhereBody
    witemAncestry = makeAncestry . ETypes.WhereDef . Sugar.wiParamI
    makeWhereItemEdits item =
      (liftM . map) (Widget.weakerEvents (whereItemDeleteEventMap item) . snd) $
      DefinitionEdit.makeParts makeExpressionEdit
      (witemAncestry item) (Sugar.wiParamI item) (Sugar.wiValuePtr item)
    whereItemDeleteEventMap whereItem =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete variable" .
      liftM WidgetIds.fromIRef $ do
        Property.set
          (Sugar.wiApplyPtr whereItem)
          (Sugar.wiLambdaBodyI whereItem)
        return $ Sugar.wiLambdaBodyI whereItem

