{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.WhereEdit(make, makeWithBody) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, atTextSizeColor, assignCursor)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as Ancestry
import qualified Editor.CodeEdit.DefinitionEdit as DefinitionEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> Sugar.Where m
  -> Widget.Id -> TWidget ViewTag m
make makeExpressionEdit ancestry where_@(Sugar.Where items _) myId = do
    whereLabel <-
      atTextSizeColor Config.whereTextSize Config.whereColor $
      BWidgets.makeLabel "where" myId
    whereEdits <- makeWhereItemsGrid
    return . BWidgets.vbox $
      [ whereLabel
      , Widget.scale Config.whereScaleFactor whereEdits
      ]
  where
    makeWhereItemsGrid = liftM (Grid.toWidget . addJumps . Grid.makeKeyed) $ mapM makeWhereItemEdits items
    addJumps = (Grid.atGridContent . fmap . map) DefinitionEdit.addJumps
    makeWhereItemEdits item =
      (liftM . map . second) (Widget.weakerEvents (whereItemDeleteEventMap item)) $
      DefinitionEdit.makeParts makeExpressionEdit
      (witemAncestry item) (Sugar.wiParamI item) (Sugar.wiValuePtr item)
    makeAncestry role = Ancestry.AncestryItemWhere (Ancestry.WhereParent where_ role) : ancestry
    witemAncestry = makeAncestry . Ancestry.WhereDef . Sugar.wiParamI
    whereItemDeleteEventMap whereItem =
      Widget.actionEventMapMovesCursor Config.delKeys "Delete variable" .
      liftM WidgetIds.fromIRef $ do
        Property.set
          (Sugar.wiApplyPtr whereItem)
          (Sugar.wiLambdaBodyI whereItem)
        return $ Sugar.wiLambdaBodyI whereItem

makeWithBody
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> Sugar.Where m
  -> Widget.Id -> TWidget ViewTag m
makeWithBody makeExpressionEdit ancestry where_@(Sugar.Where _ bodyPtr) myId = do
  bodyI <- getP bodyPtr
  whereEdit <- make makeExpressionEdit ancestry where_ myId
  assignCursor myId (WidgetIds.fromIRef bodyI) $ do
    bodyEdit <- makeExpressionEdit bodyAncestry bodyPtr
    return . BWidgets.vbox $
      [ bodyEdit
      , whereEdit
      ]
  where
    makeAncestry role = Ancestry.AncestryItemWhere (Ancestry.WhereParent where_ role) : ancestry
    bodyAncestry = makeAncestry Ancestry.WhereBody
