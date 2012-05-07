{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.SectionEdit(make) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor)
import Editor.CodeEdit.Ancestry (AncestryItem(..), ApplyParent(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.DataOps (ExpressionPtr)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as Ancestry
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> ExpressionPtr m
  -> Sugar.Section m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr (Sugar.Section mLArg op mRArg) myId = do
  -- TODO: Remove this when obliterate ancestry
  cursorPosI <- getP . Sugar.rExpressionPtr . head $ catMaybes [mRArg, Just op, mLArg]
  assignCursor myId (WidgetIds.fromIRef cursorPosI) $ do
    let
      makeAncestry role =
        AncestryItemApply
        (ApplyParent role {-TODO: This is wrong until Ancestry dies-}Ancestry.InfixRight (error "TODO sldjgn") expressionPtr) :
        ancestry
      fromMArg role =
        maybe (return []) $ liftM (:[]) . makeExpressionEdit (makeAncestry role)
    lArgEdit <- fromMArg Ancestry.ApplyArg mLArg
    opEdit <- liftM (:[]) $ makeExpressionEdit (makeAncestry Ancestry.ApplyFunc) op
    rArgEdit <- fromMArg Ancestry.ApplyArg mRArg
    return . BWidgets.hboxSpaced $ lArgEdit ++ opEdit ++ rArgEdit
