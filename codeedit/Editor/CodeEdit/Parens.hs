{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Parens(addTextParens, addParens, makeParensId)
where

import Control.Monad (void)
import Data.ByteString.Char8 (pack)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.Sugar (ParensType(..))
import Editor.CTransaction (TWidget, WidgetT, subCursor)
import Editor.MonadF (MonadF)
import Editor.WidgetIds (parensPrefix)
import qualified Data.Store.Property as Property
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as A
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

addTextParens
  :: MonadF m
  => (TWidget t m -> TWidget t m)
  -> (TWidget t m -> TWidget t m)
  -> Widget.Id
  -> WidgetT t m
  -> TWidget t m
addTextParens onLParen onRParen parenId widget = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  return $ BWidgets.hbox [ beforeParen, widget, afterParen ]
  where
    label str = BWidgets.makeLabel str $ parensPrefix parenId

squareDraw :: Vector2 Widget.R -> Draw.Image Draw.Any
squareDraw (Vector2 w h) = mconcat
  [ Draw.line (0, 0) (w, 0)
  , Draw.line (w, 0) (w, h)
  , Draw.line (w, h) (0, h)
  , Draw.line (0, h) (0, 0)
  ]

squareFrame :: Anim.AnimId -> Vector2 Widget.R -> Anim.Frame
squareFrame animId size =
  Anim.simpleFrameDownscale animId size . void $ squareDraw size

addSquareParens :: Widget.Id -> WidgetT t m -> WidgetT t m
addSquareParens parensId =
  Widget.atImageWithSize addSquareFrame .
  Widget.translateBy (* ((1 - Config.squareParensScaleFactor) / Config.squareParensScaleFactor / 2)) .
  Widget.atSizeDependentWidgetData (Widget.scaleSizeDependentWidgetData Config.squareParensScaleFactor)
  where
    addSquareFrame size = mappend $ squareFrame (Widget.toAnimId parensId) size

makeParensId :: Monad m => A.ExpressionAncestry m -> Transaction ViewTag m Widget.Id
makeParensId (A.AncestryItemApply (A.ApplyParent role _ _ parentPtr) : _) = do
  parentI <- Property.get parentPtr
  return $
    Widget.joinId (WidgetIds.fromIRef parentI)
    [pack $ show role]
makeParensId (A.AncestryItemLambda (A.LambdaParent _ parentI) : _) =
  return $ WidgetIds.fromIRef parentI
makeParensId (A.AncestryItemWhere (A.WhereParent (Sugar.Where _ body) role) : _) = do
  bodyI <- Property.get $ Sugar.rExpressionPtr body
  return $
    Widget.joinId (WidgetIds.fromIRef bodyI)
    [pack $ show role]
makeParensId (A.AncestryItemParamType (A.ParamTypeParent parentI) : _) =
  return $ WidgetIds.fromIRef parentI
makeParensId [] =
  return $ Widget.Id ["root parens"]

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor WidgetIds.parenHighlightId Config.parenHighlightColor

addParens
  :: (MonadF m)
  => ParensType
  -> Widget.Id
  -> WidgetT ViewTag m
  -> TWidget ViewTag m
addParens SquareParens myId widget = return $ addSquareParens myId widget
addParens TextParens myId widget = do
  mInsideParenId <- subCursor rParenId
  widgetWithParens <- addTextParens id doHighlight myId widget
  return $ maybe id (const highlightExpression) mInsideParenId widgetWithParens
  where
    rParenId = Widget.joinId myId [")"]
    doHighlight = (>>= BWidgets.makeFocusableView rParenId)
