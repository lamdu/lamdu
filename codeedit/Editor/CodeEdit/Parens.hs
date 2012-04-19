{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.Parens(addParens, makeParensId)
where

import Data.ByteString.Char8 (pack)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, WidgetT)
import Editor.MonadF (MonadF)
import Editor.WidgetIds (parensPrefix)
import qualified Data.Store.Property as Property
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as A
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

addParens
  :: MonadF m
  => (TWidget t m -> TWidget t m)
  -> (TWidget t m -> TWidget t m)
  -> Widget.Id
  -> WidgetT t m
  -> TWidget t m
addParens onLParen onRParen parenId widget = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  return $ BWidgets.hbox [ beforeParen, widget, afterParen ]
  where
    label str = BWidgets.makeLabel str $ parensPrefix parenId

makeParensId :: Monad m => A.ExpressionAncestry m -> Transaction ViewTag m Widget.Id
makeParensId (A.AncestryItemApply (A.ApplyParent role _ _ parentPtr) : _) = do
  parentI <- Property.get parentPtr
  return $
    Widget.joinId (WidgetIds.fromIRef parentI)
    [pack $ show role]
makeParensId (A.AncestryItemLambda (A.LambdaParent _ parentI) : _) =
  return $ WidgetIds.fromIRef parentI
makeParensId (A.AncestryItemWhere (A.WhereParent (Sugar.Where _ body) role) : _) = do
  bodyI <- Property.get body
  return $
    Widget.joinId (WidgetIds.fromIRef bodyI)
    [pack $ show role]
makeParensId (A.AncestryItemParamType (A.ParamTypeParent parentI) : _) =
  return $ WidgetIds.fromIRef parentI
makeParensId [] =
  return $ Widget.Id ["root parens"]
