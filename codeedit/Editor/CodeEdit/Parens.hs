{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Parens(addTextParens, addParens, makeParensId)
where

import Control.Monad (liftM, liftM2)
import Data.ByteString.Char8 (pack)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, WidgetT, transaction, subCursor)
import Editor.MonadF (MonadF)
import Editor.WidgetIds (parensPrefix)
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as A
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

data HighlightParens = DoHighlightParens | DontHighlightParens

makeCondParensId
  :: Monad m
  => Bool -> A.ExpressionAncestry m
  -> Transaction ViewTag m (Maybe Widget.Id)
makeCondParensId False = const $ return Nothing
makeCondParensId True = liftM Just . makeParensId

setDoHighlight
  :: (Monad m, Functor f)
  => m (f a) -> m (f (HighlightParens, a))
setDoHighlight = (liftM . fmap) ((,) DoHighlightParens)

setDontHighlight
  :: (Monad m, Functor f)
  => m (f a) -> m (f (HighlightParens, a))
setDontHighlight = (liftM . fmap) ((,) DontHighlightParens)

getParensInfo
  :: Monad m
  => Sugar.Expression m -> A.ExpressionAncestry m
  -> Transaction ViewTag m (Maybe (HighlightParens, Widget.Id))
getParensInfo (Sugar.ExpressionApply _) ancestry@(A.AncestryItemApply (A.ApplyParent A.ApplyArg A.Prefix _ _) : _) =
  setDoHighlight . liftM Just $ makeParensId ancestry
getParensInfo (Sugar.ExpressionApply (Data.Apply funcI _)) ancestry@(A.AncestryItemApply (A.ApplyParent A.ApplyArg _ _ _) : _) = do
  isInfix <-
    liftM2 (||)
    (Infix.isInfixFunc funcI) (Infix.isApplyOfInfixOp funcI)
  setDoHighlight $ makeCondParensId isInfix ancestry
getParensInfo (Sugar.ExpressionApply (Data.Apply funcI _)) ancestry@(A.AncestryItemApply (A.ApplyParent A.ApplyFunc _ _ _) : _) = do
  isInfix <- Infix.isApplyOfInfixOp funcI
  setDoHighlight $ makeCondParensId isInfix ancestry
getParensInfo (Sugar.ExpressionApply (Data.Apply funcI _)) ancestry = do
  isInfix <- Infix.isInfixFunc funcI
  setDoHighlight $ makeCondParensId isInfix ancestry
getParensInfo (Sugar.ExpressionGetVariable _) (A.AncestryItemApply (A.ApplyParent A.ApplyFunc _ _ _) : _) =
  return Nothing
getParensInfo (Sugar.ExpressionGetVariable var) ancestry = do
  name <- Property.get $ Anchors.variableNameRef var
  setDontHighlight $ makeCondParensId (Infix.isInfixName name) ancestry
getParensInfo (Sugar.ExpressionWhere _) ancestry =
  if A.isAncestryRHS ancestry
  then return Nothing
  else setDontHighlight . liftM Just $ makeParensId ancestry
getParensInfo (Sugar.ExpressionFunc _) ancestry@(A.AncestryItemApply _ : _) =
  setDoHighlight . liftM Just $ makeParensId ancestry
getParensInfo _ _ =
  return Nothing

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

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor WidgetIds.parenHighlightId Config.parenHighlightColor

addParens
  :: (MonadF m)
  => Widget.Id
  -> Sugar.Expression m
  -> A.ExpressionAncestry m
  -> WidgetT ViewTag m
  -> TWidget ViewTag m
addParens myId sExpr ancestry widget = do
  mInfo <- transaction $ getParensInfo sExpr ancestry
  case mInfo of
    Nothing -> return widget
    Just (needHighlight, parensId) -> do
      mInsideParenId <- subCursor rParenId
      widgetWithParens <- addTextParens id doHighlight parensId widget
      return $ maybe id (const highlightExpression) mInsideParenId widgetWithParens
      where
        rParenId = Widget.joinId myId [")"]
        doHighlight =
          case needHighlight of
            DoHighlightParens -> (>>= BWidgets.makeFocusableView rParenId)
            DontHighlightParens -> id
