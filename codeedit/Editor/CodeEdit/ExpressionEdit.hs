{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first, second)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, transaction)
import Editor.MonadF (MonadF)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data HoleResultPicker m = NotAHole | IsAHole (Maybe (HoleEdit.ResultPicker m))

make
  :: MonadF m
  => ETypes.ExpressionAncestry m -> IRef Data.Definition
  -> ETypes.ExpressionPtr m -> TWidget ViewTag m
make ancestry definitionI expressionPtr = do
  expressionI <- getP expressionPtr
  expr <- getP $ Transaction.fromIRef expressionI
  let
    notAHole = (fmap . liftM) ((,) NotAHole)
    wrapNonHole keys isDelegating f =
      notAHole . BWidgets.wrapDelegatedWithKeys keys isDelegating f
    makeExpression = (`make` definitionI)
    makeEditor =
      case expr of
        Data.ExpressionHole holeState ->
          (fmap . liftM . first) IsAHole .
          BWidgets.wrapDelegatedWithKeys
            FocusDelegator.defaultKeys FocusDelegator.Delegating second $
            HoleEdit.make ancestry definitionI holeState expressionPtr
        Data.ExpressionGetVariable varRef ->
          notAHole $ VarEdit.make ancestry varRef
        Data.ExpressionLambda lambda ->
          wrapNonHole Config.exprFocusDelegatorKeys
            FocusDelegator.Delegating id $
          LambdaEdit.make makeExpression ancestry expressionPtr lambda
        Data.ExpressionApply apply ->
          wrapNonHole Config.exprFocusDelegatorKeys
            FocusDelegator.Delegating id $
          ApplyEdit.make makeExpression ancestry expressionPtr apply
        Data.ExpressionLiteralInteger integer ->
          wrapNonHole FocusDelegator.defaultKeys
            FocusDelegator.NotDelegating id $
          LiteralEdit.makeInt expressionI integer
  let expressionId = WidgetIds.fromIRef expressionI
  (holePicker, widget) <- makeEditor expressionId
  (addArgDoc, addArgHandler) <-
    transaction $ ETypes.makeAddArgHandler ancestry expressionPtr
  let
    eventMap = mconcat
      [ moveUnlessOnHole .
        Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument" .
        ETypes.diveIn $ DataOps.giveAsArg expressionPtr
      , moveUnlessOnHole .
        Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" .
        ETypes.diveIn $ DataOps.callWithArg expressionPtr
      , pickResultFirst $
        Widget.actionEventMapMovesCursor
        Config.addNextArgumentKeys addArgDoc addArgHandler
      , ifHole (const mempty) $
        Widget.actionEventMapMovesCursor
        relinkKeys "Replace" . ETypes.diveIn $
        DataOps.replace expressionPtr
      , Widget.actionEventMapMovesCursor
        Config.lambdaWrapKeys "Lambda wrap" . ETypes.diveIn $
        DataOps.lambdaWrap expressionPtr
      ]
    relinkKeys
      | null ancestry = Config.relinkKeys ++ Config.delKeys
      | otherwise = Config.relinkKeys
    ifHole f =
      case holePicker of
        NotAHole -> id
        IsAHole x -> f x
    moveUnlessOnHole = ifHole $ (const . fmap . liftM . Widget.atECursor . const) Nothing
    pickResultFirst = ifHole (maybe id (fmap . joinEvents))
    joinEvents x y = do
      r <- liftM Widget.eAnimIdMapping x
      (liftM . Widget.atEAnimIdMapping) (. r) y
  return $ Widget.weakerEvents eventMap widget
