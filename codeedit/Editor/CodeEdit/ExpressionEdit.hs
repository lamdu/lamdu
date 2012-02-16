{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Data.Monoid(Monoid(..))
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
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

make
  :: MonadF m
  => ETypes.ExpressionAncestry m -> IRef Data.Definition
  -> ETypes.ExpressionPtr m -> TWidget ViewTag m
make ancestry definitionI expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionId = WidgetIds.fromIRef expressionI

  expr <- getP $ Transaction.fromIRef expressionI
  (addArgDoc, addArgHandler) <-
    transaction $ ETypes.makeAddArgHandler ancestry expressionPtr
  let
    makeEditor =
      case expr of
        Data.ExpressionHole holeState ->
          BWidgets.wrapDelegatedWithKeys
            FocusDelegator.defaultKeys FocusDelegator.Delegating id $
          HoleEdit.make ancestry definitionI holeState expressionPtr
        Data.ExpressionGetVariable varRef ->
          VarEdit.make ancestry varRef
        Data.ExpressionApply apply ->
          BWidgets.wrapDelegatedWithKeys
            Config.exprFocusDelegatorKeys FocusDelegator.Delegating id $
          ApplyEdit.make (`make` definitionI) ancestry expressionPtr apply
        Data.ExpressionLiteralInteger integer ->
          BWidgets.wrapDelegatedWithKeys
            FocusDelegator.defaultKeys FocusDelegator.NotDelegating id $
          LiteralEdit.makeInt expressionI integer
  widget <- makeEditor expressionId
  let
    eventMap = mconcat
      [ Widget.actionEventMapMovesCursor
        Config.addNextArgumentKeys addArgDoc addArgHandler
      , Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument" .
        ETypes.diveIn $ DataOps.giveAsArg expressionPtr
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" . ETypes.diveIn $ DataOps.callWithArg expressionPtr
      , Widget.actionEventMapMovesCursor
        Config.relinkKeys "Replace" . ETypes.diveIn $ DataOps.replace expressionPtr
      ]
  return $ Widget.weakerEvents eventMap widget
