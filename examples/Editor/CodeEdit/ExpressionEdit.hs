{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.CodeEdit.VarView as VarView
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

make :: MonadF m =>
  ETypes.ExpressionAncestry m -> IRef Data.Definition ->
  ETypes.ExpressionPtr m ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
make ancestry definitionI expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    mkCallWithArg = ETypes.diveIn $ DataOps.callWithArg expressionPtr
    mkGiveAsArg = ETypes.diveIn $ DataOps.giveAsArg expressionPtr
    expressionId = WidgetIds.fromIRef expressionI

    wrap keys entryState f =
      BWidgets.wrapDelegatedWithKeys keys entryState first f expressionId

    eventMap = mconcat $
      [ ETypes.makeAddNextArgEventMap expressionPtr | not $ ETypes.isArgument ancestry ] ++
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument"
        mkGiveAsArg
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" mkCallWithArg
      , Widget.actionEventMapMovesCursor
        Config.relinkKeys "Replace" . ETypes.diveIn $ DataOps.replace expressionPtr
      ]

  expr <- getP expressionRef
  (needParen, (widget, parenId)) <-
    case expr of
      Data.ExpressionHole holeState ->
        liftM ((,) False) .
        wrap FocusDelegator.defaultKeys FocusDelegator.NotDelegating .
          (fmap . liftM) (flip (,) expressionId) $
          HoleEdit.make ancestry definitionI holeState expressionPtr
      Data.ExpressionGetVariable varRef -> do
        varRefView <- VarView.make varRef expressionId
        isInfix <- ETypes.isInfixVar varRef
        let
          jumpToDefinitionEventMap =
            Widget.actionEventMapMovesCursor Config.jumpToDefinitionKeys "Jump to definition" jumpToDefinition
          jumpToDefinition =
            case varRef of
              Data.DefinitionRef defI -> Anchors.newPane defI
              Data.ParameterRef paramI -> return $ WidgetIds.fromIRef paramI
              Data.BuiltinRef _builtI -> return expressionId
          needParen =
            case ancestry of
              ETypes.NotArgument -> False
              _ -> isInfix
        return
          (needParen,
           (Widget.weakerEvents jumpToDefinitionEventMap varRefView,
            expressionId))
      Data.ExpressionApply apply@(Data.Apply funcI _) -> do
        isFullOp <- ETypes.isApplyOfInfixOp funcI
        isInfix <- ETypes.isInfixFunc funcI
        result <-
          wrap Config.exprFocusDelegatorKeys FocusDelegator.Delegating $
          ApplyEdit.make (flip make definitionI) expressionPtr apply
        let
          needParen =
            case ancestry of
              ETypes.Root -> isInfix
              ETypes.Argument _ -> True
              ETypes.NotArgument -> isFullOp
        return (needParen, result)

  (resultWidget, resultParenId) <-
    if needParen then do
      resWidget <- ETypes.addParens parenId widget
      return (resWidget, expressionId)
    else
      return (widget, parenId)
  return (Widget.weakerEvents eventMap resultWidget, resultParenId)
