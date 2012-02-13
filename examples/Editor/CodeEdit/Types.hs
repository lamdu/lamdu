{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Types(
  ExpressionPtr,
  ExpressionAncestry(..),
  ArgumentData(..),
  FuncType(..),
  isArgument, addParens,
  varId, diveIn, isOperatorName,
  isInfixVar, isInfixFunc, isApplyOfInfixOp,
  makeAddNextArgEventMap)
where

import Control.Monad (liftM)
import Data.ByteString.Char8 (pack)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

type ExpressionPtr m = Transaction.Property ViewTag m (IRef Data.Expression)

data FuncType = Infix | Prefix
  deriving (Eq, Ord, Show, Read)

data ArgumentData m = ArgumentData {
  adFuncType :: FuncType,
  adParentPtr :: Transaction.Property ViewTag m (IRef Data.Expression),
  adApply :: Data.Apply
  }

data ExpressionAncestry m =
    Argument (ArgumentData m)
  | NotArgument
  | Root

isArgument :: ExpressionAncestry m -> Bool
isArgument (Argument _) = True
isArgument _ = False

addParens :: MonadF m => Widget.Id -> Widget (Transaction t m) -> TWidget t m
addParens parenId widget = do
  beforeParen <- label "("
  afterParen <- label ")"
  return $ BWidgets.hbox [ beforeParen, widget, afterParen ]
  where
    label str = BWidgets.makeTextView str $ Widget.joinId parenId [pack str]

varId :: Data.VariableRef -> Widget.Id
varId = Data.onVariableIRef WidgetIds.fromIRef

diveIn :: Functor f => f (IRef a) -> f Widget.Id
diveIn = fmap WidgetIds.fromIRef

isOperatorName :: String -> Bool
isOperatorName = all (not . Char.isAlphaNum)

isInfixVar :: Monad m => Data.VariableRef -> CTransaction t m Bool
isInfixVar = liftM isOperatorName . getP . Anchors.variableNameRef

isInfixFunc :: Monad m => IRef Data.Expression -> CTransaction t m Bool
isInfixFunc funcI = do
  expr <- getP $ Transaction.fromIRef funcI
  case expr of
    Data.ExpressionGetVariable var -> isInfixVar var
    _ -> return False

isApplyOfInfixOp :: Monad m => IRef Data.Expression -> CTransaction t m Bool
isApplyOfInfixOp exprI = do
  expr <- getP $ Transaction.fromIRef exprI
  case expr of
    Data.ExpressionApply (Data.Apply funcI _) -> isInfixFunc funcI
    _ -> return False

makeAddNextArgEventMap :: MonadF m =>
  Transaction.Property t m (IRef Data.Expression) -> Widget.EventHandlers (Transaction t m)
makeAddNextArgEventMap =
  Widget.actionEventMapMovesCursor Config.addNextArgumentKeys "Add another argument" .
  diveIn . DataOps.callWithArg
