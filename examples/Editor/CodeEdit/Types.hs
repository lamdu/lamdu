{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Types(
  ExpressionAncestry(..),
  ArgumentData(..),
  FuncType(..),
  isArgument, addParens,
  varId, diveIn, isOperatorName)
where

import Data.ByteString.Char8 (pack)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

data FuncType = Infix | Prefix
  deriving (Eq, Ord, Show, Read)

data ArgumentData m = ArgumentData {
  _adFuncType :: FuncType,
  adParentPtr :: Transaction.Property ViewTag m (IRef Data.Expression)
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
diveIn = fmap $ WidgetIds.delegating . WidgetIds.fromIRef

isOperatorName :: String -> Bool
isOperatorName = all (not . Char.isAlphaNum)
