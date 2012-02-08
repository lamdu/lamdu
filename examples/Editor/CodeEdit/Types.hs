{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.Types(
  ExpressionAncestry(..),
  ArgumentData(..),
  FuncType(..),
  isArgument)
where

import Data.Store.IRef (IRef)
import Editor.Anchors (ViewTag)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data

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
