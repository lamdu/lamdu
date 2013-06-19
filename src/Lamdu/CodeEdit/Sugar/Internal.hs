module Lamdu.CodeEdit.Sugar.Internal
  ( BodyU, ExpressionU
  ) where

import Lamdu.CodeEdit.Sugar.Types

type BodyU m = Body MStoredName m (ExpressionU m)
type ExpressionU m = Expression MStoredName m
