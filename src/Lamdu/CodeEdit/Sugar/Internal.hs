module Lamdu.CodeEdit.Sugar.Internal
  ( BodyU, ExpressionU
  ) where

import Lamdu.CodeEdit.Sugar.Types

type BodyU m a = Body MStoredName m (ExpressionU m a)
type ExpressionU m a = Expression MStoredName m a
