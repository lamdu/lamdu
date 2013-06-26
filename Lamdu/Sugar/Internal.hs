module Lamdu.Sugar.Internal
  ( BodyU, ExpressionU
  ) where

import Lamdu.Sugar.Types

type BodyU m a = Body MStoredName m (ExpressionU m a)
type ExpressionU m a = Expression MStoredName m a
