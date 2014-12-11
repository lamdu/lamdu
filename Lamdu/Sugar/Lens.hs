{-# LANGUAGE FlexibleContexts #-}

module Lamdu.Sugar.Lens
  ( subExprPayloads, payloadsIndexedByPath
  , holePayloads, holeArgs
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens

subExprPayloads ::
  Lens.IndexedTraversal
  (ExpressionP name m ())
  (ExpressionP name m a)
  (ExpressionP name m b)
  a b
subExprPayloads f val@(Expression body pl) =
  Expression
  <$> (body & Lens.traversed .> subExprPayloads %%~ f)
  <*> Lens.indexed f (void val) pl

payloadsIndexedByPath ::
  Lens.IndexedTraversal
  [ExpressionP name m ()]
  (ExpressionP name m a)
  (ExpressionP name m b)
  a b
payloadsIndexedByPath f =
  go []
  where
    go path val@(Expression body pl) =
      Expression
      <$> Lens.traversed (go newPath) body
      <*> Lens.indexed f newPath pl
      where
        newPath = void val : path

holePayloads ::
  Lens.IndexedTraversal' (ExpressionP name m ()) (ExpressionP name m a) a
holePayloads =
  subExprPayloads . Lens.ifiltered predicate
  where
    predicate idx _ = Lens.has (rBody . _BodyHole) idx

holeArgs ::
  Lens.IndexedTraversal' [ExpressionP name m ()] (ExpressionP name m a) a
holeArgs =
  payloadsIndexedByPath . Lens.ifiltered predicate
  where
    predicate (_:parent:_) _ = Lens.has (rBody . _BodyHole) parent
    predicate _ _ = False
