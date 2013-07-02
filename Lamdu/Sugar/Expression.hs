{-# LANGUAGE FlexibleContexts #-}
module Lamdu.Sugar.Expression
  ( subExpressions, bodyHole
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens

-- Parent payloads come after children. Children are in Body-Foldable
-- order
subExpressions ::
  Lens.IndexedTraversal
  (ExpressionP name m ())
  (ExpressionP name m a) (ExpressionP name m b) a b
subExpressions f expr =
  Expression <$>
  (Lens.traversed .> subExpressions) f (expr ^. rBody) <*>
  Lens.indexed f
  -- Remove annotations from expr so it is a legal traversal (index
  -- mustn't overlap with what's being traversed)
  (void expr)
  (expr ^. rPayload)

-- Affine traversal:
bodyHole :: Lens.Traversal' (Body name m expr) (Hole name m expr)
bodyHole f (BodyHole hole) = BodyHole <$> f hole
bodyHole f (BodyInferred inferred) = BodyInferred <$> iHole f inferred
bodyHole _ body = pure body
