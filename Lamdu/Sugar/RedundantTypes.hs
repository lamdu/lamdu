{-# LANGUAGE RankNTypes #-}

module Lamdu.Sugar.RedundantTypes
  ( redundantTypes
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (Traversal')
import Control.Lens.Operators
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens

redundantTypesDefaultTop :: Bool -> Traversal' (Expression name m a) (Payload m a)
redundantTypesDefaultTop topRedundant f (Expression body pl) =
  case body of
  BodyGetVar{} -> redundant
  BodyRecord{} -> redundant
  BodyList{} -> redundantChildren
  BodyApply (Apply func specialArgs annotatedArgs) ->
    ( Apply
      <$> ( func & redundantTypesDefaultTop True %%~ f )
      <*> ( specialArgs & Lens.traversed r )
      <*> ( annotatedArgs & Lens.traversed . Lens.traversed %%~ r )
    )
    <&> BodyApply & mk
  _ -> mk recBody
  where
    r = redundantTypes f
    mk newBody =
      Expression <$> newBody <*> (if topRedundant then f else pure) pl
    recBody = body & Lens.traversed r
    redundant = Expression <$> recBody <*> f pl
    redundantChildren =
      body & Lens.traversed . redundantTypesDefaultTop True %%~ f & mk

redundantTypes :: Traversal' (Expression name m a) (Payload m a)
redundantTypes = redundantTypesDefaultTop False
