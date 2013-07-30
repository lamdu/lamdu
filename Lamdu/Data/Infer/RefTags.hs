{-# LANGUAGE EmptyDataDecls #-}
module Lamdu.Data.Infer.RefTags
  ( TagExpr, ExprRef
  , TagRule, RuleRef
  ) where

import qualified Data.OpaqueRef as OR

data TagExpr def
data TagRule def

type ExprRef def = OR.Ref (TagExpr def)
type RuleRef def = OR.Ref (TagRule def)
