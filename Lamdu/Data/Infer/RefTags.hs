{-# LANGUAGE EmptyDataDecls #-}
module Lamdu.Data.Infer.RefTags
  ( TagExpr, ExprRef
  , TagRule, RuleRef
  , TagParam, ParamRef
  ) where

import qualified Data.OpaqueRef as OR

data TagExpr def
data TagRule def
data TagParam def

type ExprRef def = OR.Ref (TagExpr def)
type RuleRef def = OR.Ref (TagRule def)
type ParamRef def = OR.Ref (TagParam def)
