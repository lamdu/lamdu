{-# LANGUAGE EmptyDataDecls #-}
module Lamdu.Data.Infer.RefTags
  ( TagExpr
  , ExprRef
  ) where

import qualified Data.OpaqueRef as OR

data TagExpr def

type ExprRef def = OR.Ref (TagExpr def)
