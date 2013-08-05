module Lamdu.Data.Infer.Trigger.Types
  ( Trigger(..)
  , ParameterRefEvent(..)
  , Fired(..)
  ) where

import Lamdu.Data.Expression.Utils () -- Show instance for Expr.Body
import Lamdu.Data.Infer.RefTags (ParamRef, ExprRef)
import qualified Lamdu.Data.Expression as Expr

-- Triggers are alive as long as their truthfulness is yet
-- unknown. When they become known, they fire (see Fired) below and
-- disappear.
data Trigger def
  = OnDirectlyTag
  | OnKnownBody
  | OnParameterRef (ParamRef def)
  | OnUnify
  deriving (Eq, Ord, Show)

data ParameterRefEvent
  = ParameterRefOutOfScope
  | NotParameterRef
  | IsParameterRef
  deriving (Eq, Ord, Show)

data Fired def
  = FiredDirectlyTag Bool
  | FiredKnownBody (Expr.Body () (ExprRef def))
  | FiredParameterRef (ParamRef def) ParameterRefEvent
  | FiredUnify (ExprRef def)
  deriving (Show)
