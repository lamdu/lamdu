module Lamdu.Data.Infer.Trigger.Types
  ( Trigger(..)
  , ParameterRefEvent(..)
  , Fired(..)
  ) where

import Lamdu.Data.Infer.RefTags (ParamRef, ExprRef)

-- Triggers are alive as long as their truthfulness is yet
-- unknown. When they become known, they fire (see Fired) below and
-- disappear.
data Trigger def
  = OnDirectlyTag
  | OnRecordType
  | OnGetDef
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
  | FiredRecordType Bool
  | FiredGetDef Bool
  | FiredParameterRef (ParamRef def) ParameterRefEvent
  | FiredUnify (ExprRef def)
  deriving (Eq, Ord, Show)
