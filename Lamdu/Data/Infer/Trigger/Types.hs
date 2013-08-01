module Lamdu.Data.Infer.Trigger.Types
  ( Trigger(..)
  , Fired(..)
  ) where

import Lamdu.Data.Infer.RefTags (ParamRef)

-- Triggers are alive as long as their truthfulness is yet
-- unknown. Once they're known to be false, they're removed. Once
-- they're known to be true, they trigger a rule and are removed.
data Trigger def
  = OnDirectlyTag
  | OnRecordType
  -- IsParameterRef may remain "unknown" even though scope makes it
  -- known that it isn't a parameter ref:
  | OnParameterRef (ParamRef def)
  -- ScopeHasParameterRef only triggers "no"
  | OnScopeHasParameterRef (ParamRef def)
  deriving (Eq, Ord, Show)

-- TODO: The above IsParameterRef/ScopeHasParameterRef need to be a
-- single trigger with 3 options.  The current 2 yes/no rules are a
-- messy way to encode the same

data Fired def
  = FiredDirectlyTag Bool
  | FiredRecordType Bool
  | FiredParameterRef (ParamRef def) Bool
  | FiredScopeHasParameterRef (ParamRef def) Bool
  deriving (Eq, Ord, Show)
