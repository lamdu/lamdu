module Lamdu.Data.Infer.Trigger.Internal
  ( Trigger(..)
  ) where

import Lamdu.Data.Infer.RefTags (ParamRef)

-- Triggers are alive as long as their truthfulness is yet
-- unknown. Once they're known to be false, they're removed. Once
-- they're known to be true, they trigger a rule and are removed.
data Trigger def
  = IsDirectlyTag
  | IsRecordType
  -- IsParameterRef may remain "unknown" even though scope makes it
  -- known that it isn't a parameter ref:
  | IsParameterRef (ParamRef def)
  -- ScopeHasParameterRef only triggers "no"
  | ScopeHasParameterRef (ParamRef def)
  deriving (Eq, Ord)

-- TODO: The above IsParameterRef/ScopeHasParameterRef need to be a
-- single trigger with 3 options.  The current 2 yes/no rules are a
-- messy way to encode the same
