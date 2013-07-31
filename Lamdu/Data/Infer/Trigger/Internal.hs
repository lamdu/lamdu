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
  | IsParameterRef (ParamRef def)
  deriving (Eq, Ord)
