{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.Data.Infer.TypedValue
  ( TypedValue(..), tvVal, tvType
  ) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Typeable (Typeable)
import Lamdu.Data.Infer.RefTags (ExprRef)
import qualified Control.Lens as Lens

-- TypedValue:
data TypedValue def = TypedValue
  { _tvVal :: {-# UNPACK #-}! (ExprRef def)
  , _tvType :: {-# UNPACK #-}! (ExprRef def)
  } deriving (Eq, Ord, Typeable)
Lens.makeLenses ''TypedValue
instance Show (TypedValue def) where
  showsPrec n (TypedValue v t) =
    showParen (n > 0) (unwords [show v, ":", show t] ++)

derive makeBinary ''TypedValue
