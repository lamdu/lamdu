{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..), T, CT
  , InferContext(..), icContext, icHashKey
    , icAugment, icAugmented
  , NoInferred(..), Inferred
  , NoStored(..), Stored
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Cache as Cache
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer

type T = Transaction
type CT m = StateT Cache (T m)

data NoInferred = NoInferred
type Inferred = Infer.Payload

data NoStored = NoStored
type Stored m = ExprIRef.ValIProperty m

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ValI t }
  deriving (Eq, Ord, Binary, Typeable)

data InferContext = InferContext
  { _icContext :: Infer.Context
  , _icHashKey :: Cache.KeyBS
    -- ^ icHashKey is a "compact" unique identifier of the icContext
    -- for cheaper memoization
  }
Lens.makeLenses ''InferContext

icAugment ::
  (Monad n, Typeable a, Binary a) =>
  a -> StateT InferContext n ()
icAugment x = State.modify $ icAugmented x

icAugmented :: (Typeable a, Binary a) => a -> InferContext -> InferContext
icAugmented x = icHashKey %~ Cache.bsOfKey . (,) x
