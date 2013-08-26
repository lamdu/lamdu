{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..), T, CT
  , InferContext(..), icContext, icHashKey
  , initialInferContext
  , NoInferred(..), InferredWC
  , NoStored(..), Stored
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable, Typeable1)
import Lamdu.Data.Expression.IRef (DefIM)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..))
import qualified Control.Lens as Lens
import qualified Data.Cache as Cache
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer

type T = Transaction
type CT m = StateT Cache (T m)

data NoInferred = NoInferred
type InferredWC m = InferredWithConflicts (DefIM m)

data NoStored = NoStored
type Stored m = ExprIRef.ExpressionProperty m

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ExpressionI t }
  deriving (Eq, Ord, Binary, Typeable)

data InferContext m = InferContext
  { _icContext :: Infer.Context (DefIM m)
  , _icHashKey :: Cache.KeyBS
    -- ^ icHashKey is a "compact" unique identifier of the icContext
    -- for cheaper memoization
  }
Lens.makeLenses ''InferContext

initialInferContext :: Typeable1 m => DefIM m -> (InferContext m, Infer.Node (DefIM m))
initialInferContext defI =
  Infer.initial (Just defI) & Lens._1 %~ wrapContext
  where
    wrapContext inferContext =
      InferContext inferContext $ Cache.bsOfKey defI
