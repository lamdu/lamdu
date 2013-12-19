{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..), T, CT
  , InferContext(..), icContext, icHashKey
    , icAugment, icAugmented
  , initialInferContext
  , NoInferred(..), Inferred
  , NoStored(..), Stored
  , LoadedExpr
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT, runState)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable, Typeable1)
import Lamdu.Data.Expr.IRef (DefIM)
import Lamdu.Data.Infer.Deref (DerefedTV)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Cache as Cache
import qualified Lamdu.Data.Expr.IRef as ExprIRef
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Load as Load
import qualified System.Random as Random

type T = Transaction
type CT m = StateT Cache (T m)

data NoInferred = NoInferred
type Inferred m = DerefedTV (DefIM m)

data NoStored = NoStored
type Stored m = ExprIRef.ExprProperty m

type LoadedExpr m = Load.LoadedExpr (DefIM m)

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ExprI t }
  deriving (Eq, Ord, Binary, Typeable)

data InferContext m = InferContext
  { _icContext :: Infer.Context (DefIM m)
  , _icHashKey :: Cache.KeyBS
    -- ^ icHashKey is a "compact" unique identifier of the icContext
    -- for cheaper memoization
  }
Lens.makeLenses ''InferContext

icAugment ::
  (Monad n, Typeable a, Binary a) =>
  a -> StateT (InferContext m) n ()
icAugment x = State.modify $ icAugmented x

icAugmented :: (Typeable a, Binary a) => a -> InferContext m -> InferContext m
icAugmented x = icHashKey %~ Cache.bsOfKey . (,) x

emptyContext :: Infer.Context (DefIM m)
emptyContext = Infer.emptyContext $ Random.mkStdGen 0

initialInferContext ::
  Typeable1 m => DefIM m -> (Infer.TypedValue (DefIM m), InferContext m)
initialInferContext defI =
  runState (Load.newDefinition defI) emptyContext & Lens._2 %~ wrapContext
  where
    wrapContext inferContext =
      InferContext inferContext $ Cache.bsOfKey defI
