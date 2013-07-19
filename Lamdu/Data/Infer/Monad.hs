module Lamdu.Data.Infer.Monad
  ( Error(..), InferActions(..), Infer, liftError, error
  , run, substOrUnify
  ) where

import Prelude hiding (error)

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..), mapStateT)
import Data.Store.Guid (Guid)
import Data.UnionFind (Ref)
import Lamdu.Data.Expression.Utils () -- Expr.Body Show instance
import Lamdu.Data.Infer.Internal
import qualified Control.Monad.Trans.Reader as Reader
import qualified Lamdu.Data.Expression as Expr

data Error def
  = VarEscapesScope Guid
  | VarNotInScope
  | InfiniteExpression Ref
  | Mismatch (Expr.Body def Ref) (Expr.Body def Ref)
  deriving (Show)

newtype InferActions def = InferActions
  { iaSubstOrUnify :: Ref -> AppliedPiResult -> Infer def ()
  }

type Infer def a =
  StateT (Context def)
  (ReaderT (InferActions def)
   (Either (Error def))) a

liftError :: Either (Error def) a -> Infer def a
liftError = lift . lift

error :: Error def -> Infer def a
error = liftError . Left

run :: InferActions def -> Infer def a -> StateT (Context def) (Either (Error def)) a
run inferActions = mapStateT (`runReaderT` inferActions)

substOrUnify :: Eq def => Ref -> AppliedPiResult -> Infer def ()
substOrUnify ref apr = do
  act <- lift (Reader.asks iaSubstOrUnify)
  act ref apr
