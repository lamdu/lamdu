-- | A convenience monad transformer that can load, infer, unify and update.
-- Allows binding together actions of all these kinds.
module Lamdu.Infer.Trans
    ( M
    , liftInfer
    , liftInner
    , run
    ) where

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import qualified Lamdu.Debug as Debug
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr

import           Lamdu.Prelude

type M m = StateT Infer.Context (ExceptT InferErr.Error m)

liftInfer :: Applicative m => Infer a -> M m a
liftInfer = mapStateT (ExceptT . pure) . Infer.run

liftInner :: Monad m => m a -> M m a
liftInner = lift . lift

run :: (HasCallStack, Functor m) => Debug.Monitors -> M m a -> m (Either InferErr.Error (a, Infer.Context))
run monitors =
    fmap reportInferenceTime . runExceptT . (`runStateT` Infer.initialContext)
    where
        Debug.Evaluator reportInferenceTime =
            monitors ^. Debug.inference
