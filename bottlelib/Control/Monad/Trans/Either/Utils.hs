module Control.Monad.Trans.Either.Utils
  ( leftToJust, justToLeft
  ) where

import Control.Monad (mzero, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), left)
import Control.Monad.Trans.Maybe (MaybeT(..))

justToLeft :: Monad m => MaybeT m a -> EitherT a m ()
justToLeft = maybe (return ()) left <=< lift . runMaybeT

leftToJust :: Monad m => EitherT a m () -> MaybeT m a
leftToJust = either return (const mzero) <=< lift . runEitherT
