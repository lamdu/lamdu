module Control.Monad.Trans.Either.Utils
  ( leftToJust, justToLeft
  , Matcher, runMatcher, runMatcherT
  , eitherToMaybeT
  ) where

import Control.Monad (mzero, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), left)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Functor.Identity (Identity(..))

-- | A Matcher is used an an early-termination matching mechanism
type MatcherT m a = EitherT a m a
type Matcher a = MatcherT Identity a

runMatcherT :: Functor m => MatcherT m a -> m a
runMatcherT = fmap uneither . runEitherT

runMatcher :: Matcher a -> a
runMatcher = runIdentity . runMatcherT

justToLeft :: Monad m => MaybeT m a -> EitherT a m ()
justToLeft = maybe (return ()) left <=< lift . runMaybeT

leftToJust :: Monad m => EitherT a m () -> MaybeT m a
leftToJust = either return (const mzero) <=< lift . runEitherT

uneither :: Either a a -> a
uneither = either id id

eitherToMaybeT :: Monad m => Either l a -> MaybeT m a
eitherToMaybeT = MaybeT . return . either (const Nothing) Just
