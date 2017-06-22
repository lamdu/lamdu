module Control.Monad.Trans.Either.Utils
    ( justToLeft
    , MatcherT, runMatcherT
    ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), left)
import Control.Monad.Trans.Maybe (MaybeT(..))

-- | A Matcher is used an an early-termination matching mechanism
type MatcherT m a = EitherT a m a

runMatcherT :: Functor m => MatcherT m a -> m a
runMatcherT = fmap uneither . runEitherT

justToLeft :: Monad m => MaybeT m a -> EitherT a m ()
justToLeft = maybe (return ()) left <=< lift . runMaybeT

uneither :: Either a a -> a
uneither = either id id
