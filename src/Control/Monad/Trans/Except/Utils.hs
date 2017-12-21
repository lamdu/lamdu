module Control.Monad.Trans.Except.Utils
    ( justToLeft
    , MatcherT, runMatcherT
    ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT(..))

-- | A Matcher is used an an early-termination matching mechanism
type MatcherT m a = ExceptT a m a

runMatcherT :: Functor m => MatcherT m a -> m a
runMatcherT = fmap uneither . runExceptT

justToLeft :: Monad m => MaybeT m a -> ExceptT a m ()
justToLeft = maybe (return ()) throwE <=< lift . runMaybeT

uneither :: Either a a -> a
uneither = either id id
