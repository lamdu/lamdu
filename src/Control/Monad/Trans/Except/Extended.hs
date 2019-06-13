module Control.Monad.Trans.Except.Extended
    ( module Control.Monad.Trans.Except
    , justToLeft
    , MatcherT, runMatcherT
    ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable (traverse_)

import Prelude

-- | A Matcher is used an an early-termination matching mechanism
type MatcherT m a = ExceptT a m a

runMatcherT :: Functor m => MatcherT m a -> m a
runMatcherT = fmap uneither . runExceptT

justToLeft :: Monad m => MaybeT m a -> ExceptT a m ()
justToLeft = traverse_ throwE <=< lift . runMaybeT

uneither :: Either a a -> a
uneither = either id id
