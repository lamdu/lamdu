-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertScheme
    ) where

import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (Transaction)
import qualified Hyper.Syntax.Scheme as S
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

convertScheme ::
    Applicative f =>
    EntityId -> Pure # T.Scheme -> f (Scheme InternalName (OnceT (T m)) (T m))
convertScheme _ (Pure (S.Scheme _tvs _)) = Scheme undefined () & pure
