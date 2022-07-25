-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertScheme
    ) where

import qualified Hyper.Syntax.Scheme as S
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertScheme ::
    Applicative m =>
    EntityId -> Pure # T.Scheme -> m (Scheme InternalName)
convertScheme _ (Pure (S.Scheme tvs _)) = Scheme tvs () & pure
