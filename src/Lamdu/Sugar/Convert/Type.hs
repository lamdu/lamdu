-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertType, convertInferredScheme
    ) where

import           Hyper.Syntax.Scheme (sTyp)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertType ::
    Applicative m =>
    EntityId -> Pure # T.Type -> m ()
convertType _ (Pure _) = pure ()

convertInferredScheme ::
    (Applicative i, Applicative m) =>
    EntityId -> Pure # T.Scheme -> m (Scheme InternalName i Proxy)
convertInferredScheme entityId s =
    convertType entityId (s ^. _Pure . sTyp) <&> Scheme foralls
    where
        foralls =
            TaggedList
            (pure undefined) -- TODO: Proper tlAddFirst even though no action
            Nothing -- TODO: Convert foralls so GUI could display them
