module Lamdu.Sugar.Convert.GetVar
    ( convertVar
    ) where

import Control.Applicative (Applicative(..))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

convertVar :: MonadA m => ConvertM.Context m -> V.Var -> GetVar Guid m
convertVar sugarContext param
  | param == recurseGetVar =
    GetVar
    { _gvName = UniqueId.toGuid defI
    , _gvJumpTo = pure $ EntityId.ofIRef defI
    , _gvVarType = GetDefinition
    }
  | otherwise =
    GetVar
    { _gvName = UniqueId.toGuid param
    , _gvJumpTo = pure $ EntityId.ofLambdaParam param
    , _gvVarType = GetParameter
    }
    where
        defI = sugarContext ^. ConvertM.scDefI
