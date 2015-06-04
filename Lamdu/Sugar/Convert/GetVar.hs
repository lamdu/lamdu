module Lamdu.Sugar.Convert.GetVar
    ( convertVar
    ) where

import           Control.Applicative (Applicative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import           Lamdu.Builtins.Anchors (recurseVar)
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

convertVar :: MonadA m => ConvertM.Context m -> V.Var -> Type -> GetVar Guid m
convertVar sugarContext param paramType
  | param == recurseVar =
    GetVarNamed NamedVar
    { _nvName = UniqueId.toGuid defI
    , _nvJumpTo = pure $ EntityId.ofIRef defI
    , _nvVarType = GetDefinition
    }
  | otherwise =
    if isGetParamRecord
    then
        GetVarParamsRecord ParamsRecordVar
        { _prvFieldNames = typeRecordGuids
        }
    else
        GetVarNamed NamedVar
        { _nvName = UniqueId.toGuid param
        , _nvJumpTo = pure $ EntityId.ofLambdaParam param
        , _nvVarType = GetParameter
        }
    where
        typeRecordGuids = typeRecordTags <&> UniqueId.toGuid
        typeRecordTags = paramType ^.. ExprLens._TRecord . ExprLens.compositeTags
        isGetParamRecord = param `elem` recordParamVars
        recordParamVars =
            sugarContext ^..
            ConvertM.scTagParamInfos . Lens.traverse .
            Lens.to ConvertM.tpiFromParameters
        defI = sugarContext ^. ConvertM.scDefI
