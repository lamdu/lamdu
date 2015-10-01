{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.GetVar
    ( convertVar
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import           Data.Store.Guid (Guid)
import           Lamdu.Builtins.Anchors (recurseVar)
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

convertVar :: MonadA m => ConvertM.Context m -> V.Var -> Type -> GetVar Guid m
convertVar sugarContext param paramType
    | param == recurseVar =
      GetVarNamed NamedVar
      { _nvName = UniqueId.toGuid defI
      , _nvJumpTo = pure $ EntityId.ofIRef defI
      , _nvVarType = GetDefinition
      , _nvMode = NormalBinder
      }
    | isGetParamRecord =
      GetVarParamsRecord ParamsRecordVar
      { _prvFieldNames = typeRecordGuids
      }
    | otherwise =
      GetVarNamed NamedVar
      { _nvName = UniqueId.toGuid param
      , _nvJumpTo = pure $ EntityId.ofLambdaParam param
      , _nvVarType = GetParameter
      , _nvMode = NormalBinder
      }
    where
        typeRecordGuids = typeRecordTags <&> UniqueId.toGuid
        typeRecordTags = paramType ^.. ExprLens._TRecord . ExprLens.compositeTags
        isGetParamRecord = param `elem` recordParamVars
        recordParamVars =
            sugarContext ^..
            ConvertM.scTagParamInfos . Lens.traverse .
            Lens.to ConvertM.tpiFromParameters
        defI =
            sugarContext ^. ConvertM.scDefI
            & fromMaybe (error "recurseVar used not in definition context?!")
