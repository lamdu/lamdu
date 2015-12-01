{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.GetVar
    ( convertVar
    ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Store.Guid (Guid)
import           Lamdu.Builtins.Anchors (recurseVar)
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Monad (siTagParamInfos, tpiFromParameters)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

convertVar :: MonadA m => ConvertM.Context m -> V.Var -> Type -> GetVar Guid m
convertVar sugarContext param paramType
    | param == recurseVar =
      GetBinder BinderVar
      { _bvNameRef = selfNameRef
      , _bvForm = GetDefinition
      , _bvMInline = Nothing
      }

    | isGetLet =
      GetBinder BinderVar
      { _bvNameRef = paramNameRef
      , _bvForm = GetLet
      , _bvMInline = Nothing
      }

    | isGetParamRecord =
      GetParamsRecord ParamsRecordVar { _prvFieldNames = typeRecordGuids }

    | otherwise =
      GetParam (Param paramNameRef GetParameter NormalBinder)

    where
        selfNameRef =
            NameRef
            { _nrName = UniqueId.toGuid defI
            , _nrGotoDefinition = pure $ EntityId.ofIRef defI
            }
        paramNameRef =
            NameRef
            { _nrName = UniqueId.toGuid param
            , _nrGotoDefinition = pure $ EntityId.ofLambdaParam param
            }
        typeRecordGuids = typeRecordTags <&> UniqueId.toGuid
        typeRecordTags = paramType ^.. ExprLens._TRecord . ExprLens.compositeTags
        isGetLet = param `Set.member` (scopeInfo ^. ConvertM.siLetItems)
        isGetParamRecord = param `elem` recordParamVars
        scopeInfo = sugarContext ^. ConvertM.scScopeInfo
        recordParamVars =
            scopeInfo ^. siTagParamInfos & Map.elems & map tpiFromParameters
        defI =
            sugarContext ^. ConvertM.scDefI
            & fromMaybe (error "recurseVar used not in definition context?!")
