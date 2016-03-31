{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
module Lamdu.Eval.Results.Process
    ( addTypes
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Eval.Results (Val(..), Body(..))
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Nominal as N
import           Lamdu.Expr.Scheme (schemeType)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

extractRecordTypeField :: T.Tag -> T.Type -> Maybe (T.Type, T.Type)
extractRecordTypeField tag typ =
    do
        comp <- typ ^? ExprLens._TRecord
        let flat = FlatComposite.fromComposite comp
        fieldType <- flat ^. FlatComposite.fields . Lens.at tag
        Just
            ( fieldType
            , flat
              & FlatComposite.fields . Lens.at tag .~ Nothing
              & FlatComposite.toComposite & T.TRecord
            )

extractSumTypeField :: T.Tag -> T.Type -> Maybe T.Type
extractSumTypeField tag typ =
    do
        comp <- typ ^? ExprLens._TSum
        FlatComposite.fromComposite comp ^. FlatComposite.fields . Lens.at tag

type AddTypes val res = (T.Type -> val -> res) -> T.Type -> Body res

addTypesRecExtend :: V.RecExtend val -> AddTypes val res
addTypesRecExtend (V.RecExtend tag val rest) go typ =
    case extractRecordTypeField tag typ of
    Nothing ->
        -- TODO: this is a work-around for a bug. HACK
        -- we currently don't know types for eval results of polymorphic values
        case typ of
        T.TVar{} ->
            V.RecExtend tag
            (go typ val)
            (go typ rest)
            & RRecExtend
        _ -> ER.EvalTypeError "addTypes bad type for RRecExtend" & RError
    Just (valType, restType) ->
        V.RecExtend tag
        (go valType val)
        (go restType rest)
        & RRecExtend

addTypesInject :: V.Inject val -> AddTypes val res
addTypesInject (V.Inject tag val) go typ =
    case extractSumTypeField tag typ of
    Nothing ->
        -- TODO: this is a work-around for a bug. HACK
        -- we currently don't know types for eval results of polymorphic values
        case typ of
        T.TVar{} -> go typ val & V.Inject tag & RInject
        _ -> ER.EvalTypeError "addTypes bad type for RInject" & RError
    Just valType -> go valType val & V.Inject tag & RInject

addTypesArray :: [val] -> AddTypes val res
addTypesArray items go typ =
    do
        (nomId, params) <- typ ^? ExprLens._TInst
        nomId == Builtins.arrayTid & guard
        paramType <-
            case Map.toList params of
            [(k, v)] ->
                do
                    k == Builtins.valTypeParamId & guard
                    Just v
            _ -> Nothing
        items <&> go paramType & RArray & Just
    & fromMaybe (ER.EvalTypeError "addTypes bad type for RArray" & RError)

addTypes :: Map T.NominalId N.Nominal -> T.Type -> Val () -> Val T.Type
addTypes nomsMap typ (Val () b) =
    case b of
    RRecExtend recExtend -> recurse (addTypesRecExtend recExtend)
    RInject inject -> recurse (addTypesInject inject)
    RArray items -> recurse (addTypesArray items)
    RFunc -> RFunc
    RRecEmpty -> RRecEmpty
    RPrimVal l -> RPrimVal l
    RError e -> RError e
    & Val typ
    where
        recurse f = f (addTypes nomsMap) (unwrapTInsts nomsMap typ)

-- Will loop forever for bottoms like: newtype Void = Void Void
unwrapTInsts :: Map T.NominalId N.Nominal -> T.Type -> T.Type
unwrapTInsts nomsMap typ =
    case typ of
    T.TInst tid params ->
        Map.lookup tid nomsMap
        & fromMaybe (error "addTypes: nominal missing from map")
        & N.apply params
        & \case
          N.OpaqueNominal -> typ
          N.NominalType scheme -> scheme ^. schemeType & unwrapTInsts nomsMap
    _ -> typ
