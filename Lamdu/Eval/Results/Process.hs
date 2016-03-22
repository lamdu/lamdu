{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Eval.Results.Process
    ( addTypes
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
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

addTypes :: Map T.NominalId N.Nominal -> T.Type -> Val () -> Val T.Type
addTypes nomsMap typ (Val () b) =
    case b of
    RRecExtend (V.RecExtend tag val rest) ->
        case extractRecordTypeField tag bodyType of
        Nothing -> ER.EvalTypeError "addTypes bad type for RRecExtend" & RError
        Just (valType, restType) ->
            V.RecExtend tag
            (addTypes nomsMap valType val) (addTypes nomsMap restType rest)
            & RRecExtend
    RInject (V.Inject tag val) ->
        case extractSumTypeField tag bodyType of
        Nothing -> ER.EvalTypeError "addTypes bad type for RInject" & RError
        Just valType -> addTypes nomsMap valType val & V.Inject tag & RInject
    RFunc -> RFunc
    RRecEmpty -> RRecEmpty
    RPrimVal l -> RPrimVal l
    RError e -> RError e
    & Val typ
    where
        bodyType =
            case typ of
            T.TInst tid params ->
                Map.lookup tid nomsMap
                & fromMaybe (error "addTypes: nominal missing from map")
                & N.apply params & (^. schemeType)
            _ -> typ
