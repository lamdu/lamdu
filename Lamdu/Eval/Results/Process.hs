{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, NoImplicitPrelude, RecordWildCards #-}
module Lamdu.Eval.Results.Process
    ( addTypes
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Lamdu.Eval.Results (Val(..), Body(..))
import qualified Lamdu.Eval.Val as EV
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.Lens as ExprLens
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

addTypes :: T.Type -> Val () -> Val T.Type
addTypes typ (Val () b) =
    case b of
    RRecExtend (V.RecExtend tag val rest) ->
        case extractRecordTypeField tag typ of
        Nothing -> EV.EvalTypeError "addTypes bad type for RRecExtend" & RError
        Just (valType, restType) ->
            V.RecExtend tag (addTypes valType val) (addTypes restType rest)
            & RRecExtend
    RInject (V.Inject tag val) ->
        case extractSumTypeField tag typ of
        Nothing -> EV.EvalTypeError "addTypes bad type for RInject" & RError
        Just valType -> addTypes valType val & V.Inject tag & RInject
    RFunc -> RFunc
    RRecEmpty -> RRecEmpty
    RLiteral l -> RLiteral l
    RError e -> RError e
    & Val typ
