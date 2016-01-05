{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, NoImplicitPrelude, RecordWildCards #-}
module Lamdu.Eval.Results
    ( Body(..), _RRecExtend, _RInject, _RFunc, _RRecEmpty, _RLiteral, _RError
    , Val(..), payload, body, fromEval
    , extractField, addTypes
    , EvalResults(..), erExprValues, erAppliesOfLam
    , empty
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Lamdu.Eval.Val (ScopeId, EvalError)
import qualified Lamdu.Eval.Val as EV
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

data Body val
    = RRecExtend (V.RecExtend val)
    | RInject (V.Inject val)
    | RFunc
    | RRecEmpty
    | RLiteral V.Literal
    | RError EvalError
    deriving (Show, Functor, Foldable, Traversable)

data Val pl = Val
    { _payload :: pl
    , _body :: Body (Val pl)
    } deriving (Show, Functor, Foldable, Traversable)

fromEval :: EV.Val srcId -> Val ()
fromEval v =
    case v of
    EV.HRecExtend x -> x <&> fromEval & RRecExtend
    EV.HInject x -> x <&> fromEval & RInject
    EV.HRecEmpty -> RRecEmpty
    EV.HLiteral x -> RLiteral x
    EV.HFunc {} -> RFunc
    EV.HAbsurd {} -> RFunc
    EV.HCase {} -> RFunc
    EV.HBuiltin {} -> RFunc
    EV.HError err -> RError err
    & Val ()

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

extractField :: T.Tag -> Val () -> Val ()
extractField tag (Val () (RRecExtend (V.RecExtend vt vv vr)))
    | vt == tag = vv
    | otherwise = extractField tag vr
extractField _ v@(Val () RError {}) = v
extractField tag x =
    "Expected record with tag: " ++ show tag ++ " got: " ++ show x
    & EV.EvalTypeError & RError & Val ()

data EvalResults srcId =
    EvalResults
    { _erExprValues :: Map srcId (Map ScopeId (Val ()))
    , _erAppliesOfLam :: Map srcId (Map ScopeId [(ScopeId, Val ())])
    } deriving Show

empty :: EvalResults srcId
empty =
    EvalResults
    { _erExprValues = Map.empty
    , _erAppliesOfLam = Map.empty
    }

Lens.makeLenses ''EvalResults
Lens.makeLenses ''Val
Lens.makePrisms ''Body
