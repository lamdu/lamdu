{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Lamdu.Eval.Results.Process
    ( addTypes
    ) where

import           AST (Tree, Tie, Ann(..), Pure(..), _Pure, Recursive)
import           AST.Class.Combinators (And, proxyNoConstraint, NoConstraint)
import           AST.Class.HasChild (HasChild)
import qualified AST.Class.HasChild as HasChild
import qualified AST.Term.Nominal as N
import           AST.Term.Row (RowExtend(..))
import qualified AST.Term.Row as Row
import           AST.Term.Scheme (sTyp, _QVarInstances, QVarInstances, Scheme)
import           AST.Unify (QVarHasInstance)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Eval.Results (Val, Body(..))
import qualified Lamdu.Eval.Results as ER

import           Lamdu.Prelude

extractRecordTypeField :: T.Tag -> Tree Pure T.Type -> Maybe (Tree Pure T.Type, Tree Pure T.Type)
extractRecordTypeField tag typ =
    do
        flat <- typ ^? _Pure . T._TRecord . T.flatRow
        fieldType <- flat ^. Row.freExtends . Lens.at tag
        Just
            ( fieldType
            , _Pure . T._TRecord . T.flatRow # (flat & Row.freExtends . Lens.at tag .~ Nothing)
            )

extractVariantTypeField :: T.Tag -> Tree Pure T.Type -> Maybe (Tree Pure T.Type)
extractVariantTypeField tag typ =
    typ ^? _Pure . T._TVariant . T.flatRow
    >>= (^. Row.freExtends . Lens.at tag)

type AddTypes val f = (Tree Pure T.Type -> val -> Tie f Body) -> Tree Pure T.Type -> Body f

typeError :: String -> Body val
typeError = RError . ER.EvalTypeError . Text.pack

addTypesRecExtend ::
    Tree (RowExtend T.Tag val val) k ->
    (Tree Pure T.Type -> Tree k val -> Tree f Body) ->
    Tree Pure T.Type ->
    Tree Body f
addTypesRecExtend (RowExtend tag val rest) go typ =
    case extractRecordTypeField tag typ of
    Nothing ->
        -- TODO: this is a work-around for a bug. HACK
        -- we currently don't know types for eval results of polymorphic values
        case typ ^. _Pure of
        T.TVar{} ->
            RowExtend tag
            (go typ val)
            (go typ rest)
            & RRecExtend
        T.TInst{} ->
            -- Work around for MutRefs: todo better presentation which shows their current value?
            RRecEmpty
        _ -> "addTypesRecExtend got " ++ show typ & typeError
    Just (valType, restType) ->
        RowExtend tag
        (go valType val)
        (go restType rest)
        & RRecExtend

addTypesInject :: V.Inject val -> AddTypes val f
addTypesInject (V.Inject tag val) go typ =
    case extractVariantTypeField tag typ of
    Nothing ->
        -- TODO: this is a work-around for a bug. HACK
        -- we currently don't know types for eval results of polymorphic values
        case typ ^. _Pure of
        T.TVar{} -> go typ val & V.Inject tag & RInject
        _ -> "addTypesInject got " ++ show typ & typeError
    Just valType -> go valType val & V.Inject tag & RInject

addTypesArray :: [val] -> AddTypes val f
addTypesArray items go typ =
    case typ ^? _Pure . T._TInst . N.nArgs . HasChild.getChild . _QVarInstances . Lens.ix Builtins.valTypeParamId of
    Nothing ->
        -- TODO: this is a work-around for a bug. HACK
        -- we currently don't know types for eval results of polymorphic values
        case typ ^. _Pure of
        T.TVar{} -> items <&> go typ & RArray
        _ -> "addTypesArray got " ++ show typ & typeError
    Just paramType -> items <&> go paramType & RArray

addTypes :: Map T.NominalId (Tree Pure (N.NominalDecl T.Type)) -> Tree Pure T.Type -> Val () -> Val (Tree Pure T.Type)
addTypes nomsMap typ (Ann () b) =
    case b of
    RRecExtend recExtend -> recurse (addTypesRecExtend recExtend)
    RInject inject -> recurse (addTypesInject inject)
    RArray items -> recurse (addTypesArray items)
    RFunc x -> RFunc x
    RRecEmpty -> RRecEmpty
    RPrimVal l -> RPrimVal l
    RError e -> RError e
    & Ann typ
    where
        recurse f = f (addTypes nomsMap) (unwrapTInsts nomsMap typ)

applyNominal ::
    Recursive
        (HasChild (N.NomVarTypes typ) `And` QVarHasInstance Ord `And` NoConstraint)
        typ =>
    Tree Pure (N.NominalDecl typ) ->
    Tree (N.NomVarTypes typ) (QVarInstances Pure) ->
    Tree Pure (Scheme (N.NomVarTypes typ) typ)
applyNominal nom params =
    _Pure #
    ( N.applyNominal proxyNoConstraint (Lens._Wrapped . _Pure #) nom params
    & runIdentity
    )

-- Will loop forever for bottoms like: newtype Void = Void Void
unwrapTInsts :: Map T.NominalId (Tree Pure (N.NominalDecl T.Type)) -> Tree Pure T.Type -> Tree Pure T.Type
unwrapTInsts nomsMap typ =
    case typ ^. _Pure of
    T.TInst (N.NominalInst tid params) ->
        Map.lookup tid nomsMap
        <&> (\nominalInst -> applyNominal nominalInst params ^. _Pure . sTyp)
        <&> unwrapTInsts nomsMap
        & fromMaybe typ
    _ -> typ
