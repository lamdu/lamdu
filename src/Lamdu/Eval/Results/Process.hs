{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module Lamdu.Eval.Results.Process
    ( addTypes
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Hyper
import           Hyper.Class.Optic (HNodeLens(..))
import qualified Hyper.Type.AST.Nominal as N
import           Hyper.Type.AST.Row (RowExtend(..))
import qualified Hyper.Type.AST.Row as Row
import           Hyper.Type.AST.Scheme (sTyp, _QVarInstances, QVarInstances, Scheme)
import           Hyper.Unify.QuantifiedVar (HasQuantifiedVar(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Calc.Type as T
import           Lamdu.Eval.Results (Val, Body(..))
import qualified Lamdu.Eval.Results as ER

import           Lamdu.Prelude

extractRecordTypeField :: T.Tag -> Pure # T.Type -> Maybe (Pure # T.Type, Pure # T.Type)
extractRecordTypeField tag typ =
    do
        flat <- typ ^? _Pure . T._TRecord . T.flatRow
        fieldType <- flat ^. Row.freExtends . Lens.at tag
        Just
            ( fieldType
            , _Pure . T._TRecord . T.flatRow # (flat & Row.freExtends . Lens.at tag .~ Nothing)
            )

extractVariantTypeField :: T.Tag -> Pure # T.Type -> Maybe (Pure # T.Type)
extractVariantTypeField tag typ =
    typ ^? _Pure . T._TVariant . T.flatRow
    >>= (^. Row.freExtends . Lens.at tag)

type AddTypes val f = (Pure # T.Type -> val -> f :# Body) -> Pure # T.Type -> Body f

typeError :: String -> Body val
typeError = RError . ER.EvalTypeError . Text.pack

addTypesRecExtend ::
    RowExtend T.Tag val val # k ->
    (Pure # T.Type -> k # val -> f # Body) ->
    Pure # T.Type ->
    Body # f
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

addTypesInject :: ER.Inject # Ann a -> AddTypes (Ann a # Body) f
addTypesInject (ER.Inject tag val) go typ =
    case extractVariantTypeField tag typ of
    Nothing ->
        -- TODO: this is a work-around for a bug. HACK
        -- we currently don't know types for eval results of polymorphic values
        case typ ^. _Pure of
        T.TVar{} -> go typ val & ER.Inject tag & RInject
        _ -> "addTypesInject got " ++ show typ & typeError
    Just valType -> go valType val & ER.Inject tag & RInject

addTypesArray :: [val] -> AddTypes val f
addTypesArray items go typ =
    case typ ^? _Pure . T._TInst . N.nArgs . hNodeLens . _QVarInstances . Lens.ix Builtins.valTypeParamId of
    Nothing ->
        -- TODO: this is a work-around for a bug. HACK
        -- we currently don't know types for eval results of polymorphic values
        case typ ^. _Pure of
        T.TVar{} -> items <&> go typ & RArray
        _ -> "addTypesArray got " ++ show typ & typeError
    Just paramType -> items <&> go paramType & RArray

addTypes :: Map T.NominalId (Pure # N.NominalDecl T.Type) -> Pure # T.Type -> Val () -> Val (Pure # T.Type)
addTypes nomsMap typ (Ann (Const ()) b) =
    case b of
    RRecExtend recExtend -> r (addTypesRecExtend recExtend)
    RInject inject -> r (addTypesInject inject)
    RArray items -> r (addTypesArray items)
    RFunc x -> RFunc x
    RRecEmpty -> RRecEmpty
    RPrimVal l -> RPrimVal l
    RError e -> RError e
    & Ann (Const typ)
    where
        r f = f (addTypes nomsMap) (unwrapTInsts nomsMap typ)

class
    (HFunctor k, HasQuantifiedVar k, Ord (QVar k), HNodeLens T.Types k) =>
    ApplyNominal k where
    applyNominalRecursive :: Proxy k -> Dict (HNodesConstraint k ApplyNominal)
instance ApplyNominal T.Type where applyNominalRecursive _ = Dict
instance ApplyNominal T.Row where applyNominalRecursive _ = Dict

applyNominal ::
    Pure # N.NominalDecl T.Type ->
    T.Types # QVarInstances Pure ->
    Pure # Scheme T.Types T.Type
applyNominal nom params =
    _Pure # (nom ^. _Pure . N.nScheme & sTyp %~ subst params)

subst ::
    forall t.
    ApplyNominal t =>
    T.Types # QVarInstances Pure ->
    Pure # t ->
    Pure # t
subst params (Pure x) =
    withDict (applyNominalRecursive (Proxy @t)) $
    _Pure #
    case x ^? quantifiedVar of
    Nothing -> hmap (Proxy @ApplyNominal #> subst params) x
    Just q ->
        params ^?
        hNodeLens . _QVarInstances . Lens.ix q . _Pure
        & fromMaybe (quantifiedVar # q)

-- Will loop forever for bottoms like: newtype Void = Void Void
unwrapTInsts :: Map T.NominalId (Pure # N.NominalDecl T.Type) -> Pure # T.Type -> Pure # T.Type
unwrapTInsts nomsMap typ =
    case typ ^. _Pure of
    T.TInst (N.NominalInst tid params) ->
        Map.lookup tid nomsMap
        <&> (\nominalInst -> applyNominal nominalInst params ^. _Pure . sTyp)
        <&> unwrapTInsts nomsMap
        & fromMaybe typ
    _ -> typ
