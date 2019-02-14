{-# LANGUAGE TupleSections, TypeFamilies #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( value
    , valueConversion
    , fillHoles
    , applyForms
    ) where

import           AST (monoChildren)
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import           AST.Term.Row (RowExtend(..))
import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad (mzero)
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal, _NominalType)
import qualified Lamdu.Calc.Type.Nominal as Nominal
import           Lamdu.Calc.Type.Scheme (schemeType)
import           Lamdu.Infer (Context, Payload(..))
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (update)
import qualified Lamdu.Infer.Update as Update
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type Nominals = Map T.NominalId Nominal

loadNominalsForType :: Monad m => (T.NominalId -> m Nominal) -> Type -> m Nominals
loadNominalsForType loadNominal typ =
    go Map.empty (typ ^. ExprLens.typeTIds . Lens.to Set.singleton)
    where
        go res toLoad
            | Set.null toLoad = pure res
            | otherwise =
                do
                    nominals <- Map.fromSet loadNominal toLoad & sequenceA
                    let result = res <> nominals
                    let newTIds =
                            nominals
                            ^. Lens.traversed . Nominal.nomType
                            . _NominalType . schemeType
                            . ExprLens.typeTIds . Lens.to Set.singleton
                            & (`Set.difference` Map.keysSet result)
                    go result newTIds

valueConversion ::
    Monad m =>
    (T.NominalId -> m Nominal) -> a ->
    Val (Payload, a) -> m (StateT Context [] (Val (Payload, a)))
valueConversion loadNominal empty src =
    loadNominalsForType loadNominal
    (src ^. ann . _1 . Infer.plType)
    <&> \nominals -> valueConversionH nominals empty src

valueConversionH ::
    Nominals -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
valueConversionH nominals empty src =
    pure src <|>
    case srcInferPl ^. Infer.plType of
    T.TRecord composite
        | Lens.nullOf (val . V._BRecExtend) src ->
        composite ^.. ExprLens.compositeFields <&> getField & lift
        where
            getField (tag, typ) =
                V.GetField src tag
                & V.BGetField
                & Ann (Payload typ (srcInferPl ^. Infer.plScope), empty)
    _ -> valueConversionNoSplit nominals empty src
    where
        srcInferPl = src ^. ann . _1

valueConversionNoSplit ::
    Nominals -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
valueConversionNoSplit nominals empty src =
    case srcType of
    T.TInst name _params
        | Lens.has (Lens.ix name . Nominal.nomType . Nominal._NominalType) nominals
        && Lens.nullOf V._BToNom srcVal ->
        -- TODO: Expose primitives from Infer to do this without partiality
        do
            (_, resType) <-
                Infer.inferFromNom nominals (V.Nom name ())
                (\_ () -> pure (srcType, Ann () (V.BLeaf V.LHole)))
                srcScope
            updated <-
                src & annotations . _1 . Infer.plType %%~ update
                & Update.liftInfer
            V.Nom name updated & V.BFromNom & mkRes resType & pure
        & Infer.run
        & mapStateT
            (either (error "Infer of FromNom on non-opaque Nominal shouldn't fail") pure)
        >>= valueConversionNoSplit nominals empty
    T.TFun argType resType | Lens.nullOf V._BLam srcVal ->
        if Lens.has (ExprLens.valLeafs . V._LHole) arg
            then
                -- If the suggested argument has holes in it
                -- then stop suggesting there to avoid "overwhelming"..
                pure applied
            else valueConversionNoSplit nominals empty applied
        where
            arg =
                valueNoSplit (Payload argType srcScope)
                & annotations %~ (, empty)
            applied = V.Apply src arg & V.BApp & mkRes resType
    T.TVariant composite | Lens.nullOf V._BInject srcVal ->
        Infer.freshInferredVar srcScope "s"
        & Infer.run
        & mapStateT
            (either (error "Infer.freshInferredVar shouldn't fail") pure)
        <&>
        \dstType ->
        suggestCaseWith composite (Payload dstType srcScope)
        & annotations %~ (, empty)
        & (`V.Apply` src) & V.BApp & mkRes dstType
    _ -> mzero
    where
        srcInferPl = src ^. ann . _1
        srcType = srcInferPl ^. Infer.plType
        srcScope = srcInferPl ^. Infer.plScope
        mkRes typ = Ann (Payload typ srcScope, empty)
        srcVal = src ^. val

value :: Payload -> [Val Payload]
value pl@(Payload (T.TVariant comp) scope) =
    case comp of
    T.RVar{} -> [V.BLeaf V.LHole]
    _ -> comp ^.. ExprLens.compositeFields <&> inject
    <&> Ann pl
    where
        inject (tag, innerTyp) =
            valueNoSplit (Payload innerTyp scope) & V.Inject tag & V.BInject
value typ = [valueNoSplit typ]

valueNoSplit :: Payload -> Val Payload
valueNoSplit (Payload (T.TRecord composite) scope) =
    suggestRecordWith composite scope
valueNoSplit (Payload (T.TFun (T.TVariant composite) r) scope) =
    suggestCaseWith composite (Payload r scope)
valueNoSplit pl@(Payload typ scope) =
    case typ of
    T.TFun _ r ->
        -- TODO: add var to the scope?
        valueNoSplit (Payload r scope) & V.Lam "var" & V.BLam
    _ -> V.BLeaf V.LHole
    & Ann pl

suggestRecordWith :: T.Row -> Infer.Scope -> Val Payload
suggestRecordWith recordType scope =
    case recordType of
    T.RVar{} -> V.BLeaf V.LHole
    T.REmpty -> V.BLeaf V.LRecEmpty
    T.RExtend f t r ->
        RowExtend f
        (valueNoSplit (Payload t scope))
        (suggestRecordWith r scope)
        & V.BRecExtend
    & Ann (Payload (T.TRecord recordType) scope)

suggestCaseWith :: T.Row -> Payload -> Val Payload
suggestCaseWith variantType resultPl@(Payload resultType scope) =
    case variantType of
    T.RVar{} -> V.BLeaf V.LHole
    T.REmpty -> V.BLeaf V.LAbsurd
    T.RExtend tag fieldType rest ->
        RowExtend tag
        (valueNoSplit (Payload (T.TFun fieldType resultType) scope))
        (suggestCaseWith rest resultPl)
        & V.BCase
    & Ann (Payload (T.TFun (T.TVariant variantType) resultType) scope)

fillHoles :: a -> Val (Payload, a) -> Val (Payload, a)
fillHoles empty (Ann pl (V.BLeaf V.LHole)) =
    valueNoSplit (pl ^. _1)
    & annotations %~ (, empty)
    & ann . _2 .~ (pl ^. _2)
fillHoles empty (Ann pl (V.BApp (V.Apply func arg))) =
    -- Dont fill in holes inside apply funcs. This may create redexes..
    fillHoles empty arg & V.Apply func & V.BApp & Ann pl
fillHoles _ v@(Ann _ (V.BGetField (V.GetField (Ann _ (V.BLeaf V.LHole)) _))) =
    -- Dont fill in holes inside get-field.
    v
fillHoles empty x = x & val . monoChildren %~ fillHoles empty

applyForms ::
    ListClass.List m =>
    (T.NominalId -> StateT Context m Nominal) -> a -> Val (Payload, a) ->
    StateT Context m (Val (Payload, a))
applyForms _ _ v@(Ann _ V.BLam {}) = pure v
applyForms _ _ v@(Ann pl0 (V.BInject (V.Inject tag (Ann pl1 (V.BLeaf V.LHole))))) =
    pure (Ann pl0 (V.BInject (V.Inject tag (Ann pl1 (V.BLeaf V.LRecEmpty)))))
    <|> pure v
applyForms loadNominal empty x =
    case inferPl ^. Infer.plType of
    T.TVar tv
        | any (`Lens.has` x)
            [ ExprLens.valVar
            , ExprLens.valGetField . V.getFieldRecord . ExprLens.valVar
            ] ->
            -- a variable that's compatible with a function type
            pure x <|>
            do
                arg <- freshVar "af"
                res <- freshVar "af"
                let varTyp = T.TFun arg res
                unify varTyp (T.TVar tv)
                    & Infer.run & mapStateT assertSuccess
                V.BLeaf V.LHole
                    & Ann (plSameScope arg)
                    & V.Apply x & V.BApp
                    & Ann (plSameScope res)
                    & pure
        where
            assertSuccess (Left err) =
                fail $
                "Unify of a tv with function type should always succeed, but failed: " ++
                prettyShow err
            assertSuccess (Right t) = pure t
            freshVar = Infer.run . Infer.freshInferredVar (inferPl ^. Infer.plScope)
            scope = inferPl ^. Infer.plScope
            plSameScope t = (Infer.Payload t scope, empty)
    T.TRecord{} | Lens.has ExprLens.valVar x ->
        -- A "params record" (or just a let item which is a record..)
        pure x
    _ ->
        x
        & fillHoles empty
        & valueConversion loadNominal empty
        <&> mapStateT ListClass.fromList
        & join
    where
        inferPl = x ^. ann . _1
