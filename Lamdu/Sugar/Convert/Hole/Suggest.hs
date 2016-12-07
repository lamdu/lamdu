{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( value
    , valueConversion
    , valueNoSplit
    , fillHoles
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal, _NominalType)
import qualified Lamdu.Calc.Type.Nominal as Nominal
import           Lamdu.Calc.Type.Scheme (schemeType)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Infer (Context, Payload(..))
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Update (update)
import qualified Lamdu.Infer.Update as Update

import           Lamdu.Prelude

type Nominals = Map T.NominalId Nominal

loadNominalsForType :: Monad m => (T.NominalId -> m Nominal) -> Type -> m Nominals
loadNominalsForType loadNominal typ =
    go Map.empty (typ ^. ExprLens.typeTIds . Lens.to Set.singleton)
    where
        go res toLoad
            | Set.null toLoad = return res
            | otherwise =
                do
                    nominals <- Map.fromSet loadNominal toLoad & sequenceA
                    let result = mappend res nominals
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
    do
        nominals <-
            loadNominalsForType loadNominal
            (src ^. Val.payload . _1 . Infer.plType)
        valueConversionH nominals empty src & return

valueConversionH ::
    Nominals -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
valueConversionH nominals empty src =
    case srcInferPl ^. Infer.plType of
    T.TRecord composite ->
        composite ^.. ExprLens.compositeFields
        <&> getField & lift
        & prependOpt src
        where
            getField (tag, typ) =
                V.GetField src tag & V.BGetField
                & Val (Payload typ (srcInferPl ^. Infer.plScope), empty)
    _ -> valueConversionNoSplit nominals empty src
    where
        srcInferPl = src ^. Val.payload . _1

prependOpt :: a -> StateT s [] a -> StateT s [] a
prependOpt opt act = StateT $ \s -> (opt, s) : runStateT act s

valueConversionNoSplit ::
    Nominals -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
valueConversionNoSplit nominals empty src =
    prependOpt src $
    case srcType of
    T.TInst name _params
        | Lens.has (Lens.ix name . Nominal.nomType . Nominal._NominalType) nominals
        && bodyNot V._BToNom ->
        -- TODO: Expose primitives from Infer to do this without partiality
        do
            (_, resType) <-
                Infer.inferFromNom nominals (V.Nom name ())
                (\_ () -> return (srcType, ()))
                srcScope
            updated <-
                src & Lens.traversed . _1 . Infer.plType %%~ update
                & Update.liftInfer
            V.Nom name updated & V.BFromNom & mkRes resType & return
        & Infer.run
        & mapStateT
            (either (error "Infer of FromNom on non-opaque Nominal shouldn't fail") return)
        >>= valueConversionNoSplit nominals empty
    T.TFun argType resType | bodyNot V._BLam ->
        if Lens.has (ExprLens.valLeafs . V._LHole) arg
            then
                -- If the suggested argument has holes in it
                -- then stop suggesting there to avoid "overwhelming"..
                return applied
            else valueConversionNoSplit nominals empty applied
        where
            arg =
                valueNoSplit (Payload argType srcScope)
                & Lens.traversed %~ flip (,) empty
            applied = V.Apply src arg & V.BApp & mkRes resType
    T.TSum composite | bodyNot V._BInject  ->
        do
            dstType <-
                Infer.freshInferredVar srcScope "s"
                & Infer.run
                & mapStateT
                    (either (error "Infer.freshInferredVar shouldn't fail")
                    return)
            suggestCaseWith composite (Payload dstType srcScope)
                & Lens.traversed %~ flip (,) empty
                & (`V.Apply` src) & V.BApp & mkRes dstType
                & return
    _ -> mzero
    where
        srcInferPl = src ^. Val.payload . _1
        srcType = srcInferPl ^. Infer.plType
        srcScope = srcInferPl ^. Infer.plScope
        mkRes typ = Val (Payload typ srcScope, empty)
        bodyNot f = Lens.nullOf (Val.body . f) src

value :: Payload -> [Val Payload]
value pl@(Payload (T.TSum comp) scope) =
    case comp of
    T.CVar{} -> [V.BLeaf V.LHole]
    _ -> comp ^.. ExprLens.compositeFields <&> inject
    <&> Val pl
    where
        inject (tag, innerTyp) =
            valueNoSplit (Payload innerTyp scope) & V.Inject tag & V.BInject
value typ = [valueNoSplit typ]

valueNoSplit :: Payload -> Val Payload
valueNoSplit (Payload (T.TRecord composite) scope) =
    suggestRecordWith composite scope
valueNoSplit (Payload (T.TFun (T.TSum composite) r) scope) =
    suggestCaseWith composite (Payload r scope)
valueNoSplit pl@(Payload typ scope) =
    case typ of
    T.TFun _ r ->
        -- TODO: add var to the scope?
        valueNoSplit (Payload r scope) & V.Lam "var" & V.BLam
    _ -> V.BLeaf V.LHole
    & Val pl

suggestRecordWith :: T.Product -> Infer.Scope -> Val Payload
suggestRecordWith recordType scope =
    case recordType of
    T.CVar{} -> V.BLeaf V.LHole
    T.CEmpty -> V.BLeaf V.LRecEmpty
    T.CExtend f t r ->
        V.RecExtend f
        (valueNoSplit (Payload t scope))
        (suggestRecordWith r scope)
        & V.BRecExtend
    & Val (Payload (T.TRecord recordType) scope)

suggestCaseWith :: T.Sum -> Payload -> Val Payload
suggestCaseWith sumType resultPl@(Payload resultType scope) =
    case sumType of
    T.CVar{} -> V.BLeaf V.LHole
    T.CEmpty -> V.BLeaf V.LAbsurd
    T.CExtend tag fieldType rest ->
        V.Case tag
        (valueNoSplit (Payload (T.TFun fieldType resultType) scope))
        (suggestCaseWith rest resultPl)
        & V.BCase
    & Val (Payload (T.TFun (T.TSum sumType) resultType) scope)

fillHoles :: a -> Val (Payload, a) -> Val (Payload, a)
fillHoles empty (Val pl (V.BLeaf V.LHole)) =
    valueNoSplit (pl ^. _1) <&> flip (,) empty & Val.payload . _2 .~ (pl ^. _2)
fillHoles empty (Val pl (V.BApp (V.Apply func arg))) =
    -- Dont fill in holes inside apply funcs. This may create redexes..
    fillHoles empty arg & V.Apply func & V.BApp & Val pl
fillHoles empty val = val & Val.body . Lens.traversed %~ fillHoles empty
