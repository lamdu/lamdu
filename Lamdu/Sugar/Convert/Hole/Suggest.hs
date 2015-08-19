{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( value
    , valueConversion
    , valueNoSplit
    , fillHoles
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Nominal as Nominal
import           Lamdu.Expr.Scheme (schemeType)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Infer (Context, Payload(..))
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Update (update)
import qualified Lamdu.Infer.Update as Update

loadNominalsForType :: Monad m => (T.Id -> m Nominal) -> Type -> m Infer.Loaded
loadNominalsForType loadNominal typ =
    go Map.empty (typ ^. ExprLens.typeTIds . Lens.to Set.singleton)
    where
        go res toLoad
            | Set.null toLoad = Infer.Loaded Map.empty res & return
            | otherwise =
                do
                    loadedNominals <- Map.fromSet loadNominal toLoad & sequenceA
                    let result = mappend res loadedNominals
                    let newTIds =
                            loadedNominals
                            ^. Lens.traversed . Lens.to Nominal.nScheme . schemeType
                            . ExprLens.typeTIds . Lens.to Set.singleton
                            & (`Set.difference` Map.keysSet result)
                    go result newTIds

valueConversion ::
    MonadA m =>
    (T.Id -> m Nominal) -> a ->
    Val (Payload, a) -> m (StateT Context [] (Val (Payload, a)))
valueConversion loadNominal empty src =
    do
        loaded <-
            loadNominalsForType loadNominal
            (src ^. V.payload . _1 . Infer.plType)
        valueConversionH loaded empty src & return

valueConversionH ::
    Infer.Loaded -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
valueConversionH loaded empty src =
    case srcInferPl ^. Infer.plType of
    T.TRecord composite ->
        composite ^.. ExprLens.compositeFields
        <&> getField & lift
        & prependOpt src
        where
            getField (tag, typ) =
                V.GetField src tag & V.BGetField
                & V.Val (Payload typ (srcInferPl ^. Infer.plScope), empty)
    _ -> valueConversionNoSplit loaded empty src
    where
        srcInferPl = src ^. V.payload . _1

prependOpt :: a -> StateT s [] a -> StateT s [] a
prependOpt opt act = StateT $ \s -> (opt, s) : runStateT act s

valueConversionNoSplit ::
    Infer.Loaded -> a -> Val (Payload, a) ->
    StateT Context [] (Val (Payload, a))
valueConversionNoSplit loaded empty src =
    prependOpt src $
    case srcType of
    T.TInst name _params | bodyNot ExprLens._BToNom ->
        -- TODO: Expose primitives from Infer to do this without partiality
        do
            (_, resType) <-
                Infer.inferFromNom
                (Infer.loadedNominals loaded) (V.Nom name ())
                (\_ () -> return (srcType, ()))
                srcScope
            updated <-
                src & Lens.traversed . _1 . Infer.plType %%~ update
                & Update.liftInfer
            V.Nom name updated & V.BFromNom & mkRes resType & return
        & Infer.run
        & mapStateT
            (either (error "Infer of FromNom on Nominal shouldn't fail") return)
        >>= valueConversionNoSplit loaded empty
    T.TFun argType resType | bodyNot ExprLens._BAbs ->
        if Lens.has (ExprLens.valLeafs . ExprLens._LHole) arg
            then
                -- If the suggested argument has holes in it
                -- then stop suggesting there to avoid "overwhelming"..
                return applied
            else valueConversionNoSplit loaded empty applied
        where
            arg =
                valueNoSplit (Payload argType srcScope)
                & Lens.traversed %~ flip (,) empty
            applied = V.Apply src arg & V.BApp & mkRes resType
    T.TSum composite | bodyNot ExprLens._BInject  ->
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
        srcInferPl = src ^. V.payload . _1
        srcType = srcInferPl ^. Infer.plType
        srcScope = srcInferPl ^. Infer.plScope
        mkRes typ = Val (Payload typ srcScope, empty)
        bodyNot f = Lens.nullOf (V.body . f) src

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
        valueNoSplit (Payload r scope) & V.Lam "var" & V.BAbs
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
    valueNoSplit (pl ^. _1) <&> flip (,) empty & V.payload . _2 .~ (pl ^. _2)
fillHoles empty (Val pl (V.BApp (V.Apply func arg))) =
    -- Dont fill in holes inside apply funcs. This may create redexes..
    fillHoles empty arg & V.Apply func & V.BApp & Val pl
fillHoles empty val = val & V.body . Lens.traversed %~ fillHoles empty
