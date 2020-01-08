-- | Common completions for holes and fragments

{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Lamdu.Sugar.Convert.Completions
    ( forType
    , forTypeObvious, forTypeUTermWithoutSplit, suggestCaseWith
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Infer (InferResult, inferResult)
import           Hyper.Type.AST.FuncType
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Type.Prune
import           Hyper.Unify
import           Hyper.Unify.Lookup (semiPruneLookup)
import           Hyper.Unify.New (newUnbound, newTerm)
import           Hyper.Unify.Term
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

-- | Term with unifiable type annotations
type TypedTerm m = Ann (InferResult (UVarOf m)) # V.Term

lookupBody :: Unify f t => UVarOf f # t -> f (Maybe (t # UVarOf f))
lookupBody x = semiPruneLookup x <&> (^? _2 . _UTerm . uBody)

-- | Suggest values that fit a type, may "split" once, to suggest many
-- injects for a sum type. These are offerred in holes (not fragments).
forType ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Type -> m [TypedTerm m]
forType t =
    -- TODO: DSL for matching/deref'ing UVar structure
    lookupBody t
    >>= \case
    Just (T.TVariant r) -> forVariant r
    typ -> forTypeUTermWithoutSplit typ <&> (^.. Lens._Just)
    <&> Lens.mapped %~ Ann (inferResult # t)

forVariant ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Row -> m [V.Term # Ann (InferResult (UVarOf m))]
forVariant r =
    lookupBody r >>=
    \case
    Just (T.RExtend (RowExtend tag typ rest)) ->
        (:)
        <$> (forTypeObvious typ <&> V.Inject tag <&> V.BInject)
        <*> forVariant rest
    _ -> pure []

forTypeObvious ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Type -> m (TypedTerm m)
forTypeObvious t =
    lookupBody t
    >>= forTypeUTermObvious
    <&> fromMaybe (V.BLeaf V.LHole)
    <&> Ann (inferResult # t)

forTypeUTermWithoutSplit ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    Maybe (T.Type # UVarOf m) -> m (Maybe (V.Term # Ann (InferResult (UVarOf m))))
forTypeUTermWithoutSplit (Just (T.TRecord r)) = forRecord r
forTypeUTermWithoutSplit t = forTypeUTermObvious t

forTypeUTermObvious ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    Maybe (T.Type # UVarOf m) -> m (Maybe (V.Term # Ann (InferResult (UVarOf m))))
forTypeUTermObvious (Just (T.TFun (FuncType param result))) =
    lookupBody param >>=
    \case
    Just (T.TVariant row) -> suggestCaseWith row result
    _ -> suggestLam result
    <&> Just
forTypeUTermObvious _ = pure Nothing

suggestLam ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Type ->
    m (V.Term # Ann (InferResult (UVarOf m)))
suggestLam result =
    V.TypedLam "var"
    <$> (newUnbound <&> (inferResult #) <&> (`Ann` (_HCompose # Pruned)))
    <*> forTypeObvious result
    <&> V.BLam

forRecord ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Row -> m (Maybe (V.Term # Ann (InferResult (UVarOf m))))
forRecord r =
    lookupBody r >>=
    \case
    Just T.REmpty -> V.BLeaf V.LRecEmpty & Just & pure
    Just (T.RExtend (RowExtend tag typ rest)) ->
        RowExtend tag
        <$> forTypeObvious typ
        <*> ( Ann
                <$> (newTerm (T.TRecord rest) <&> (inferResult #))
                <*> (forRecord rest <&> fromMaybe (V.BLeaf V.LHole))
            )
        <&> V.BRecExtend
        <&> Just
    _ -> pure Nothing

suggestCaseWith ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Row -> UVarOf m # T.Type ->
    m (V.Term # Ann (InferResult (UVarOf m)))
suggestCaseWith variantType resultType =
    lookupBody variantType >>=
    \case
    Just T.REmpty -> V.BLeaf V.LAbsurd & pure
    Just (T.RExtend (RowExtend tag fieldType rest)) ->
        RowExtend tag
        <$> (Ann
                <$> (mkCaseType fieldType <&> (inferResult #))
                <*> (V.TypedLam "var"
                    <$> (newUnbound <&> (inferResult #) <&> (`Ann` (_HCompose # Pruned)))
                    <*> forTypeObvious resultType
                    <&> V.BLam
                    )
            )
        <*> (Ann
                <$> (T.TVariant rest & newTerm >>= mkCaseType <&> (inferResult #))
                <*> suggestCaseWith rest resultType)
        <&> V.BCase
        where
            mkCaseType which = FuncType which resultType & T.TFun & newTerm
    _ ->
        -- TODO: Maybe this should be a lambda, like a TFun from non-variant
        V.BLeaf V.LHole & pure
