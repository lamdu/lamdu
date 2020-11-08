-- | Common completions for holes and fragments

{-# LANGUAGE TypeFamilies #-}
module Lamdu.Sugar.Convert.Completions
    ( suggestForType
    , suggestForTypeObvious, suggestForTypeUTermWithoutSplit, suggestCaseWith
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Infer (InferResult, inferResult)
import           Hyper.Type.AST.FuncType
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Type.Prune
import           Hyper.Unify
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
suggestForType ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Type -> m [TypedTerm m]
suggestForType t =
    -- TODO: DSL for matching/deref'ing UVar structure
    lookupBody t
    >>= \case
    Just (T.TVariant r) -> forVariant r
    typ -> suggestForTypeUTermWithoutSplit typ <&> (^.. Lens._Just)
    <&> Lens.mapped %~ Ann (inferResult # t)

forVariant ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Row -> m [V.Term # Ann (InferResult (UVarOf m))]
forVariant r =
    lookupBody r >>=
    \case
    Just (T.RExtend (RowExtend tag typ rest)) ->
        (:)
        <$> (suggestForTypeObvious typ <&> V.Inject tag <&> V.BInject)
        <*> forVariant rest
    _ -> pure []

suggestForTypeObvious ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Type -> m (TypedTerm m)
suggestForTypeObvious t =
    lookupBody t
    >>= suggestForTypeUTermObvious
    <&> fromMaybe (V.BLeaf V.LHole)
    <&> Ann (inferResult # t)

suggestForTypeUTermWithoutSplit ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    Maybe (T.Type # UVarOf m) -> m (Maybe (V.Term # Ann (InferResult (UVarOf m))))
suggestForTypeUTermWithoutSplit (Just (T.TRecord r)) = forRecord r
suggestForTypeUTermWithoutSplit t = suggestForTypeUTermObvious t

suggestForTypeUTermObvious ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    Maybe (T.Type # UVarOf m) -> m (Maybe (V.Term # Ann (InferResult (UVarOf m))))
suggestForTypeUTermObvious (Just (T.TFun (FuncType param result))) =
    lookupBody param >>=
    \case
    Just (T.TVariant row) -> suggestCaseWith row result
    _ -> suggestLam result
    <&> Just
suggestForTypeUTermObvious (Just (T.TRecord r)) =
    lookupBody r <&> forRecordUTermObvious
suggestForTypeUTermObvious _ = pure Nothing

suggestLam ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Type ->
    m (V.Term # Ann (InferResult (UVarOf m)))
suggestLam result =
    V.TypedLam "var"
    <$> (newUnbound <&> (inferResult #) <&> (`Ann` (_HCompose # Pruned)))
    <*> suggestForTypeObvious result
    <&> V.BLam

forRecord ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Row -> m (Maybe (V.Term # Ann (InferResult (UVarOf m))))
forRecord r =
    lookupBody r >>=
    \case
    Just (T.RExtend (RowExtend tag typ rest)) ->
        RowExtend tag
        <$> suggestForTypeObvious typ
        <*> ( Ann
                <$> (newTerm (T.TRecord rest) <&> (inferResult #))
                <*> (forRecord rest <&> fromMaybe (V.BLeaf V.LHole))
            )
        <&> V.BRecExtend
        <&> Just
    t -> forRecordUTermObvious t & pure

forRecordUTermObvious :: Maybe (T.Row # h0) -> Maybe (V.Term # h1)
forRecordUTermObvious (Just T.REmpty) = V.BLeaf V.LRecEmpty & Just
forRecordUTermObvious _ = Nothing

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
                    <*> suggestForTypeObvious resultType
                    <&> V.BLam
                    )
            )
        <*> (Ann
                <$> (T.TVariant rest & newTerm >>= mkCaseType <&> (inferResult #))
                <*> suggestCaseWith rest resultType)
        <&> V.BCase
        where
            mkCaseType which = FuncType which resultType & T.TFun & newTerm
    _ -> suggestLam resultType
