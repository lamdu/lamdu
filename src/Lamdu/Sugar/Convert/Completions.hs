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
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    UVarOf m # T.Type ->
    m [f (TypedTerm m)]
suggestForType mkVar t =
    -- TODO: DSL for matching/deref'ing UVar structure
    lookupBody t
    >>= \case
    Just (T.TVariant r) -> forVariant mkVar t r
    typ -> suggestForTypeUTermWithoutSplit mkVar typ <&> (^.. Lens._Just)
    <&> Lens.mapped . Lens.mapped %~ Ann (inferResult # t)

forVariant ::
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    UVarOf m # T.Type ->
    UVarOf m # T.Row ->
    m [f (V.Term # Ann (InferResult (UVarOf m)))]
forVariant mkVar t r =
    lookupBody r >>=
    \case
    Just (T.RExtend (RowExtend tag typ rest)) ->
        do
            injType <- FuncType typ t & T.TFun & newTerm
            inj <-
                suggestForTypeObvious mkVar typ
                <&> Lens.mapped %~ V.BApp . V.App (Ann (inferResult # injType) (V.BLeaf (V.LInject tag)))
            forVariant mkVar t rest <&> (inj:)
    _ -> pure []

suggestForTypeObvious ::
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    UVarOf m # T.Type ->
    m (f (TypedTerm m))
suggestForTypeObvious mkVar t =
    lookupBody t
    >>= suggestForTypeUTermObvious mkVar
    <&> fromMaybe (V.BLeaf V.LHole & pure)
    <&> Lens.mapped %~ Ann (inferResult # t)

suggestForTypeUTermWithoutSplit ::
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    Maybe (T.Type # UVarOf m) ->
    m (Maybe (f (V.Term # Ann (InferResult (UVarOf m)))))
suggestForTypeUTermWithoutSplit mkVar (Just (T.TRecord r)) = forRecord mkVar r
suggestForTypeUTermWithoutSplit mkVar t = suggestForTypeUTermObvious mkVar t

suggestForTypeUTermObvious ::
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    Maybe (T.Type # UVarOf m) ->
    m (Maybe (f (V.Term # Ann (InferResult (UVarOf m)))))
suggestForTypeUTermObvious mkVar (Just (T.TFun (FuncType param result))) =
    lookupBody param >>=
    \case
    Just (T.TVariant row) -> suggestCaseWith mkVar row result
    _ -> suggestLam mkVar result
    <&> Just
suggestForTypeUTermObvious _ (Just (T.TRecord r)) =
    lookupBody r <&> forRecordUTermObvious <&> Lens._Just %~ pure
    where
        forRecordUTermObvious (Just T.REmpty) = V.BLeaf V.LRecEmpty & Just
        forRecordUTermObvious _ = Nothing
suggestForTypeUTermObvious _ _ = pure Nothing

suggestLam ::
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    UVarOf m # T.Type ->
    m (f (V.Term # Ann (InferResult (UVarOf m))))
suggestLam mkVar result =
    do
        t <- newUnbound <&> (inferResult #) <&> (`Ann` (_HCompose # Pruned))
        body <- suggestForTypeObvious mkVar result
        V.TypedLam <$> mkVar <*> pure t <*> body
            <&> V.BLam & pure

forRecord ::
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    UVarOf m # T.Row ->
    m (Maybe (f (V.Term # Ann (InferResult (UVarOf m)))))
forRecord mkVar r =
    lookupBody r >>=
    \case
    Just (T.RExtend (RowExtend tag typ rest)) ->
        do
            field <- suggestForTypeObvious mkVar typ
            restType <- newTerm (T.TRecord rest) <&> (inferResult #)
            restBody <- forRecord mkVar rest <&> fromMaybe (pure (V.BLeaf V.LHole))
            RowExtend tag <$> field <*> (restBody <&> Ann restType) <&> V.BRecExtend & pure
        <&> Just
    _ -> V.BLeaf V.LRecEmpty & pure & Just & pure

suggestCaseWith ::
    (Applicative f, UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var ->
    UVarOf m # T.Row ->
    UVarOf m # T.Type ->
    m (f (V.Term # Ann (InferResult (UVarOf m))))
suggestCaseWith mkVar variantType resultType =
    lookupBody variantType >>=
    \case
    Just (T.RExtend (RowExtend tag fieldType rest)) ->
        do
            handlerType <- mkCaseType fieldType
            field <- suggestLam mkVar resultType
            restType <- newTerm (T.TVariant rest) >>= mkCaseType
            restBody <- suggestCaseWith mkVar rest resultType
            RowExtend tag <$> (field <&> Ann handlerType) <*> (restBody <&> Ann restType) <&> V.BCase & pure
        where
            mkCaseType which = FuncType which resultType & T.TFun & newTerm <&> (inferResult #)
    _ -> V.BLeaf V.LAbsurd & pure & pure
