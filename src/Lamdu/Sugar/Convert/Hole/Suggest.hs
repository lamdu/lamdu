{-# LANGUAGE TypeFamilies, TypeApplications, RankNTypes #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( termTransforms
    , termTransformsWithModify
    ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import           Hyper
import           Hyper.Infer (InferResult, inferResult, inferBody)
import           Hyper.Type.AST.FuncType
import           Hyper.Type.AST.Nominal
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Unify
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.New (newUnbound, newTerm)
import           Hyper.Unify.Term
import           Lamdu.Calc.Infer (PureInfer, InferState, runPureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Convert.Completions (suggestForTypeObvious, suggestForTypeUTermWithoutSplit, suggestCaseWith)

import           Lamdu.Prelude

-- | Term with unifiable type annotations
type TypedTerm m = Ann (InferResult (UVarOf m)) # V.Term

lookupBody :: Unify f t => UVarOf f # t -> f (Maybe (t # UVarOf f))
lookupBody x = semiPruneLookup x <&> (^? _2 . _UTerm . uBody)

-- | These are offered in fragments (not holes). They transform a term
-- by wrapping it in a larger term where it appears once.
termTransforms ::
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState [] (Ann a # V.Term)
termTransforms srcScope mkPl getInferred src =
    getInferred (src ^. hAnn) ^. inferResult & lookupBody & liftInfer ()
    <&> (^? Lens._Just . T._TRecord)
    >>=
    \case
    Just row | Lens.nullOf (hVal . V._BRecExtend) src ->
        transformGetFields mkPl src row
    _ -> termTransformsWithoutSplit srcScope mkPl getInferred src

transformGetFields ::
    (InferResult UVar # V.Term -> a # V.Term) ->
    Ann a # V.Term -> UVar # T.Row ->
    StateT InferState [] (Ann a # V.Term)
transformGetFields mkPl src row =
    lookupBody row & liftInfer ()
    <&> (^? Lens._Just . T._RExtend)
    >>=
    \case
    Nothing -> empty
    Just (RowExtend tag typ rest) ->
        pure (Ann (mkPl (inferResult # typ)) (V.BGetField (V.GetField src tag)))
        <|> transformGetFields mkPl src rest

liftInfer :: env -> PureInfer env a -> StateT InferState [] a
liftInfer e act =
    do
        s <- State.get
        case runPureInfer e s act of
            Left{} -> empty
            Right (r, newState) -> r <$ State.put newState

termTransformsWithoutSplit ::
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState [] (Ann a # V.Term)
termTransformsWithoutSplit srcScope mkPl getInferred src =
    do
        -- Don't modify a redex from the outside.
        -- Such transform are more suitable in it!
        Lens.nullOf (hVal . V._BApp . V.appFunc . hVal . V._BLam) src & guard

        (s1, typ) <-
            getInferred (src ^. hAnn) ^. inferResult & semiPruneLookup & liftInfer ()
        case typ ^? _UTerm . uBody of
            Just (T.TInst (NominalInst name _params))
                | Lens.nullOf (hVal . V._BToNom) src ->
                    do
                        fromNomRes <- V.LFromNom name & V.BLeaf & inferBody
                        let fromNomTyp = fromNomRes ^. _2 . _ANode
                        resultType <- newUnbound
                        _ <- FuncType s1 resultType & T.TFun & newTerm >>= unify fromNomTyp
                        V.App (mkResult fromNomTyp (V.BLeaf (V.LFromNom name))) src
                            & V.BApp & mkResult resultType & pure
                    & liftInfer srcScope
                    >>= termOptionalTransformsWithoutSplit srcScope mkPl getInferred
            Just (T.TVariant row) | Lens.nullOf (hVal . V._BInject) src ->
                do
                    dstType <- newUnbound
                    caseType <- FuncType s1 dstType & T.TFun & newTerm
                    suggestCaseWith row dstType
                        <&> Ann (inferResult # caseType)
                        <&> hflipped %~ hmap (const mkPl)
                        <&> (`V.App` src) <&> V.BApp <&> mkResult dstType
                & liftInfer (V.emptyScope @UVar)
            _ | Lens.nullOf (hVal . V._BLam) src ->
                -- Apply if compatible with a function
                do
                    argType <- liftInfer (V.emptyScope @UVar) newUnbound
                    resType <- liftInfer (V.emptyScope @UVar) newUnbound
                    _ <-
                        FuncType argType resType & T.TFun & newTerm
                        >>= unify s1
                        & liftInfer (V.emptyScope @UVar)
                    arg <-
                        forTypeWithoutSplit argType & liftInfer (V.emptyScope @UVar)
                        <&> hflipped %~ hmap (const mkPl)
                    let applied = V.App src arg & V.BApp & mkResult resType
                    pure applied
                        <|>
                        do
                            -- If the suggested argument has holes in it
                            -- then stop suggesting there to avoid "overwhelming"..
                            Lens.nullOf (ExprLens.valLeafs . V._LHole) arg & guard
                            termTransformsWithoutSplit srcScope mkPl getInferred applied
            _ -> empty
    where
        mkResult t = Ann (mkPl (inferResult # t))

termOptionalTransformsWithoutSplit ::
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState [] (Ann a # V.Term)
termOptionalTransformsWithoutSplit srcScope mkPl getInferred src =
    pure src <|>
    termTransformsWithoutSplit srcScope mkPl getInferred src

forTypeWithoutSplit ::
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    UVarOf m # T.Type -> m (TypedTerm m)
forTypeWithoutSplit t =
    lookupBody t
    >>= suggestForTypeUTermWithoutSplit
    <&> fromMaybe (V.BLeaf V.LHole)
    <&> Ann (inferResult # t)

fillHoles ::
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    PureInfer (V.Scope # UVar) (Ann a # V.Term)
fillHoles mkPl getInferred (Ann pl (V.BLeaf V.LHole)) =
    suggestForTypeObvious (getInferred pl ^. inferResult)
    <&> hflipped %~ hmap (const mkPl)
fillHoles mkPl getInferred (Ann pl (V.BApp (V.App func arg))) =
    -- Dont fill in holes inside apply funcs. This may create redexes..
    fillHoles mkPl getInferred arg <&> V.App func <&> V.BApp <&> Ann pl
fillHoles _ _ v@(Ann _ (V.BGetField (V.GetField (Ann _ (V.BLeaf V.LHole)) _))) =
    -- Dont fill in holes inside get-field.
    pure v
fillHoles mkPl getInferred x =
    hVal
    ( htraverse $
        \case
        HWitness V.W_Term_Term -> fillHoles mkPl getInferred
        _ -> pure
    ) x

-- | Transform by wrapping OR modifying a term. Used by both holes and
-- fragments to expand "seed" terms. Holes include these as results
-- whereas fragments emplace their content inside holes of these
-- results.
termTransformsWithModify ::
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState [] (Ann a # V.Term)
termTransformsWithModify _ _ _ v@(Ann _ V.BLam {}) = pure v -- Avoid creating a surprise redex
termTransformsWithModify _ _ getInferred v@(Ann pl0 (V.BInject (V.Inject tag (Ann pl1 (V.BLeaf V.LHole))))) =
    getInferred pl1 ^. inferResult & lookupBody & liftInfer ()
    >>=
    \case
    Just (T.TRecord r) ->
        lookupBody r & liftInfer ()
        >>=
        \case
        Just T.REmpty ->
            -- Variant:<hole> ==> Variant.
            pure (Ann pl0 (V.BInject (V.Inject tag (Ann pl1 (V.BLeaf V.LRecEmpty)))))
            <|> pure v
        _ -> pure v
    _ -> pure v
termTransformsWithModify srcScope mkPl getInferred src =
    getInferred (src ^. hAnn) ^. inferResult & lookupBody & liftInfer ()
    >>=
    \case
    Just T.TRecord{} | Lens.has ExprLens.valVar src ->
        -- A "params record" (or just a let item which is a record..)
        pure src
    _ ->
        do
            t <- fillHoles mkPl getInferred src & liftInfer V.emptyScope
            pure t <|> termTransforms srcScope mkPl getInferred t
