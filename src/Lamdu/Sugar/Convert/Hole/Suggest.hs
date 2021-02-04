{-# LANGUAGE TypeFamilies, TypeApplications, RankNTypes #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( termTransforms
    , termTransformsWithModify
    ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Monad (MonadPlus)
import           Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import           Data.Functor (($>))
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
    MonadPlus m =>
    m V.Var ->
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState m (Ann a # V.Term)
termTransforms mkVar srcScope mkPl getInferred src =
    lookupBody srcType & liftInfer ()
    <&> (^? Lens._Just . T._TRecord)
    >>=
    \case
    Just row | Lens.nullOf (hVal . V._BRecExtend) src ->
        transformGetFields mkPl src srcType row
    _ -> termTransformsWithoutSplit mkVar srcScope mkPl getInferred src
    where
        srcType = getInferred (src ^. hAnn) ^. inferResult

transformGetFields ::
    MonadPlus m =>
    (InferResult UVar # V.Term -> a # V.Term) ->
    Ann a # V.Term -> UVar # T.Type -> UVar # T.Row ->
    StateT InferState m (Ann a # V.Term)
transformGetFields mkPl src fullTyp row =
    lookupBody row & liftInfer ()
    <&> (^? Lens._Just . T._RExtend)
    >>=
    \case
    Nothing -> empty
    Just (RowExtend tag typ rest) ->
        ( FuncType fullTyp typ & T.TFun & newTerm & liftInfer (V.emptyScope @UVar)
            <&> (inferResult #) <&> mkPl <&> (`Ann` V.BLeaf (V.LGetField tag))
            <&> (`V.App` src) <&> V.BApp <&> Ann (mkPl (inferResult # typ))
        ) <|> transformGetFields mkPl src fullTyp rest

liftInfer :: MonadPlus m => env -> PureInfer env a -> StateT InferState m a
liftInfer e act =
    do
        s <- State.get
        case runPureInfer e s act of
            Left{} -> empty
            Right (r, newState) -> r <$ State.put newState

termTransformsWithoutSplit ::
    MonadPlus m =>
    m V.Var ->
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState m (Ann a # V.Term)
termTransformsWithoutSplit mkVar srcScope mkPl getInferred src =
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
                    >>= termOptionalTransformsWithoutSplit mkVar srcScope mkPl getInferred
            Just (T.TVariant row) | Lens.nullOf (hVal . V._BApp . V.appFunc . hVal . V._BLeaf . V._LInject) src ->
                do
                    dstType <- newUnbound
                    caseType <- FuncType s1 dstType & T.TFun & newTerm
                    suggestCaseWith mkVar row dstType
                        <&> Lens.mapped %~
                            mkResult dstType . V.BApp . (`V.App` src) .
                            (hflipped %~ hmap (const mkPl)) . Ann (inferResult # caseType)
                & liftInfer (V.emptyScope @UVar)
                >>= lift
            _ | Lens.nullOf (hVal . V._BLam) src ->
                -- Apply if compatible with a function
                do
                    argType <- liftInfer (V.emptyScope @UVar) newUnbound
                    resType <- liftInfer (V.emptyScope @UVar) newUnbound
                    _ <-
                        FuncType argType resType & T.TFun & newTerm
                        >>= unify s1
                        & liftInfer (V.emptyScope @UVar)
                    let argHole = V.BLeaf V.LHole & Ann (mkPl (inferResult # argType)) & pure & pure
                    arg <-
                        case src ^? hVal . V._BLeaf of
                        Just V.LInject{} -> argHole
                        Just V.LGetField{} -> argHole
                        _ ->
                            forTypeWithoutSplit mkVar argType & liftInfer (V.emptyScope @UVar)
                            <&> Lens.mapped . hflipped %~ hmap (const mkPl)
                    let applied = arg <&> V.App src <&> V.BApp <&> mkResult resType
                    lift applied
                        <|>
                        do
                            -- If the suggested argument has holes in it
                            -- then stop suggesting there to avoid "overwhelming"..
                            arg >>= guard . Lens.nullOf (ExprLens.valLeafs . V._LHole) & lift
                            lift applied >>= termTransformsWithoutSplit mkVar srcScope mkPl getInferred
            _ -> empty
    where
        mkResult t = Ann (mkPl (inferResult # t))

termOptionalTransformsWithoutSplit ::
    MonadPlus m =>
    m V.Var ->
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState m (Ann a # V.Term)
termOptionalTransformsWithoutSplit mkVar srcScope mkPl getInferred src =
    pure src <|>
    termTransformsWithoutSplit mkVar srcScope mkPl getInferred src

forTypeWithoutSplit ::
    Applicative f =>
    (UnifyGen m T.Type, UnifyGen m T.Row) =>
    f V.Var -> UVarOf m # T.Type -> m (f (TypedTerm m))
forTypeWithoutSplit mkVar t =
    lookupBody t
    >>= suggestForTypeUTermWithoutSplit mkVar
    <&> fromMaybe (pure (V.BLeaf V.LHole))
    <&> Lens.mapped %~ Ann (inferResult # t)

fillHoles ::
    MonadPlus m =>
    m V.Var ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState m (Ann a # V.Term)
fillHoles mkVar mkPl getInferred (Ann pl (V.BLeaf V.LHole)) =
    suggestForTypeObvious @_ @(PureInfer (V.Scope # UVar)) mkVar (getInferred pl ^. inferResult)
    & liftInfer V.emptyScope
    >>= lift
    <&> hflipped %~ hmap (const mkPl)
fillHoles _ _ _ v@(Ann _ (V.BApp (V.App (Ann _ (V.BLeaf V.LGetField{})) (Ann _ (V.BLeaf V.LHole))))) =
    -- Dont fill in holes inside get-field.
    pure v
fillHoles mkVar mkPl getInferred (Ann pl (V.BApp (V.App func arg))) =
    -- Dont fill in holes inside apply funcs. This may create redexes..
    fillHoles mkVar mkPl getInferred arg <&> Ann pl . V.BApp . V.App func
fillHoles mkVar mkPl getInferred x =
    hVal
    ( htraverse $
        \case
        HWitness V.W_Term_Term -> fillHoles mkVar mkPl getInferred
        _ -> pure
    ) x

-- | Transform by wrapping OR modifying a term. Used by both holes and
-- fragments to expand "seed" terms. Holes include these as results
-- whereas fragments emplace their content inside holes of these
-- results.
termTransformsWithModify ::
    MonadPlus m =>
    m V.Var ->
    V.Scope # UVar ->
    (forall n. InferResult UVar # n -> a # n) ->
    (a # V.Term -> InferResult UVar # V.Term) ->
    Ann a # V.Term ->
    StateT InferState m (Ann a # V.Term)
termTransformsWithModify _ _ _ _ v@(Ann _ V.BLam {}) = pure v -- Avoid creating a surprise redex
termTransformsWithModify _ srcScope _ getInferred
    v@(Ann pl0 (V.BApp (V.App i@(Ann _ (V.BLeaf V.LInject{})) (Ann pl1 (V.BLeaf V.LHole))))) =
    getInferred pl1 ^. inferResult & lookupBody & liftInfer ()
    >>=
    \case
    Just (T.TRecord t) ->
        liftInfer srcScope (newTerm T.REmpty >>= unify t) $> Ann pl0 (V.BApp (V.App i (Ann pl1 (V.BLeaf V.LRecEmpty))))
        <|> pure v
    _ -> pure v -- this shouldn't happend
termTransformsWithModify mkVar srcScope mkPl getInferred src =
    getInferred (src ^. hAnn) ^. inferResult & lookupBody & liftInfer ()
    >>=
    \case
    Just T.TRecord{} | Lens.has ExprLens.valVar src ->
        -- A "params record" (or just a let item which is a record..)
        pure src
    _ ->
        do
            t <- fillHoles mkVar mkPl getInferred src
            pure t <|> termTransforms mkVar srcScope mkPl getInferred t
