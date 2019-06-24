-- | Convert applied holes to Fragments

{-# LANGUAGE TypeFamilies #-}

module Lamdu.Sugar.Convert.Fragment
    ( convertAppliedHole
      -- Used by Convert.GetVar:
    , fragmentVar
    ) where

import           AST (Tree, Pure, monoChildren)
import           AST.Infer (IResult(..), irScope, irType)
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import           AST.Term.FuncType (FuncType(..))
import           AST.Unify (unify, applyBindings, newTerm)
import           AST.Unify.Binding (UVar)
import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.ListT (ListT)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State (State, runState, StateT(..), mapStateT)
import qualified Control.Monad.State as State
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.List.Class as ListClass
import qualified Data.Property as Property
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Calc.Infer (InferState, runPureInfer, PureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Pure as P
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Sugar.Annotations (neverShowAnnotations, alwaysShowAnnotations)
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, convertPayload)
import           Lamdu.Sugar.Convert.Fragment.Heal (healMismatch)
import qualified Lamdu.Sugar.Convert.Hole as Hole
import           Lamdu.Sugar.Convert.Hole.ResultScore (resultScore)
import qualified Lamdu.Sugar.Convert.Hole.Suggest as Suggest
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

fragmentResultProcessor ::
    Monad m => EntityId -> Val (Input.Payload m a) -> Hole.ResultProcessor m
fragmentResultProcessor topEntityId fragment =
    Hole.ResultProcessor
    { Hole.rpEmptyPl = NotFragment
    , Hole.rpPostProcess = holeResultsEmplaceFragment fragment
    , Hole.rpPreConversion = replaceFragment topEntityId 0
    }

mkOptions ::
    Monad m =>
    ConvertM.PositionInfo ->
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Expression name i o (Payload name i o a) ->
    Input.Payload m a ->
    ConvertM m (T m [HoleOption InternalName (T m) (T m)])
mkOptions posInfo sugarContext argI argS exprPl =
    Hole.mkOptions posInfo (fragmentResultProcessor topEntityId argI) exprPl
    <&> (pure fragmentOptions <>)
    <&> Lens.mapped %~ Hole.addWithoutDups mkSuggested
    where
        mkSuggested = mkAppliedHoleSuggesteds sugarContext argI exprPl
        fragmentOptions =
            [ P.app P.hole P.hole | Lens.nullOf (val . _BodyLam) argS ]
            <&> Hole.mkOption sugarContext
                (fragmentResultProcessor topEntityId argI) exprPl
        topEntityId = exprPl ^. Input.stored . Property.pVal & EntityId.ofValI

mkAppliedHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Input.Payload m a ->
    [HoleOption InternalName (T m) (T m)]
mkAppliedHoleSuggesteds sugarContext argI exprPl =
    runStateT
    (Suggest.termTransforms Nothing (argI & annotations %~ onPl))
    (sugarContext ^. ConvertM.scInferContext)
    <&> onSuggestion
    where
        onPl pl = (Just pl, pl ^. Input.inferResult)
        onSuggestion (sugg, newInferCtx) =
            mkOptionFromFragment
            (sugarContext & ConvertM.scInferContext .~ newInferCtx)
            exprPl sugg

checkTypeMatch :: Monad m => Tree UVar T.Type -> Tree UVar T.Type -> ConvertM m Bool
checkTypeMatch x y =
    Lens.view ConvertM.scInferContext
    <&> check
    <&> Lens.has Lens._Right
    where
        check ctx =
            unify x y >> applyBindings x
            & runPureInfer V.emptyScope ctx

convertAppliedHole ::
    (Monad m, Monoid a) =>
    ConvertM.PositionInfo ->
    Tree (V.Apply V.Term) (Ann (Input.Payload m a)) ->
    ExpressionU m a ->
    Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedHole posInfo (V.Apply funcI argI) argS exprPl
    | Lens.has ExprLens.valHole funcI =
        do
            isTypeMatch <-
                checkTypeMatch (argI ^. ann . Input.inferResult . irType)
                (exprPl ^. Input.inferResult . irType)
            postProcess <- ConvertM.postProcessAssert
            sugarContext <- Lens.view id
            let showAnn
                    | sugarContext ^. ConvertM.scConfig . Config.showAllAnnotations = alwaysShowAnnotations
                    | otherwise = neverShowAnnotations
            options <-
                argS
                & annotations %~ (,) showAnn
                & annotations convertPayload
                >>= (mkOptions posInfo sugarContext argI ?? exprPl)
                & Reader.local (ConvertM.scAnnotationsMode .~ Annotations.None)
            healMis <- healMismatch
            BodyFragment Fragment
                { _fExpr =
                    argS
                    & ann . pActions . detach .~ FragmentExprAlready storedEntityId
                    & ann . pActions . mSetToHole ?~
                        (DataOps.setToHole stored <* postProcess <&> EntityId.ofValI)
                , _fHeal =
                    ( if isTypeMatch
                        then DataOps.replace stored argIRef <* postProcess
                        else argIRef <$ healMis (stored ^. Property.pVal)
                    )
                    <&> EntityId.ofValI
                , _fTypeMatch = isTypeMatch
                , _fOptions = options
                } & pure
            >>= addActions [funcI, argI] exprPl
        & lift
        <&> ann . pActions . detach .~ FragmentAlready storedEntityId
    | otherwise = empty
    where
        argIRef = argI ^. ann . Input.stored . Property.pVal
        stored = exprPl ^. Input.stored
        storedEntityId = stored & Property.value & EntityId.ofValI

liftPureInfer :: Tree V.Scope UVar -> PureInfer a -> StateT InferState (Either (Tree Pure T.TypeError)) a
liftPureInfer scope act =
    do
        st <- Lens.use id
        case runPureInfer scope st act of
            Left x -> throwError x
            Right (r, newSt) -> r <$ (id .= newSt)

exceptToListT :: Monad m => Either t a -> ListT m a
exceptToListT (Left _) = mempty
exceptToListT (Right x) = pure x

holeResultsEmplaceFragment ::
    Monad m =>
    Val (Input.Payload n a) -> Hole.ResultVal n () ->
    Hole.ResultGen m (Hole.ResultVal n IsFragment)
holeResultsEmplaceFragment rawFragmentExpr x =
    markNotFragment x
    & emplaceInHoles emplace
    & ListClass.fromList
    & lift
    & join
    where
        emplace pl =
            -- Try to emplace the fragmentExpr in directly, but if
            -- that results in a unification type error, fall back to
            -- emplacing another fragment wrapping the fragmentExpr:
            ListClass.fromList
            [ do
                _ <- unify fragmentType (snd pl ^. irType)
                -- Perform occurs checks
                -- TODO: Share with occurs check that happens for sugaring?
                t <- State.get
                _ <- annotations (applyBindings . (^. Input.inferResult . irType)) rawFragmentExpr
                _ <- annotations (applyBindings . (^. _2 . irType)) x
                -- Roll back state after occurs checks
                fragmentExpr <$ State.put t
                & liftPureInfer (snd pl ^. irScope)
                & mapStateT exceptToListT
            , FuncType (snd pl ^. irType) fragmentType & T.TFun & newTerm
                & liftPureInfer (snd pl ^. irScope) & mapStateT exceptToListT
                <&>
                \t ->
                V.Apply
                (Ann ((Nothing, NotFragment), snd pl & irType .~ t) (V.BLeaf V.LHole))
                fragmentExpr
                & V.BApp & Ann ((Nothing, NotFragment), snd pl)
            ]
            & lift
            & join
            & mapStateT (ListClass.take 1)
        fragmentExpr = rawFragmentExpr & annotations %~ onFragmentPayload
        onFragmentPayload pl =
            ( (Just (pl ^. Input.stored . Property.pVal), IsFragment)
            , pl ^. Input.inferResult
            )
        fragmentType = rawFragmentExpr ^. ann . Input.inferResult . irType
data IsFragment = IsFragment | NotFragment

markNotFragment :: Hole.ResultVal n () -> Hole.ResultVal n IsFragment
markNotFragment = annotations %~ _1 . _2 .~ NotFragment

-- TODO: Unify type according to IsFragment, avoid magic var
fragmentVar :: V.Var
fragmentVar = "HOLE FRAGMENT EXPR"

replaceFragment ::
    EntityId -> Int -> Val (Input.Payload m IsFragment) -> Val (Input.Payload m ())
replaceFragment parentEntityId idxInParent (Ann pl bod) =
    case pl ^. Input.userData of
    IsFragment ->
        V.LVar fragmentVar & V.BLeaf
        & Ann (void pl & Input.entityId .~ EntityId.ofFragmentUnder idxInParent parentEntityId)
    NotFragment ->
        bod & Lens.indexing monoChildren %@~ replaceFragment (pl ^. Input.entityId)
        & Ann (void pl)

emplaceInHoles :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
emplaceInHoles replaceHole =
    map fst . filter snd . (`runStateT` False) . go
    where
        go oldVal@(Ann x bod) =
            do
                alreadyReplaced <- State.get
                if alreadyReplaced
                    then pure (pure oldVal)
                    else
                        case bod of
                        V.BLeaf V.LHole ->
                            join $ lift
                                [ replace x
                                , pure (pure oldVal)
                                ]
                        V.BApp (V.Apply (Ann f (V.BLeaf V.LHole)) arg@(Ann _ (V.BLeaf V.LHole))) ->
                            join $ lift
                                [ replace f
                                    <&> fmap (Ann x . V.BApp . (`V.Apply` arg))
                                , pure (pure oldVal)
                                ]
                        _ ->
                            monoChildren (fmap Lens.Const . go) bod
                            <&> Lens.sequenceAOf (monoChildren . Lens._Wrapped)
                            <&> Lens.mapped . monoChildren %~ (^. Lens._Wrapped)
                            <&> Lens.mapped %~ Ann x
        replace x = replaceHole x <$ State.put True

mkResultValFragment ::
    IResult UVar V.Term ->
    Val (Maybe (Input.Payload m a), IResult UVar V.Term) ->
    State InferState (Hole.ResultVal m IsFragment)
mkResultValFragment inferred x =
    x & annotations . _1 %~ onPl
    & Hole.detachValIfNeeded emptyPl inferred
    where
        emptyPl = (Nothing, NotFragment)
        onPl Nothing = emptyPl
        onPl (Just inputPl) = (inputPl ^. Input.stored & Property.value & Just, IsFragment)

mkOptionFromFragment ::
    Monad m =>
    ConvertM.Context m ->
    Input.Payload m a ->
    Val (Maybe (Input.Payload m a), IResult UVar V.Term) ->
    HoleOption InternalName (T m) (T m)
mkOptionFromFragment sugarContext exprPl x =
    HoleOption
    { _hoVal = baseExpr
    , _hoSugaredBaseExpr = Hole.sugar sugarContext exprPl baseExpr
    , _hoResults =
        do
            newDeps <- Hole.loadNewDeps (depsProp ^. Property.pVal) scope x
            let newSugarContext =
                    sugarContext
                    & ConvertM.scInferContext .~ inferContext
                    & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
            let updateDeps = (depsProp ^. Property.pSet) newDeps
            pure
                ( resultScore resolved
                , Hole.mkResult (replaceFragment topEntityId 0) newSugarContext
                    updateDeps exprPl result
                )
            <&> pure & ListClass.joinL
    }
    where
        depsProp = sugarContext ^. ConvertM.scFrozenDeps
        (result, inferContext) =
            runState
            (mkResultValFragment (exprPl ^. Input.inferResult) x)
            (sugarContext ^. ConvertM.scInferContext)
        resolved =
            annotations (applyBindings . (^. Lens._2 . irType)) result
            & runPureInfer scope inferContext
            & Hole.assertSuccessfulInfer
            & fst
        scope = exprPl ^. Input.inferResult . irScope
        topEntityId = exprPl ^. Input.stored . Property.pVal & EntityId.ofValI
        baseExpr = pruneExpr x
        pruneExpr (Ann (Just{}, _) _) = P.hole
        pruneExpr (Ann _ b) = b & monoChildren %~ pruneExpr & Ann ()
