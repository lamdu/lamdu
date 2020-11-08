-- | Convert applied holes to Fragments

{-# LANGUAGE TypeFamilies, TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Convert.Fragment
    ( convertAppliedHole
      -- Used by Convert.GetVar:
    , fragmentVar
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.ListT (ListT)
import           Control.Monad.Once (OnceT, Typeable)
import           Control.Monad.State (State, runState, StateT(..), mapStateT)
import qualified Control.Monad.State as State
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.List.Class as ListClass
import qualified Data.Property as Property
import           Hyper
import           Hyper.Class.Infer.InferOf (InferOfConstraint, inferOfConstraint)
import           Hyper.Infer (InferResult(..), inferResult, inferUVarsApplyBindings)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Unify (Unify(..), BindingDict(..), unify, applyBindings)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Term (UTerm(..), UTermBody(..))
import           Lamdu.Calc.Infer (InferState, runPureInfer, PureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (iref)
import qualified Lamdu.Sugar.Config as Config
import qualified Lamdu.Sugar.Convert.Expression.Actions as Actions
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
import           Revision.Deltum.Hyper (Write(..))
import           Revision.Deltum.Transaction (Transaction)
import qualified System.Random.Extended as Random

import           Lamdu.Prelude

type T = Transaction

fragmentResultProcessor ::
    Monad m =>
    EntityId -> Ann (Input.Payload m a) # V.Term -> Hole.ResultProcessor m
fragmentResultProcessor topEntityId fragment =
    Hole.ResultProcessor
    { Hole.rpEmptyPl = NotFragment
    , Hole.rpPostProcess = holeResultsEmplaceFragment fragment
    , Hole.rpPreConversion = replaceFragment topEntityId 0
    }

mkOptions ::
    (Monad m, Typeable m) =>
    ConvertM.PositionInfo ->
    ConvertM.Context m ->
    Ann (Input.Payload m a) # V.Term ->
    Ann pl # Term v name i o ->
    Input.Payload m a # V.Term ->
    ConvertM m (OnceT (T m) [HoleOption EvalPrep InternalName (OnceT (T m)) (T m)])
mkOptions posInfo sugarContext argI argS exprPl =
    Hole.mkOptions posInfo (fragmentResultProcessor topEntityId argI) exprPl
    <&> (fragmentOptions <>)
    <&> Lens.mapped %~ Hole.addWithoutDups mkSuggested
    <&> Lens.mapped . Lens.mapped %~ snd
    where
        mkSuggested = mkAppliedHoleSuggesteds sugarContext argI exprPl
        fragmentOptions =
            [ App hole hole & V.BApp & Ann (Const ()) | Lens.nullOf (hVal . _BodyLam) argS ]
            & traverse
                ( \x ->
                    Hole.mkOption sugarContext (fragmentResultProcessor topEntityId argI) exprPl x
                    <&> (,) x
                )
        topEntityId = exprPl ^. Input.stored . iref & EntityId.ofValI
        hole = V.BLeaf V.LHole & Ann (Const ())

mkAppliedHoleSuggesteds ::
    (Monad m, Typeable m) =>
    ConvertM.Context m ->
    Ann (Input.Payload m a) # V.Term ->
    Input.Payload m a # V.Term ->
    [(V.Val (), HoleOption EvalPrep InternalName (OnceT (T m)) (T m))]
mkAppliedHoleSuggesteds sugarContext argI exprPl =
    runStateT
    ( Suggest.termTransforms (exprPl ^. Input.inferScope) (WriteNew :*:) (^. _2)
        ( argI & hflipped %~
            hmap
            ( Proxy @(Recursively (InferOfConstraint HFunctor)) #*#
                \w ->
                withDict (recursively (p0 w)) $
                withDict (inferOfConstraint (Proxy @HFunctor) w)
                onPl
            )
        )
    ) (sugarContext ^. ConvertM.scInferContext)
    <&> onSuggestion
    where
        p0 :: proxy h -> Proxy (InferOfConstraint HFunctor h)
        p0 _ = Proxy
        onPl pl =
            ExistingRef (pl ^. Input.stored . iref) :*:
            (pl ^. Input.inferRes & hflipped %~ hmap (const (^. _2)))
        onSuggestion (sugg, newInferCtx) =
            ( sugg & hflipped %~ hmap (\_ _ -> Const ())
            , mkOptionFromFragment
                (sugarContext & ConvertM.scInferContext .~ newInferCtx)
                exprPl sugg
            )

checkTypeMatch :: Monad m => UVar # T.Type -> UVar # T.Type -> ConvertM m Bool
checkTypeMatch x y =
    Lens.view ConvertM.scInferContext
    <&> check
    <&> Lens.has Lens._Right
    where
        check ctx =
            unify x y >> applyBindings x
            & runPureInfer V.emptyScope ctx

convertAppliedHole ::
    (Monad m, Typeable m, Monoid a) =>
    ConvertM.PositionInfo ->
    V.App V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ExpressionU EvalPrep m a ->
    MaybeT (ConvertM m) (ExpressionU EvalPrep m a)
convertAppliedHole posInfo app@(V.App funcI argI) exprPl argS =
    do
        Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.fragment) >>= guard
        guard (Lens.has ExprLens.valHole funcI)
        do
            isTypeMatch <-
                checkTypeMatch (argI ^. hAnn . Input.inferredTypeUVar)
                (exprPl ^. Input.inferredTypeUVar)
            postProcess <- ConvertM.postProcessAssert
            sugarContext <- Lens.view id
            options <- mkOptions posInfo sugarContext argI argS exprPl
            healMis <- healMismatch
            -- TODO: Check isTypeMatch once in convertAppliedHole
            typeMismatch <-
                if isTypeMatch
                then pure Nothing
                else
                    Actions.makeTypeAnnotation
                        (EntityId.ofFragmentArg (argPl ^. Input.entityId))
                        (argPl ^. Input.inferredType) <&> Just
            BodyFragment Fragment
                { _fExpr =
                    argS
                    & annotation . pActions . detach .~ FragmentExprAlready storedEntityId
                    & annotation . pActions . mSetToHole ?~
                        (DataOps.setToHole stored <* postProcess <&> EntityId.ofValI)
                , _fHeal =
                    ( if isTypeMatch
                        then DataOps.replace stored argIRef <* postProcess
                        else argIRef <$ healMis (stored ^. iref)
                    )
                    <&> EntityId.ofValI
                , _fTypeMismatch = typeMismatch
                , _fOptions = options
                } & pure
            >>= Actions.addActions app exprPl
            & lift
        <&> annotation . pActions . detach .~ FragmentAlready storedEntityId
    where
        argPl = argS ^. annotation . pInput
        argIRef = argI ^. hAnn . Input.stored . iref
        stored = exprPl ^. Input.stored
        storedEntityId = stored ^. iref & EntityId.ofValI

liftPureInfer :: env -> PureInfer env a -> StateT InferState (Either (Pure # T.TypeError)) a
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
    Ann (Input.Payload n a) # V.Term ->
    Ann (Write n :*: InferResult UVar) # V.Term ->
    Hole.ResultGen m (Ann ((Const IsFragment :*: Write n) :*: InferResult UVar) # V.Term)
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
                _ <- unify fragmentType (pl ^. _2 . inferResult)
                -- Perform occurs checks
                -- TODO: Share with occurs check that happens for sugaring?
                t <- State.get
                _ <- inferUVarsApplyBindings x
                -- Roll back state after occurs checks
                fragmentExpr <$ State.put t
                & liftPureInfer ()
                & mapStateT exceptToListT
            , FuncType (pl ^. _2 . inferResult) fragmentType
                & T.TFun
                & UTermBody mempty & UTerm & newVar binding
                & liftPureInfer () & mapStateT exceptToListT
                <&>
                \t ->
                V.App
                (Ann ((Const NotFragment :*: WriteNew) :*: inferResult # t) (V.BLeaf V.LHole))
                fragmentExpr
                & V.BApp & Ann ((Const NotFragment :*: WriteNew) :*: pl ^. _2)
            ]
            & lift
            & join
            & mapStateT (ListClass.take 1)
        fragmentExpr =
            rawFragmentExpr
            & hflipped %~
                hmap
                ( Proxy @(Recursively (InferOfConstraint HFunctor)) #*#
                    \w ->
                    withDict (recursively (p0 w)) $
                    withDict (inferOfConstraint (Proxy @HFunctor) w)
                    onFragmentPayload
                )
        p0 :: proxy h -> Proxy (InferOfConstraint HFunctor h)
        p0 _ = Proxy
        onFragmentPayload pl =
            (Const IsFragment :*: ExistingRef (pl ^. Input.stored . iref))
            :*: (pl ^. Input.inferRes & hflipped %~ hmap (const (^. _2)))
        fragmentType = rawFragmentExpr ^. hAnn . Input.inferredTypeUVar

data IsFragment = IsFragment | NotFragment

markNotFragment ::
    Ann (Write n :*: InferResult UVar) # V.Term ->
    Ann ((Const IsFragment :*: Write n) :*: InferResult UVar) # V.Term
markNotFragment = hflipped %~ hmap (const (_1 %~ (Const NotFragment :*:)))

-- TODO: Unify type according to IsFragment, avoid magic var
fragmentVar :: V.Var
fragmentVar = "HOLE FRAGMENT EXPR"

replaceFragment ::
    EntityId -> Int ->
    Ann (Input.Payload m IsFragment) # V.Term ->
    Ann (Input.Payload m ()) # V.Term
replaceFragment parentEntityId idxInParent (Ann pl bod) =
    case pl ^. Input.userData of
    IsFragment ->
        V.LVar fragmentVar & V.BLeaf
        & Ann (pl & Input.entityId .~ EntityId.ofFragmentUnder idxInParent parentEntityId & Input.userData .~ ())
    NotFragment ->
        htraverse
        ( \case
            HWitness V.W_Term_Term ->
                \x ->
                do
                    i <- Lens.use id
                    id += 1
                    replaceFragment (pl ^. Input.entityId) i x & pure
            HWitness V.W_Term_HCompose_Prune_Type ->
                pure . (hflipped %~ hmap (const (Input.userData .~ ())))
        ) bod
        & (`State.evalState` (0 :: Int))
        & Ann (pl & Input.userData .~ ())

newtype Foo f a h = Foo { getFoo :: f (Ann a h) }

emplaceInHoles ::
    forall f a.
    Applicative f =>
    (a # V.Term -> f (Ann a # V.Term)) ->
    Ann a # V.Term -> [f (Ann a # V.Term)]
emplaceInHoles replaceHole =
    map fst . filter snd . (`runStateT` False) . go
    where
        go :: Ann a # V.Term -> StateT Bool [] (f (Ann a # V.Term))
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
                        V.BApp (V.App (Ann f (V.BLeaf V.LHole)) arg@(Ann _ (V.BLeaf V.LHole))) ->
                            join $ lift
                                [ replace f
                                    <&> fmap (Ann x . V.BApp . (`V.App` arg))
                                , pure (pure oldVal)
                                ]
                        _ ->
                            htraverse
                            ( \case
                                HWitness V.W_Term_Term -> fmap Foo . go
                                HWitness V.W_Term_HCompose_Prune_Type -> pure . Foo . pure
                            ) bod
                            <&> htraverse (const getFoo)
                            <&> Lens.mapped %~ Ann x
        replace x = replaceHole x <$ State.put True

mkResultValFragment ::
    UVar # T.Type ->
    Ann (Write m :*: InferResult UVar) # V.Term ->
    State InferState (Ann ((Const IsFragment :*: Write m) :*: InferResult UVar) # V.Term)
mkResultValFragment inferred x =
    x & hflipped %~ hmap (const onPl)
    & Hole.detachValIfNeeded (Const IsFragment :*: WriteNew) inferred
    where
        onPl (WriteNew :*: i) = (Const NotFragment :*: WriteNew) :*: i
        onPl i = i & _1 %~ (Const IsFragment :*:)

mkOptionFromFragment ::
    (Monad m, Typeable m) =>
    ConvertM.Context m ->
    Input.Payload m a # V.Term ->
    Ann (Write m :*: InferResult UVar) # V.Term ->
    HoleOption EvalPrep InternalName (OnceT (T m)) (T m)
mkOptionFromFragment sugarContext exprPl x =
    HoleOption
    { _hoEntityId =
        x & hflipped %~ hmap (\_ _ -> Const ())
        & show & Random.randFunc
        & EntityId.EntityId
    , _hoSugaredBaseExpr = Hole.sugar sugarContext exprPl baseExpr
    , _hoResults =
        do
            newDeps <- Hole.loadNewDeps (depsProp ^. Property.pVal) (exprPl ^. Input.inferScope) x
            let newSugarContext =
                    sugarContext
                    & ConvertM.scInferContext .~ inferCtx
                    & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
            let updateDeps = (depsProp ^. Property.pSet) newDeps
            pure
                ( resultScore resolved
                , Hole.mkResult (replaceFragment topEntityId 0)
                    updateDeps exprPl result
                    & ConvertM.run newSugarContext & join
                )
            & lift
            <&> pure & ListClass.joinL
    }
    where
        depsProp = sugarContext ^. ConvertM.scFrozenDeps
        (result, inferCtx) =
            runState
            (mkResultValFragment (exprPl ^. Input.inferredTypeUVar) x)
            (sugarContext ^. ConvertM.scInferContext)
        resolved =
            result & hflipped %~ hmap (const ((Const () :*:) . (^. _2)))
            & inferUVarsApplyBindings
            <&> hflipped %~ hmap
                ( Proxy @(Recursively (InferOfConstraint HFunctor)) #*#
                    \w ->
                    withDict (recursively (p0 w)) $
                    withDict (inferOfConstraint (Proxy @HFunctor) w) $
                    (hflipped %~ hmap (const (^. _1))) . (^. _2)
                )
            & runPureInfer () inferCtx
            & Hole.assertSuccessfulInfer
            & fst
        p0 :: proxy h -> Proxy (InferOfConstraint HFunctor h)
        p0 _ = Proxy
        topEntityId = exprPl ^. Input.stored . iref & EntityId.ofValI
        baseExpr = pruneExpr x
        pruneExpr :: Ann (Write m :*: a) # V.Term -> Ann (Const ()) # V.Term
        pruneExpr (Ann (ExistingRef{} :*: _) _) = V.BLeaf V.LHole & Ann (Const ())
        pruneExpr (Ann _ b) =
            hmap
            ( \case
                HWitness V.W_Term_Term -> pruneExpr
                HWitness V.W_Term_HCompose_Prune_Type ->
                    hflipped %~ hmap (\_ _ -> Const ())
            ) b
            & Ann (Const ())
