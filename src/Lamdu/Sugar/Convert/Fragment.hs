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
import           Control.Monad.Once (OnceT, Typeable, MonadOnce(..), onceList)
import           Control.Monad.State (State, runState, StateT(..), mapStateT)
import qualified Control.Monad.State as State
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Functor.Compose (Compose(..))
import qualified Data.List.Class as ListClass
import qualified Data.Property as Property
import           Hyper
import           Hyper.Class.Infer.InferOf (InferOfConstraint, inferOfConstraint)
import           Hyper.Infer (InferResult(..), inferResult, inferUVarsApplyBindings)
import           Hyper.Type.AST.FuncType (FuncType(..))
import qualified Hyper.Type.AST.Nominal as N
import           Hyper.Unify (Unify(..), BindingDict(..), unify, applyBindings)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Term (UTerm(..), UTermBody(..))
import           Lamdu.Calc.Infer (InferState, runPureInfer, PureInfer)
import qualified Lamdu.Calc.Infer as Infer
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Data.Tag (tagOrder)
import           Lamdu.Expr.IRef (iref, readTagData)
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
import qualified Revision.Deltum.Transaction as Transaction

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
    ConvertM m (OnceT (T m) (OptionFilter -> [HoleOption InternalName (OnceT (T m)) (T m)]))
mkOptions posInfo sugarContext argI argS exprPl =
    Hole.mkOptions posInfo (fragmentResultProcessor topEntityId argI) exprPl
    <&> (fragmentOptions <>)
    <&>
    ( \mkOpts ->
        do
            suggesteds <-
                runStateT (mkSuggesteds sugarContext argI exprPl) (sugarContext ^. ConvertM.scInferContext)
                & ListClass.toList
                <&> Lens.mapped %~ fst
            mkOpts <&> Lens.mapped %~ Hole.addWithoutDups suggesteds
    )
    <&> Lens.mapped . Lens.mapped . Lens.mapped %~ snd
    >>= ConvertM.convertOnce
    where
        fragmentOptions =
            [ App hole hole & V.BApp & Ann (Const ()) | Lens.nullOf (hVal . _BodyLam) argS ]
            & traverse
                ( \x ->
                    Hole.mkOption sugarContext (fragmentResultProcessor topEntityId argI) exprPl x
                    <&> (,) x
                )
            <&> const
        topEntityId = exprPl ^. Input.stored . iref & EntityId.ofValI
        hole = V.BLeaf V.LHole & Ann (Const ())

mkSuggesteds ::
    (Monad m, Typeable m) =>
    ConvertM.Context m ->
    Ann (Input.Payload m a) # V.Term ->
    Input.Payload m a # V.Term ->
    StateT InferState (ListT (OnceT (T m)))
        (V.Val (), HoleOption InternalName (OnceT (T m)) (T m))
mkSuggesteds sugarContext argI exprPl =
    do
        deps <-
            argI ^.. hAnn . Input.inferredType . _Pure . T._TInst . N.nId
            & Hole.loadDeps [] & lift & lift & lift
        let srcScope = exprPl ^. Input.inferScope
        let (scope, ctx0) =
                runStateT
                (liftPureInfer srcScope (Infer.loadDeps deps ?? srcScope))
                (sugarContext ^. ConvertM.scInferContext)
                ^?! Lens._Right
        (sugg, ctx1) <-
            runStateT
            ( Suggest.termTransforms ((lift . lift . lift) Hole.genLamVar) scope (WriteNew :*:) (^. _2)
                ( argI & hflipped %~
                    hmap
                    ( Proxy @(Recursively (InferOfConstraint HFunctor)) #*#
                        \w ->
                        withDict (recursively (p0 w)) $
                        withDict (inferOfConstraint (Proxy @HFunctor) w)
                        onPl
                    )
                )
            ) ctx0
        mkSuggestedOptions
            (sugarContext & ConvertM.scInferContext .~ ctx1)
            exprPl sugg
            & lift & lift
            <&> (,) (sugg & hflipped %~ hmap (\_ _ -> Const ()))
    where
        p0 :: proxy h -> Proxy (InferOfConstraint HFunctor h)
        p0 _ = Proxy
        onPl pl =
            ExistingRef (pl ^. Input.stored . iref) :*:
            (pl ^. Input.inferRes & hflipped %~ hmap (const (^. _2)))

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
                    & annotation . pActions . delete .~
                        SetToHole
                        (DataOps.setToHole stored <* postProcess <&> EntityId.ofValI)
                , _fHeal =
                    ( if isTypeMatch
                        then DataOps.replace stored argIRef <* postProcess
                        else argIRef <$ healMis (stored ^. iref)
                    )
                    <&> EntityId.ofValI
                , _fTypeMismatch = typeMismatch
                , _fOptions = Hole options
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

data EmplacePos = EPOther | EPArg V.Var | EPTag T.Tag EmplacePos

emplacePosScore :: Monad m => EmplacePos -> T m Int
emplacePosScore (EPTag tag (EPArg v)) =
    Anchors.assocPresentationMode v & Property.getP >>=
    \case
    Operator l r
        | tag == l -> pure (-2)
        | tag == r -> pure (-1)
    _ -> emplacePosScore (EPTag tag EPOther)
emplacePosScore (EPTag tag _) = readTagData tag <&> (^. tagOrder)
emplacePosScore _ = pure 0

holeResultsEmplaceFragment ::
    Monad m =>
    Ann (Input.Payload n a) # V.Term ->
    Ann (Write n :*: InferResult UVar) # V.Term ->
    Hole.ResultGen m (Ann ((Const IsFragment :*: Write n) :*: InferResult UVar) # V.Term)
holeResultsEmplaceFragment rawFragmentExpr x =
    markNotFragment x
    & emplaceInHoles emplace
    & (traverse . _2) (lift . lift . lift . emplacePosScore)
    <&> ListClass.sortOn snd
    <&> lift . ListClass.fromList . map fst
    >>= join
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

emplaceInHoles ::
    forall f a.
    Applicative f =>
    (a # V.Term -> f (Ann a # V.Term)) ->
    Ann a # V.Term -> [(f (Ann a # V.Term), EmplacePos)]
emplaceInHoles replaceHole =
    ListClass.mapMaybe (_2 id) . (`runStateT` Nothing) . go EPOther
    where
        go :: EmplacePos -> Ann a # V.Term -> StateT (Maybe EmplacePos) [] (f (Ann a # V.Term))
        go ctx oldVal@(Ann x bod) =
            State.get
            >>=
            \case
            Just _ -> pure (pure oldVal)
            Nothing ->
                case bod of
                V.BLeaf V.LHole ->
                    join $ lift
                        [ replaceHole x <$ State.put (Just ctx)
                        , pure (pure oldVal)
                        ]
                V.BApp (V.App f@(Ann _ (V.BLeaf (V.LVar v))) a) ->
                    V.App
                    <$> (go EPOther f <&> Compose)
                    <*> (go (EPArg v) a <&> Compose)
                    <&> V.BApp
                    <&> fin
                V.BRecExtend (V.RowExtend t v r) ->
                    V.RowExtend t
                    <$> (go (EPTag t ctx) v <&> Compose)
                    <*> (go ctx r <&> Compose)
                    <&> V.BRecExtend
                    <&> fin
                _ ->
                    htraverse
                    ( \case
                        HWitness V.W_Term_Term -> fmap Compose . go ctx
                        HWitness V.W_Term_HCompose_Prune_Type -> pure . Compose . pure
                    ) bod
                    <&> fin
            where
                fin = fmap (Ann x) . htraverse (const getCompose)

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

mkSuggestedOptions ::
    (Monad m, Typeable m) =>
    ConvertM.Context m ->
    Input.Payload m a # V.Term ->
    Ann (Write m :*: InferResult UVar) # V.Term ->
    OnceT (T m) (HoleOption InternalName (OnceT (T m)) (T m))
mkSuggestedOptions sugarContext exprPl x =
    HoleOption
    <$> (lift Transaction.newKey <&> EntityId.EntityId)
    <*> once (lift (Hole.mkHoleSearchTerms sugarContext exprPl baseExpr))
    <*> onceList res
    where
        res =
            do
                newDeps <- Hole.loadNewDeps (depsProp ^. Property.pVal) (exprPl ^. Input.inferScope) x & lift
                let newSugarContext =
                        sugarContext
                        & ConvertM.scInferContext .~ inferCtx
                        & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
                let updateDeps = (depsProp ^. Property.pSet) newDeps
                Hole.mkResult (replaceFragment topEntityId 0)
                    updateDeps exprPl result
                    & ConvertM.run newSugarContext
                    <&> (,) (resultScore resolved)
                <&> pure & ListClass.joinL
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
