-- | Convert applied holes to Fragments

{-# LANGUAGE TypeFamilies, TupleSections #-}

module Lamdu.Sugar.Convert.Fragment
    ( convertAppliedHole
      -- Used by Convert.GetVar:
    , fragmentVar
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.ListT (ListT)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State (State, runState, StateT(..), mapStateT)
import qualified Control.Monad.State as State
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.List.Class as ListClass
import qualified Data.Property as Property
import           Hyper
import           Hyper.Infer (InferResult, inferResult)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.Functor (F)
import           Hyper.Unify (Unify(..), BindingDict(..), unify)
import           Hyper.Unify.Apply (applyBindings)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Term (UTerm(..), UTermBody(..))
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Calc.Infer (InferState, runPureInfer, PureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValI, iref)
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
import           Revision.Deltum.IRef (IRef)
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
            [ App hole hole & V.BApp & Ann (Const ()) | Lens.nullOf (hVal . _BodyLam) argS ]
            <&> Hole.mkOption sugarContext
                (fragmentResultProcessor topEntityId argI) exprPl
        topEntityId = exprPl ^. Input.stored . iref & EntityId.ofValI
        hole = V.BLeaf V.LHole & Ann (Const ())

mkAppliedHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Input.Payload m a ->
    [HoleOption InternalName (T m) (T m)]
mkAppliedHoleSuggesteds sugarContext argI exprPl =
    runStateT
    (Suggest.termTransforms Nothing (argI & hflipped . hmapped1 . Lens._Wrapped %~ onPl))
    (sugarContext ^. ConvertM.scInferContext)
    <&> onSuggestion
    where
        onPl pl = (Just (pl ^. Input.stored . iref), pl ^. Input.inferRes & hflipped %~ hmap (const (^. Lens._2)))
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
    Annotated (Input.Payload m a) (V.App V.Term) ->
    ExpressionU m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedHole posInfo (Ann (Const exprPl) (V.App funcI argI)) argS =
    do
        Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.fragment) >>= guard
        guard (Lens.has ExprLens.valHole funcI)
        do
            isTypeMatch <-
                checkTypeMatch (argI ^. annotation . Input.inferRes . inferResult . Lens._2)
                (exprPl ^. Input.inferRes . inferResult . Lens._2)
            postProcess <- ConvertM.postProcessAssert
            sugarContext <- Lens.view id
            let showAnn
                    | sugarContext ^. ConvertM.scConfig . Config.showAllAnnotations = alwaysShowAnnotations
                    | otherwise = neverShowAnnotations
            options <-
                argS
                & hflipped %~ hmap (const (Lens._Wrapped %~ (,) showAnn))
                & hflipped (htraverse (const (Lens._Wrapped convertPayload)))
                >>= (mkOptions posInfo sugarContext argI ?? exprPl)
                & Reader.local (ConvertM.scAnnotationsMode .~ Annotations.None)
            healMis <- healMismatch
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
                , _fTypeMatch = isTypeMatch
                , _fOptions = options
                } & pure
            >>= addActions [funcI, argI] exprPl
            & lift
    <&> annotation . pActions . detach .~ FragmentAlready storedEntityId
    where
        argIRef = argI ^. annotation . Input.stored . iref
        stored = exprPl ^. Input.stored
        storedEntityId = stored ^. iref & EntityId.ofValI

liftPureInfer :: env -> PureInfer env a -> StateT InferState (Either (Tree Pure T.TypeError)) a
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
    Val (Input.Payload n a) ->
    Val (Maybe (ValI n), Tree (InferResult UVar) V.Term) ->
    Hole.ResultGen m (Val ((Maybe (ValI n), IsFragment), Tree (InferResult UVar) V.Term))
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
                _ <- (hflipped . htraverse1 . Lens._Wrapped) (applyBindings . (^. _2 . inferResult)) x
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
                (Ann (Const ((Nothing, NotFragment), inferResult # t)) (V.BLeaf V.LHole))
                fragmentExpr
                & V.BApp & Ann (Const ((Nothing, NotFragment), snd pl))
            ]
            & lift
            & join
            & mapStateT (ListClass.take 1)
        fragmentExpr = rawFragmentExpr & hflipped . hmapped1 . Lens._Wrapped %~ onFragmentPayload
        onFragmentPayload pl =
            ( (Just (pl ^. Input.stored . iref), IsFragment)
            , pl ^. Input.inferRes & inferResult %~ (^. _2)
            )
        fragmentType = rawFragmentExpr ^. annotation . Input.inferRes . inferResult . _2
data IsFragment = IsFragment | NotFragment

markNotFragment ::
    Val (Maybe (ValI n), Tree (InferResult UVar) V.Term) ->
    Val ((Maybe (ValI n), IsFragment), Tree (InferResult UVar) V.Term)
markNotFragment = hflipped . hmapped1 . Lens._Wrapped . Lens._1 %~ (, NotFragment)

-- TODO: Unify type according to IsFragment, avoid magic var
fragmentVar :: V.Var
fragmentVar = "HOLE FRAGMENT EXPR"

replaceFragment ::
    EntityId -> Int -> Val (Input.Payload m IsFragment) -> Val (Input.Payload m ())
replaceFragment parentEntityId idxInParent (Ann (Const pl) bod) =
    case pl ^. Input.userData of
    IsFragment ->
        V.LVar fragmentVar & V.BLeaf
        & Ann (Const (pl & Input.entityId .~ EntityId.ofFragmentUnder idxInParent parentEntityId & Input.userData .~ ()))
    NotFragment ->
        bod & Lens.indexing htraverse1 %@~ replaceFragment (pl ^. Input.entityId)
        & Ann (Const (pl & Input.userData .~ ()))

emplaceInHoles :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
emplaceInHoles replaceHole =
    map fst . filter snd . (`runStateT` False) . go
    where
        go oldVal@(Ann (Const x) bod) =
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
                        V.BApp (V.App (Ann (Const f) (V.BLeaf V.LHole)) arg@(Ann _ (V.BLeaf V.LHole))) ->
                            join $ lift
                                [ replace f
                                    <&> fmap (Ann (Const x) . V.BApp . (`V.App` arg))
                                , pure (pure oldVal)
                                ]
                        _ ->
                            htraverse1 (fmap Lens.Const . go) bod
                            <&> Lens.sequenceAOf (htraverse1 . Lens._Wrapped)
                            <&> Lens.mapped . htraverse1 %~ (^. Lens._Wrapped)
                            <&> Lens.mapped %~ Ann (Const x)
        replace x = replaceHole x <$ State.put True

mkResultValFragment ::
    Tree UVar T.Type ->
    Val (Maybe (Tree (F (IRef m)) V.Term), Tree (InferResult UVar) V.Term) ->
    State InferState (Val ((Maybe (ValI m), IsFragment), Tree (InferResult UVar) V.Term))
mkResultValFragment inferred x =
    x & hflipped . hmapped1 . Lens._Wrapped . _1 %~ onPl
    & Hole.detachValIfNeeded emptyPl inferred
    where
        emptyPl = (Nothing, NotFragment)
        onPl Nothing = emptyPl
        onPl (Just inputPl) = (Just inputPl, IsFragment)

mkOptionFromFragment ::
    Monad m =>
    ConvertM.Context m ->
    Input.Payload m a ->
    Val (Maybe (Tree (F (IRef m)) V.Term), Tree (InferResult UVar) V.Term) ->
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
            (mkResultValFragment (exprPl ^. Input.inferRes . inferResult . _2) x)
            (sugarContext ^. ConvertM.scInferContext)
        resolved =
            (hflipped . htraverse1 . Lens._Wrapped) (applyBindings . (^. _2 . inferResult)) result
            & runPureInfer scope inferContext
            & Hole.assertSuccessfulInfer
            & fst
        scope = exprPl ^. Input.inferScope
        topEntityId = exprPl ^. Input.stored . iref & EntityId.ofValI
        baseExpr = pruneExpr x
        pruneExpr (Ann (Const (Just{}, _)) _) = V.BLeaf V.LHole & Ann (Const ())
        pruneExpr (Ann _ b) = b & htraverse1 %~ pruneExpr & Ann (Const ())
