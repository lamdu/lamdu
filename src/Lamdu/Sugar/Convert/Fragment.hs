-- | Convert applied holes to Fragments

module Lamdu.Sugar.Convert.Fragment
    ( convertAppliedHole
    , mkOptionFromFragment
      -- Used by Convert.GetVar:
    , fragmentVar
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (StateT(..), mapStateT, evalStateT)
import qualified Control.Monad.Trans.State as State
import qualified Data.List.Class as ListClass
import qualified Data.Property as Property
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
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
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Expression name i o a ->
    Input.Payload m a ->
    ConvertM m (T m [HoleOption' (T m) (Expression InternalName (T m) (T m) ())])
mkOptions sugarContext argI argS exprPl =
    Hole.mkOptions (fragmentResultProcessor topEntityId argI) exprPl
    <&> (<> pure fragmentOptions)
    <&> (\mkOpts -> Hole.addWithoutDups <$> mkOpts <*> mkSuggested)
    where
        mkSuggested = mkAppliedHoleSuggesteds sugarContext argI exprPl
        fragmentOptions =
            [ P.app P.hole P.hole | Lens.nullOf (body . _BodyLam) argS ]
            <&> Hole.SeedExpr
            <&> Hole.mkOption sugarContext
                (fragmentResultProcessor topEntityId argI) exprPl
        topEntityId = exprPl ^. Input.stored . Property.pVal & EntityId.ofValI

mkAppliedHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Input.Payload m a ->
    T m [HoleOption' (T m) (Expression InternalName (T m) (T m) ())]
mkAppliedHoleSuggesteds sugarContext argI exprPl =
    Suggest.valueConversion Load.nominal Nothing (argI <&> onPl)
    <&> (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    <&> Lens.mapped %~ onSuggestion
    where
        onPl pl = (pl ^. Input.inferred, Just pl)
        onSuggestion (sugg, newInferCtx) =
            mkOptionFromFragment
            (sugarContext & ConvertM.scInferContext .~ newInferCtx)
            exprPl (sugg <&> _1 %~ (^. Infer.plType))

checkTypeMatch :: Monad m => T.Type -> T.Type -> ConvertM m Bool
checkTypeMatch x y =
    Lens.view ConvertM.scInferContext
    <&> evalStateT (Infer.run (unify x y))
    <&> Lens.has Lens._Right

convertAppliedHole ::
    (Monad m, Monoid a) =>
    V.Apply (Val (Input.Payload m a)) -> ExpressionU m a ->
    Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedHole (V.Apply funcI argI) argS exprPl =
    do
        guard $ Lens.has ExprLens.valHole funcI
        isTypeMatch <-
            checkTypeMatch (argI ^. Val.payload . Input.inferredType)
            (exprPl ^. Input.inferredType) & lift
        postProcess <- lift ConvertM.postProcessAssert
        do
            sugarContext <- Lens.view id
            options <- mkOptions sugarContext argI (argS <&> (^. pUserData)) exprPl
            BodyFragment Fragment
                { _fExpr =
                      argS
                      & rPayload . plActions . detach .~ FragmentExprAlready storedEntityId
                      & rPayload . plActions . mSetToHole ?~
                        (DataOps.setToHole stored <* postProcess <&> EntityId.ofValI)
                , _fHeal =
                      if isTypeMatch
                      then DataOps.replace stored
                           (argI ^. Val.payload . Input.stored . Property.pVal)
                           <* postProcess
                           <&> EntityId.ofValI
                           & HealAction
                      else TypeMismatch
                , _fOptions = options
                } & pure
            >>= addActions [funcI, argI] exprPl
            & lift
            <&> rPayload . plActions . detach .~ FragmentAlready storedEntityId
    where
        stored = exprPl ^. Input.stored
        storedEntityId = stored & Property.value & EntityId.ofValI

exceptToListT :: Monad m => Either t a -> ListT m a
exceptToListT (Left _) = mempty
exceptToListT (Right x) = pure x

holeResultsEmplaceFragment ::
    Monad m =>
    Val (Input.Payload n a) -> Hole.ResultVal n () ->
    Hole.ResultGen m (Hole.ResultVal n IsFragment)
holeResultsEmplaceFragment rawFragmentExpr val =
    markNotFragment val
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
            [ fragmentExpr
              <$ (mapStateT exceptToListT . Infer.run . unify fragmentType)
                  (fst pl ^. Infer.plType)
            , V.Apply
                (Val (fst pl & Infer.plType %~ (`T.TFun` fragmentType), (Nothing, NotFragment)) (V.BLeaf V.LHole))
                fragmentExpr
                & V.BApp & Val (fst pl, (Nothing, NotFragment))
                & pure
            ]
            & lift
            & join
            & mapStateT (ListClass.take 1)
        fragmentExpr = rawFragmentExpr <&> onFragmentPayload
        onFragmentPayload pl =
            ( pl ^. Input.inferred
            , (Just (pl ^. Input.stored . Property.pVal), IsFragment)
            )
        fragmentType = rawFragmentExpr ^. Val.payload . Input.inferredType
data IsFragment = IsFragment | NotFragment

markNotFragment :: Hole.ResultVal n () -> Hole.ResultVal n IsFragment
markNotFragment val = val <&> _2 . _2 .~ NotFragment

-- TODO: Unify type according to IsFragment, avoid magic var
fragmentVar :: V.Var
fragmentVar = "HOLE FRAGMENT EXPR"

replaceFragment :: EntityId -> Int -> Val (Input.Payload m IsFragment) -> Val (Input.Payload m ())
replaceFragment parentEntityId idxInParent (Val pl bod) =
    case pl ^. Input.userData of
    IsFragment ->
        V.LVar fragmentVar & V.BLeaf
        & Val (void pl & Input.entityId .~ EntityId.ofFragmentUnder idxInParent parentEntityId)
    NotFragment ->
        bod & Lens.traversed %@~ replaceFragment (pl ^. Input.entityId)
        & Val (void pl)

emplaceInHoles :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
emplaceInHoles replaceHole =
    map fst . filter snd . (`runStateT` False) . go
    where
        go oldVal@(Val x bod) =
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
                        V.BApp (V.Apply (Val f (V.BLeaf V.LHole)) arg@(Val _ (V.BLeaf V.LHole))) ->
                            join $ lift
                                [ replace f
                                    <&> fmap (Val x . V.BApp . (`V.Apply` arg))
                                , pure (pure oldVal)
                                ]
                        _ -> traverse go bod <&> fmap (Val x) . sequenceA
        replace x = replaceHole x <$ State.put True

mkResultValFragment ::
    Monad m =>
    Infer.Payload ->
    Val (T.Type, Maybe (Input.Payload m a)) ->
    StateT Infer.Context (T m) (Hole.ResultVal m IsFragment)
mkResultValFragment inferred val =
    val <&> onPl
    & Hole.detachValIfNeeded emptyPl (inferred ^. Infer.plType)
    where
        emptyPl = (Nothing, NotFragment)
        onPl (typ, mInputPl) =
            ( inferred & Infer.plType .~ typ
            , case mInputPl of
              Nothing -> emptyPl
              Just inputPl ->
                (inputPl ^. Input.stored & Property.value & Just, IsFragment)
            )

mkOptionFromFragment ::
    Monad m =>
    ConvertM.Context m ->
    Input.Payload m a ->
    Val (T.Type, Maybe (Input.Payload m a)) ->
    HoleOption' (T m) (Expression InternalName (T m) (T m) ())
mkOptionFromFragment sugarContext exprPl val =
    HoleOption
    { _hoVal = baseExpr
    , _hoSugaredBaseExpr = Hole.sugar sugarContext exprPl baseExpr
    , _hoResults =
        do
            (result, inferContext) <-
                mkResultValFragment (exprPl ^. Input.inferred) val
                & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
            let depsProp = sugarContext ^. ConvertM.scFrozenDeps
            newDeps <-
                Hole.loadNewDeps (depsProp ^. Property.pVal)
                (exprPl ^. Input.inferred . Infer.plScope) val
            let newSugarContext =
                    sugarContext
                    & ConvertM.scInferContext .~ inferContext
                    & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
            let updateDeps = (depsProp ^. Property.pSet) newDeps
            pure
                ( resultScore (result <&> fst)
                , Hole.mkResult (replaceFragment topEntityId 0) newSugarContext
                    updateDeps stored result
                )
        <&> pure & ListClass.joinL
    }
    where
        stored = exprPl ^. Input.stored
        topEntityId = Property.value stored & EntityId.ofValI
        baseExpr = pruneExpr val
        pruneExpr (Val (_, Just{}) _) = P.hole
        pruneExpr (Val _ b) = b <&> pruneExpr & Val ()
