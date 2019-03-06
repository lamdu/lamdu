{-# LANGUAGE NamedFieldPuns #-}
module Lamdu.Sugar.Convert.Apply
    ( convert
    ) where

import           AST (Tree, monoChildren, _Pure)
import           AST.Knot.Ann (Ann(..), ann, val)
import           AST.Term.FuncType (funcIn)
import           AST.Term.Row (freExtends, freRest)
import           AST.Term.Scheme (sTyp)
import qualified Control.Lens as Lens
import           Control.Monad (MonadPlus)
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Functor.Const (Const(..))
import           Data.List.Extended (isLengthAtLeast)
import qualified Data.Map as Map
import           Data.Maybe.Extended (maybeToMPlus)
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Lamdu.Calc.Definition (Deps, depsGlobalTypes)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Case (convertAppliedCase)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, addActionsWith, subexprPayloads)
import           Lamdu.Sugar.Convert.Fragment (convertAppliedHole)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Nominal (convertAppliedFromNom)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens (childPayloads)
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) =>
    Tree (V.Apply V.Term) (Ann (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert app@(V.Apply funcI argI) exprPl =
    runMatcherT $
    do
        (funcS, argS) <-
            do
                argS <- ConvertM.convertSubexpression argI & lift
                convertAppliedHole app argS exprPl & justToLeft
                convertAppliedFromNom app argS exprPl & justToLeft
                funcS <- ConvertM.convertSubexpression funcI & lift
                protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
                pure
                    ( if Lens.has (val . _BodyHole) argS
                      then
                          let dst = argI ^. ann . Input.stored . Property.pVal
                              deleteAction =
                                  EntityId.ofValI dst <$
                                  protectedSetToVal (exprPl ^. Input.stored) dst
                          in  funcS
                              & ann . pActions . mSetToHole ?~ deleteAction
                      else funcS
                    , argS
                    )
        convertAppliedCase app funcS argS exprPl & justToLeft
        convertLabeled (app ^.. monoChildren) funcS argS exprPl & justToLeft
        convertPrefix (app ^.. monoChildren) funcS argS exprPl & lift

noDuplicates :: Ord a => [a] -> Bool
noDuplicates x = length x == Set.size (Set.fromList x)

validateDefParamsMatchArgs ::
    MonadPlus m =>
    V.Var -> Composite name i o expr -> Deps -> m ()
validateDefParamsMatchArgs var record frozenDeps =
    do
        defArgs <-
            frozenDeps ^?
                depsGlobalTypes . Lens.at var . Lens._Just
                . _Pure . sTyp . _Pure . T._TFun . funcIn
                . _Pure . T._TRecord . T.flatRow
            & maybeToMPlus
        defArgs ^? freRest . _Pure . T._REmpty & maybeToMPlus
        let sFields =
                record ^.. cItems . traverse . ciTag . tagInfo . tagVal
                & Set.fromList
        guard (sFields == Map.keysSet (defArgs ^. freExtends))

convertLabeled ::
    (Monad m, Foldable f, Monoid a) =>
    f (Val (Input.Payload m a)) ->
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled subexprs funcS argS exprPl =
    do
        -- Make sure it's a not a param, get the var
        sBinderVar <- funcS ^? val . _BodyGetVar . _GetBinder & maybeToMPlus
        -- Make sure it is not a "let" but a "def" (recursive or external)
        _ <- sBinderVar ^? bvForm . _GetDefinition & maybeToMPlus
        -- Make sure the argument is a record
        record <- argS ^? val . _BodyRecord & maybeToMPlus
        -- that is closed
        Lens.has (cTail . _ClosedComposite) record & guard
        -- with at least 2 fields
        isLengthAtLeast 2 (record ^. cItems) & guard
        frozenDeps <- Lens.view ConvertM.scFrozenDeps <&> Property.value
        let var = sBinderVar ^. bvVar
        -- If it is an external (non-recursive) def (i.e: not in
        -- scope), make sure the def (frozen) type is inferred to have
        -- closed record of same parameters
        recursiveRef <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
        validateDefParamsMatchArgs var record frozenDeps
            & unless (Just var == (recursiveRef <&> (^. ConvertM.rrDefI) <&> ExprIRef.globalId))
        let getArg field =
                AnnotatedArg
                    { _aaTag = field ^. ciTag . tagInfo
                    , _aaExpr = field ^. ciExpr
                    }
        let args = record ^. cItems <&> getArg
        let tags = args ^.. Lens.traversed . aaTag . tagVal
        unless (noDuplicates tags) $ lift $ fail "Duplicate tags shouldn't type-check"
        bod <-
            PresentationModes.makeLabeledApply
            (Ann (funcS ^. ann) (Const sBinderVar)) args exprPl
            <&> BodyLabeledApply & lift
        let userPayload =
                subexprPayloads subexprs (bod ^.. childPayloads)
                & mconcat
        addActionsWith userPayload exprPl bod & lift

convertPrefix ::
    (Monad m, Foldable f, Monoid a) =>
    f (Val (Input.Payload m a)) ->
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertPrefix subexprs funcS argS applyPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let del =
                protectedSetToVal (applyPl ^. Input.stored)
                (funcS ^. ann . pInput . Input.stored & Property.value)
                <&> EntityId.ofValI
        BodySimpleApply Apply
            { _applyFunc = funcS
            , _applyArg = argS & val . _BodyHole . holeMDelete ?~ del
            } & addActions subexprs applyPl
