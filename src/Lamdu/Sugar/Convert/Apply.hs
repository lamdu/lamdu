{-# LANGUAGE NamedFieldPuns #-}
module Lamdu.Sugar.Convert.Apply
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (MonadPlus)
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.List.Extended (isLengthAtLeast)
import qualified Data.Map as Map
import           Data.Maybe.Extended (maybeToMPlus)
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import qualified Lamdu.Calc.Type.Scheme as CalcScheme
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Case (convertAppliedCase)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, addActionsWith, subexprPayloads)
import           Lamdu.Sugar.Convert.Fragment (convertAppliedHole)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) => V.Apply (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert app@(V.Apply funcI argI) exprPl =
    runMatcherT $
    do
        (funcS, argS) <-
            do
                argS <- lift $ ConvertM.convertSubexpression argI
                justToLeft $ convertAppliedHole app argS exprPl
                funcS <- ConvertM.convertSubexpression funcI & lift
                protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
                pure
                    ( if Lens.has (body . _BodyHole) argS
                      then
                          let dst = argI ^. Val.payload . Input.stored . Property.pVal
                              deleteAction =
                                  EntityId.ofValI dst <$
                                  protectedSetToVal (exprPl ^. Input.stored) dst
                          in  funcS
                              & annotation . plActions . mSetToHole ?~ deleteAction
                      else funcS
                    , argS
                    )
        justToLeft $ convertAppliedCase app funcS argS exprPl
        justToLeft $ convertLabeled app funcS argS exprPl
        lift $ convertPrefix app funcS argS exprPl

noDuplicates :: Ord a => [a] -> Bool
noDuplicates x = length x == Set.size (Set.fromList x)

validateDefParamsMatchArgs ::
    MonadPlus m =>
    V.Var -> Composite name i o expr -> Infer.Dependencies -> m ()
validateDefParamsMatchArgs var record frozenDeps =
    do
        defArgs <-
            frozenDeps ^?
                Infer.depsGlobalTypes . Lens.at var . Lens._Just
                . CalcScheme.schemeType . T._TFun . _1 . T._TRecord
            & maybeToMPlus
        let flatArgs = FlatComposite.fromComposite defArgs
        flatArgs ^? FlatComposite.extension . Lens._Nothing & maybeToMPlus
        let sFields =
                record ^.. cItems . traverse . ciTag . tagInfo . tagVal
                & Set.fromList
        guard (sFields == Map.keysSet (flatArgs ^. FlatComposite.fields))

convertLabeled ::
    (Monad m, Foldable f, Monoid a) =>
    f (Val (Input.Payload m a)) ->
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled subexprs funcS argS exprPl =
    do
        -- Make sure it's a not a param, get the var
        sBinderVar <- funcS ^? body . _BodyGetVar . _GetBinder & maybeToMPlus
        -- Make sure it is not a "let" but a "def" (recursive or external)
        _ <- sBinderVar ^? bvForm . _GetDefinition & maybeToMPlus
        -- Make sure the argument is a record
        record <- argS ^? body . _BodyRecord & maybeToMPlus
        -- that is closed
        Lens.has (cTail . _ClosedComposite) record & guard
        -- with at least 2 fields
        isLengthAtLeast 2 (record ^. cItems) & guard
        frozenDeps <- Lens.view ConvertM.scFrozenDeps <&> Property.value
        let var = sBinderVar ^. bvVar
        let scope = exprPl ^. Input.inferred . Infer.plScope & Infer.scopeToTypeMap
        -- If it is an external (non-recursive) def (i.e: not in
        -- scope), make sure the def (frozen) type is inferred to have
        -- closed record of same parameters
        validateDefParamsMatchArgs var record frozenDeps
            & unless (var `Map.member` scope)
        let getArg field =
                AnnotatedArg
                    { _aaTag = field ^. ciTag . tagInfo
                    , _aaExpr = field ^. ciExpr
                    }
        let args = record ^. cItems <&> getArg
        let tags = args ^.. Lens.traversed . aaTag . tagVal
        unless (noDuplicates tags) $ lift $ fail "Duplicate tags shouldn't type-check"
        let bod =
                BodyLabeledApply LabeledApply
                { _aFunc = LabeledApplyFunc sBinderVar (void (funcS ^. annotation))
                , _aSpecialArgs = Verbose
                , _aAnnotatedArgs = args
                , _aRelayedArgs =
                    -- Hidden args must be determined along with the special args.
                    -- One never wants to hide an infix operator's args.
                    []
                }
        let userPayload =
                subexprPayloads subexprs
                (funcS ^. annotation : bod ^.. bodyChildren . annotation)
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
                (funcS ^. annotation . plData . pStored & Property.value)
                <&> EntityId.ofValI
        BodySimpleApply Apply
            { _applyFunc = funcS
            , _applyArg = argS & body . _BodyHole . holeMDelete ?~ del
            } & addActions subexprs applyPl
