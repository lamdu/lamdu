{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, OverloadedStrings #-}
module Lamdu.Sugar.Convert.Apply
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (MonadPlus)
import           Control.Monad.Trans.Except.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.List.Utils (isLengthAtLeast)
import qualified Data.Map as Map
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Set as Set
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import           Lamdu.Calc.Type.Scheme (schemeType)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Case (convertAppliedCase)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
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
                    ( if Lens.has (rBody . _BodyHole) argS
                      then
                          let dst = argI ^. Val.payload . Input.stored . Property.pVal
                              deleteAction =
                                  EntityId.ofValI dst <$
                                  protectedSetToVal (exprPl ^. Input.stored) dst
                          in  funcS
                              & rPayload . plActions . mSetToHole ?~ deleteAction
                      else funcS
                    , argS
                    )
        justToLeft $ convertAppliedCase funcS argS exprPl
        justToLeft $ convertLabeled funcS argS exprPl
        lift $ convertPrefix funcS argS exprPl

noDuplicates :: Ord a => [a] -> Bool
noDuplicates x = length x == Set.size (Set.fromList x)

validateDefParamsMatchArgs ::
    MonadPlus m =>
    V.Var -> Composite name f1 expr -> Property f2 Infer.Dependencies -> m ()
validateDefParamsMatchArgs var record frozenDeps =
    do
        defArgs <-
            frozenDeps ^? Property.pVal
                . Infer.depsGlobalTypes . Lens.at var . Lens._Just
                . schemeType . T._TFun . _1 . T._TRecord
            & maybeToMPlus
        let flatArgs = FlatComposite.fromComposite defArgs
        flatArgs ^? FlatComposite.extension . Lens._Nothing & maybeToMPlus
        let sFields =
                record ^.. cItems . traverse . ciTag . tagInfo . tagVal
                & Set.fromList
        guard (sFields == Map.keysSet (flatArgs ^. FlatComposite.fields))

convertLabeled ::
    Monad m =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS exprPl =
    do
        -- Make sure it's a not a param, get the var
        sBinderVar <- funcS ^? rBody . _BodyGetVar . _GetBinder & maybeToMPlus
        -- Make sure it is not a "let" but a "def" (recursive or external)
        _ <- sBinderVar ^? bvForm . _GetDefinition & maybeToMPlus
        -- Make sure the argument is a record
        record <- argS ^? rBody . _BodyRecord & maybeToMPlus
        -- that is closed
        Lens.has (cTail . _ClosedComposite) record & guard
        -- with at least 2 fields
        isLengthAtLeast 2 (record ^. cItems) & guard
        frozenDeps <- Lens.view ConvertM.scFrozenDeps
        let var = sBinderVar ^. bvNameRef . nrName . inUUID & UniqueId.varOfUUID
        let scope = exprPl ^. Input.inferred . Infer.plScope & Infer.scopeToTypeMap
        -- If it is an external (non-recursive) def (i.e: not in
        -- scope), make sure the def (frozen) type is inferred to have
        -- closed record of same parameters
        validateDefParamsMatchArgs var record frozenDeps
            & unless (var `Map.member` scope)
        let getArg field =
                AnnotatedArg
                    { _aaTag = field ^. ciTag . tagInfo
                    , _aaName = field ^. ciTag . tagName
                    , _aaExpr = field ^. ciExpr
                    }
        let args = record ^. cItems <&> getArg
        let tags = args ^.. Lens.traversed . aaTag . tagVal
        unless (noDuplicates tags) $ lift $ fail "Duplicate tags shouldn't type-check"
        BodyLabeledApply LabeledApply
            { _aFunc = sBinderVar
            , _aSpecialArgs = Verbose
            , _aAnnotatedArgs = args
            , _aRelayedArgs =
                -- Hidden args must be determined along with the special args.
                -- One never wants to hide an infix operator's args.
                []
            } & lift . addActions exprPl

convertPrefix ::
    Monad m =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertPrefix funcS argS applyPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let del =
                protectedSetToVal (applyPl ^. Input.stored)
                (funcS ^. rPayload . plData . pStored & Property.value)
                <&> EntityId.ofValI
        BodySimpleApply Apply
            { _applyFunc = funcS
            , _applyArg = argS & rBody . _BodyHole . holeMDelete ?~ del
            }
            & addActions applyPl
