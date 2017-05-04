{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, OverloadedStrings #-}
module Lamdu.Sugar.Convert.Apply
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map as Map
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import           Lamdu.Calc.Type.Scheme (schemeType)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.RecordVal as RecordVal
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Hole.Wrapper (convertAppliedHole)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
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
                let setToFuncAction =
                        funcI ^. Val.payload . Input.stored
                        & Property.value
                        & protectedSetToVal (exprPl ^. Input.stored)
                        <&> EntityId.ofValI
                        & SetToInnerExpr
                if Lens.has (rBody . _BodyHole) argS
                    then
                    return
                    ( funcS & rPayload . plActions . setToHole .~ AlreadyAppliedToHole
                    , argS & rPayload . plActions . setToInnerExpr .~ setToFuncAction
                    )
                    else return (funcS, argS)
        justToLeft $ convertAppliedCase funcS (funcI ^. Val.payload) argS exprPl
        justToLeft $ convertLabeled funcS argS argI exprPl
        lift $ convertPrefix funcS argS exprPl

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

convertLabeled ::
    (Monad m, Monoid a) =>
    ExpressionU m a -> ExpressionU m a -> Val (Input.Payload m a) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS argI exprPl =
    do
        guard $ Lens.has (Val.body . V._BLeaf . V._LRecEmpty) recordTail
        sBinderVar <-
            funcS ^? rBody . _BodyGetVar . _GetBinder & maybeToMPlus
        record <- argS ^? rBody . _BodyRecord & maybeToMPlus
        guard $ length (record ^. rItems) >= 2
        ctx <- lift ConvertM.readContext
        let var = sBinderVar ^. bvNameRef . nrName & UniqueId.identifierOfUUID & V.Var
        unless (Lens.has (Lens.at var . Lens._Just)
            (Infer.scopeToTypeMap (exprPl ^. Input.inferred . Infer.plScope))) $
            do
                defArgs <-
                    ctx ^? ConvertM.scFrozenDeps . Property.pVal
                        . Infer.depsGlobalTypes . Lens.at var . Lens._Just
                        . schemeType . T._TFun . _1 . T._TRecord
                    & maybeToMPlus
                let flatArgs = FlatComposite.fromComposite defArgs
                flatArgs ^? FlatComposite.extension . Lens._Nothing & maybeToMPlus
                let sFields =
                        record ^.. rItems . traverse . rfTag . tagVal & Set.fromList
                guard $ Map.keysSet (flatArgs ^. FlatComposite.fields) == sFields
        let getArg field =
                AnnotatedArg
                    { _aaTag = field ^. rfTag
                    , _aaExpr = field ^. rfExpr
                    }
        let args = map getArg $ record ^. rItems
        let tags = args ^.. Lens.traversed . aaTag . tagVal
        unless (noRepetitions tags) $ error "Repetitions should not type-check"
        protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
        let innerExpr =
                case (filter (Lens.nullOf ExprLens.valHole) . map snd . Map.elems) fieldsI of
                [x] -> x
                _ -> argI
        let setToInnerExprAction =
                innerExpr ^. Val.payload . Input.stored & Property.value
                & protectedSetToVal (exprPl ^. Input.stored)
                <&> EntityId.ofValI
                & SetToInnerExpr
        BodyLabeledApply LabeledApply
            { _aFunc = sBinderVar
            , _aSpecialArgs = NoSpecialArgs
            , _aAnnotatedArgs = args
            }
            & lift . addActions exprPl
            <&> rPayload %~
                ( plData <>~ argS ^. rPayload . plData ) .
                ( plActions . setToInnerExpr .~ setToInnerExprAction
                )
    where
        (fieldsI, recordTail) = RecordVal.unpack argI

convertPrefix ::
    Monad m =>
    ExpressionU m a -> ExpressionU m a ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertPrefix funcS argS applyPl =
    BodySimpleApply Apply
    { _applyFunc = funcS
    , _applyArg = argS
    }
    & addActions applyPl

convertAppliedCase ::
    (Monad m, Monoid a) =>
    ExpressionU m a -> Input.Payload m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedCase funcS funcPl argS exprPl =
    do
        caseB <- funcS ^? rBody . _BodyCase & maybeToMPlus
        Lens.has (cKind . _LambdaCase) caseB & guard
        protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
        caseB
            & cKind .~ CaseWithArg
                CaseArg
                { _caVal = argS
                , _caToLambdaCase =
                    protectedSetToVal (exprPl ^. Input.stored)
                    (funcPl ^. Input.stored & Property.value) <&> EntityId.ofValI
                }
            & BodyCase
            & lift . addActions exprPl
    <&> rPayload . plData <>~ funcS ^. rPayload . plData
