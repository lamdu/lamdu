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
import           Data.Store.Transaction (Transaction)
import           Data.UUID.Types (UUID)
import           Lamdu.Builtins.Anchors (boolTid, trueTag, falseTag)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import           Lamdu.Calc.Type.Scheme (schemeType)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import           Lamdu.Data.Anchors (bParamScopeId)
import           Lamdu.Expr.IRef (ValI)
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
                return
                    ( do
                        Lens.has (rBody . _BodyHole) argS & guard
                        let dst = argI ^. Val.payload . Input.stored . Property.pVal
                        let deleteAction =
                                (UniqueId.toUUID dst, EntityId.ofValI dst) <$
                                protectedSetToVal
                                (exprPl ^. Input.stored)
                                dst
                        funcS
                            & rPayload . plActions . setToHole .~ SetToHole deleteAction
                            & return
                        & fromMaybe funcS
                    , argS
                    )
        justToLeft $ convertAppliedCase funcS argS exprPl
        justToLeft $ convertLabeled funcS argS exprPl
        lift $ convertPrefix funcS argS exprPl

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

convertLabeled ::
    (Monad m, Monoid a) =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS exprPl =
    do
        sBinderVar <-
            funcS ^? rBody . _BodyGetVar . _GetBinder & maybeToMPlus
        record <- argS ^? rBody . _BodyRecord & maybeToMPlus
        Lens.has (rTail . _ClosedRecord) record & guard
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
        BodyLabeledApply LabeledApply
            { _aFunc = sBinderVar
            , _aSpecialArgs = NoSpecialArgs
            , _aAnnotatedArgs = args
            , _aRelayedArgs =
                -- Hidden args must be determined along with the special args.
                -- One never wants to hide an infix operator's args.
                []
            }
            & lift . addActions exprPl

convertPrefix ::
    Monad m =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertPrefix funcS argS applyPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setToFunc =
                protectedSetToVal (applyPl ^. Input.stored)
                (funcS ^. rPayload . plData . pStored & Property.value)
                <&> EntityId.ofValI
        BodySimpleApply Apply
            { _applyFunc = funcS
            , _applyArg =
                argS & rBody . _BodyHole . holeActions . holeMDelete .~ Just setToFunc
            }
            & addActions applyPl

convertAppliedCase ::
    (Monad m, Monoid a) =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedCase funcS argS exprPl =
    do
        caseB <- funcS ^? rBody . _BodyCase & maybeToMPlus
        Lens.has (cKind . _LambdaCase) caseB & guard
        protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
        let setTo = protectedSetToVal (exprPl ^. Input.stored)
        let appliedCaseB =
                caseB
                & cKind .~ CaseWithArg
                    CaseArg
                    { _caVal = simplifyCaseArg argS
                    , _caToLambdaCase =
                        setTo (funcS ^. rPayload . plData . pStored . Property.pVal) <&> EntityId.ofValI
                    }
        maybeGuard setTo appliedCaseB & addActions exprPl & lift

maybeGuard ::
    Functor m =>
    (ValI m -> Transaction m (ValI m)) -> Case UUID m (ExpressionU m a) -> Body UUID m (ExpressionU m a)
maybeGuard setToVal caseBody =
    case caseBody ^? cKind . _CaseWithArg . caVal of
    Just arg ->
        case arg ^. rBody of
        BodyFromNom nom | nom ^. nTId . tidgTId == boolTid -> tryGuard (nom ^. nVal)
        _ | arg ^? rPayload . plAnnotation . aInferredType . T._TInst . _1 == Just boolTid -> tryGuard arg
        _ -> notAGuard
    _ -> notAGuard
    where
        notAGuard = BodyCase caseBody
        tryGuard cond =
            case caseBody ^. cAlts of
            [alt0, alt1]
                | tagOf alt0 == trueTag && tagOf alt1 == falseTag -> convGuard cond alt0 alt1
                | tagOf alt1 == trueTag && tagOf alt0 == falseTag -> convGuard cond alt1 alt0
            _ -> notAGuard
        tagOf alt = alt ^. caTag . tagVal
        convGuard cond altTrue altFalse =
            case mAltFalseBinder of
            Just binder ->
                case mAltFalseBinderExpr of
                Just altFalseBinderExpr ->
                    case altFalseBinderExpr ^. rBody of
                    BodyGuard innerGuard ->
                        GuardElseIf
                        { _geScopes =
                            case binder ^. bBodyScopes of
                            SameAsParentScope -> error "lambda body should have scopes"
                            BinderBodyScope x -> x <&> Lens.mapped %~ getScope
                        , _geEntityId = altFalseBinderExpr ^. rPayload . plEntityId
                        , _geCond = innerGuard ^. gIf
                        , _geThen = innerGuard ^. gThen
                        , _geDelete = innerGuard ^. gDeleteIf
                        , _geCondAddLet = binder ^. bBody . bbAddOuterLet
                        }
                        : innerGuard ^. gElseIfs
                        & makeRes (innerGuard ^. gElse)
                        where
                            getScope [x] = x ^. bParamScopeId
                            getScope _ = error "guard evaluated more than once in same scope?"
                    _ -> simpleIfElse
                Nothing -> simpleIfElse
            Nothing -> simpleIfElse
            & BodyGuard
            where
                mAltFalseBinder = altFalse ^? caHandler . rBody . _BodyLam . lamBinder
                mAltFalseBinderExpr = mAltFalseBinder ^? Lens._Just . bBody . bbContent . _BinderExpr
                simpleIfElse = makeRes (altFalse ^. caHandler) []
                makeRes els elseIfs =
                    Guard
                    { _gIf = cond
                    , _gThen = altTrue ^. caHandler
                    , _gElseIfs = elseIfs
                    , _gElse = els
                    , _gDeleteIf =
                        fromMaybe (altFalse ^. caHandler) mAltFalseBinderExpr
                        ^. rPayload . plData . pStored . Property.pVal
                        & setToVal
                        <&> EntityId.ofValI
                    }

simplifyCaseArg :: Monoid a => ExpressionU m a -> ExpressionU m a
simplifyCaseArg argS =
    case argS ^. rBody of
    BodyFromNom nom | Lens.nullOf (nVal . rBody . _BodyHole) nom ->
        nom ^. nVal
        & rPayload . plActions . setToHole .~ argS ^. rPayload . plActions . setToHole
    _ -> argS
