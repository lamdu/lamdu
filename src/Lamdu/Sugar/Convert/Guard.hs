-- | "if" sugar/guards conversion
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Guard (convertGuard) where

import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Data.UUID.Types (UUID)
import           Lamdu.Builtins.Anchors (boolTid, trueTag, falseTag)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (bParamScopeId)
import           Lamdu.Expr.IRef (ValI)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

convertGuard ::
    Functor m =>
    (ValI m -> T m (ValI m)) ->
    Case UUID (T m) (ExpressionU m a) ->
    Maybe (Guard (T m) (ExpressionU m a))
convertGuard setToVal caseBody =
    do
        arg <- caseBody ^? cKind . _CaseWithArg . caVal
        case arg ^. rBody of
            BodyFromNom nom | nom ^. nTId . tidTId == boolTid -> tryGuard (nom ^. nVal)
            _ | arg ^? rPayload . plAnnotation . aInferredType . T._TInst . _1 == Just boolTid -> tryGuard arg
            _ -> Nothing
    where
        tryGuard cond =
            case caseBody ^. cBody . cItems of
            [alt0, alt1]
                | tagOf alt0 == trueTag && tagOf alt1 == falseTag -> convGuard cond alt0 alt1
                | tagOf alt1 == trueTag && tagOf alt0 == falseTag -> convGuard cond alt1 alt0
            _ -> Nothing
        tagOf alt = alt ^. ciTag . tagInfo . tagVal
        convGuard cond altTrue altFalse =
            case mAltFalseBinder of
            Just binder ->
                case binder ^? bBody . bbContent . _BinderExpr of
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
            & Just
            where
                mAltFalseBinder = altFalse ^? ciExpr . rBody . _BodyLam . lamBinder
                simpleIfElse =
                    altFalse ^. ciExpr
                    & rPayload . plActions . delete %~ mkElseDel
                    & rBody . _BodyLam . lamBinder . bBody . bbContent . _BinderExpr
                        . rPayload . plActions . delete %~ mkElseDel
                    & (`makeRes` [])
                mkElseDel CannotDelete =
                    delTarget altTrue & setToVal <&> EntityId.ofValI & Delete
                mkElseDel x = x
                delTarget alt =
                    alt ^? ciExpr . rBody . _BodyLam . lamBinder . bBody . bbContent . _BinderExpr
                    & fromMaybe (alt ^. ciExpr)
                    & (^. rPayload . plData . pStored . Property.pVal)
                makeRes els elseIfs =
                    Guard
                    { _gIf = cond
                    , _gThen = altTrue ^. ciExpr
                    , _gElseIfs = elseIfs
                    , _gElse = els
                    , _gDeleteIf = delTarget altFalse & setToVal <&> EntityId.ofValI
                    }

