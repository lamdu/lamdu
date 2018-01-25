-- | "if" sugar/guards conversion
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.IfElse (convertIfElse) where

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

convertIfElse ::
    Functor m =>
    (ValI m -> T m (ValI m)) ->
    Case UUID (T m) (ExpressionU m a) ->
    Maybe (IfElse (T m) (ExpressionU m a))
convertIfElse setToVal caseBody =
    do
        arg <- caseBody ^? cKind . _CaseWithArg . caVal
        case arg ^. rBody of
            BodyFromNom nom | nom ^. nTId . tidTId == boolTid -> tryIfElse (nom ^. nVal)
            _ | arg ^? rPayload . plAnnotation . aInferredType . T._TInst . _1 == Just boolTid -> tryIfElse arg
            _ -> Nothing
    where
        tryIfElse cond =
            case caseBody ^. cBody . cItems of
            [alt0, alt1]
                | tagOf alt0 == trueTag && tagOf alt1 == falseTag -> convIfElse cond alt0 alt1
                | tagOf alt1 == trueTag && tagOf alt0 == falseTag -> convIfElse cond alt1 alt0
            _ -> Nothing
        tagOf alt = alt ^. ciTag . tagInfo . tagVal
        convIfElse cond altTrue altFalse =
            case mAltFalseBinder of
            Just binder ->
                case binder ^? bBody . bbContent . _BinderExpr of
                Just altFalseBinderExpr ->
                    case altFalseBinderExpr ^. rBody of
                    BodyIfElse innerIfElse ->
                        ElseIf ElseIfContent
                        { _eiScopes =
                            case binder ^. bBodyScopes of
                            SameAsParentScope -> error "lambda body should have scopes"
                            BinderBodyScope x -> x <&> Lens.mapped %~ getScope
                        , _eiEntityId = altFalseBinderExpr ^. rPayload . plEntityId
                        , _eiIfThen = innerIfElse ^. iIfThen
                        , _eiCondAddLet = binder ^. bBody . bbAddOuterLet
                        , _eiElse = innerIfElse ^. iElse
                        }
                        & makeRes
                        where
                            getScope [x] = x ^. bParamScopeId
                            getScope _ = error "if-else evaluated more than once in same scope?"
                    _ -> simpleIfElse
                Nothing -> simpleIfElse
            Nothing -> simpleIfElse
            & Just
            where
                mAltFalseBinder = altFalse ^? ciExpr . rBody . _BodyLam . lamBinder
                simpleIfElse =
                    altFalse ^. ciExpr
                    & rBody . _BodyHole . holeMDelete ?~ elseDel
                    & rBody . _BodyLam . lamBinder . bBody . bbContent . _BinderExpr
                        . rBody . _BodyHole . holeMDelete ?~ elseDel
                    & SimpleElse
                    & makeRes
                elseDel = setToVal (delTarget altTrue) <&> EntityId.ofValI
                delTarget alt =
                    alt ^? ciExpr . rBody . _BodyLam . lamBinder . bBody . bbContent . _BinderExpr
                    & fromMaybe (alt ^. ciExpr)
                    & (^. rPayload . plData . pStored . Property.pVal)
                makeRes els =
                    IfElse
                    { _iIfThen =
                        IfThen
                        { _itIf = cond
                        , _itThen = altTrue ^. ciExpr
                        , _itDelete = delTarget altFalse & setToVal <&> EntityId.ofValI
                        }
                    , _iElse = els
                    }

