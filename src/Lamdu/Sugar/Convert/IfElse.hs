-- | "if" sugar/guards conversion
module Lamdu.Sugar.Convert.IfElse (convertIfElse) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Lamdu.Builtins.Anchors (boolTid, trueTag, falseTag)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (bParamScopeId)
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convertIfElse ::
    Functor m =>
    (ValI m -> T m (ValI m)) ->
    Case InternalName (T m) (T m) (ExpressionU m a) ->
    Maybe (IfElse InternalName (T m) (T m) (ConvertPayload m a))
convertIfElse setToVal caseBody =
    do
        arg <- caseBody ^? cKind . _CaseWithArg . caVal
        case arg ^. _PNode . val of
            BodyFromNom nom | nom ^. nTId . tidTId == boolTid -> tryIfElse (nom ^. nVal)
            _ | arg ^? _PNode . ann . pInput . Input.inferred . Infer.plType . T._TInst . _1 == Just boolTid ->
                tryIfElse arg
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
                case binder ^? fBody . bContent . _BinderExpr of
                Just altFalseBinderExpr ->
                    case altFalseBinderExpr ^. _PNode . val of
                    BodyIfElse innerIfElse ->
                        ElseIf ElseIfContent
                        { _eiScopes =
                            case binder ^. fBodyScopes of
                            SameAsParentScope -> error "lambda body should have scopes"
                            BinderBodyScope x -> x <&> Lens.mapped %~ getScope
                        , _eiEntityId = altFalseBinderExpr ^. _PNode . ann . pInput . Input.entityId
                        , _eiContent = innerIfElse
                        , _eiCondAddLet = binder ^. fBody . bAddOuterLet
                        , _eiNodeActions = altFalseBinderExpr ^. _PNode . ann . pActions
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
                mAltFalseBinder = altFalse ^? ciExpr . _PNode . val . _BodyLam . lamFunc
                simpleIfElse =
                    altFalse ^. ciExpr
                    & _PNode . val . _BodyHole . holeMDelete ?~ elseDel
                    & _PNode . val . _BodyLam . lamFunc . fBody . bContent . _BinderExpr
                        . _PNode . val . _BodyHole . holeMDelete ?~ elseDel
                    & SimpleElse
                    & makeRes
                elseDel = setToVal (delTarget altTrue) <&> EntityId.ofValI
                delTarget alt =
                    alt ^? ciExpr . _PNode . val . _BodyLam . lamFunc . fBody . bContent . _BinderExpr
                    & fromMaybe (alt ^. ciExpr)
                    & (^. _PNode . ann . pInput . Input.stored . Property.pVal)
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

