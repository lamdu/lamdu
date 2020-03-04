-- | "if" sugar/guards conversion
{-# LANGUAGE TypeFamilies #-}

module Lamdu.Sugar.Convert.IfElse (convertIfElse) where

import qualified Control.Lens as Lens
import           Hyper.Type.AST.Nominal (nId)
import           Lamdu.Builtins.Anchors (boolTid, trueTag, falseTag)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Expr.IRef (ValI, iref)
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
    Case InternalName (T m) (T m) # Annotated (ConvertPayload m a) ->
    Maybe (IfElse (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) # Annotated (ConvertPayload m a))
convertIfElse setToVal caseBody =
    do
        arg <- caseBody ^? cKind . _CaseWithArg . caVal
        case arg ^. hVal of
            BodySimpleApply (App (Ann _ (BodyFromNom nom)) x)
                | nom ^. tidTId == boolTid ->
                    -- In "case _»Nom of ..." the case expression doesn't absorb the FromNom
                    -- (and also in case of fragment)
                    tryIfElse x
            _ | arg ^? annotation . pInput . Input.inferredType . _Pure . T._TInst . nId == Just boolTid ->
                tryIfElse arg
            _ -> Nothing
    where
        tryIfElse cond =
            case caseBody ^. cBody . cItems of
            [alt0, alt1]
                | tagOf alt0 == trueTag && tagOf alt1 == falseTag -> convIfElse cond alt0 alt1
                | tagOf alt1 == trueTag && tagOf alt0 == falseTag -> convIfElse cond alt1 alt0
            _ -> Nothing
        tagOf alt = alt ^. ciTag . tagRefTag . tagVal
        convIfElse cond altTrue altFalse =
            Just IfElse
            { _iIf = cond
            , _iThen = altTrue ^. ciExpr
            , _iElse =
                case altFalse ^?
                     ciExpr . hVal . _BodyLam . lamFunc .
                     fBody . hVal . _BinderTerm . _BodyIfElse
                of
                Just innerIfElse ->
                    Ann
                    { _hVal = ElseIf innerIfElse
                    , _hAnn = altFalse ^. ciExpr . annotation & Const
                    }
                Nothing ->
                    altFalse ^. ciExpr . hVal
                    & _BodyHole . holeMDelete ?~ elseDel
                    & _BodyLam . lamFunc . fBody . hVal . _BinderTerm .
                        _BodyHole . holeMDelete ?~ elseDel
                    & SimpleElse
                    & Ann (Const (altFalse ^. ciExpr . annotation))
            }
            where
                elseDel = setToVal (delTarget altTrue) <&> EntityId.ofValI
                delTarget alt =
                    alt ^? ciExpr . hVal . _BodyLam . lamFunc . fBody
                    . Lens.filteredBy (hVal . _BinderTerm) . annotation
                    & fromMaybe (alt ^. ciExpr . annotation)
                    & (^. pInput . Input.stored . iref)
