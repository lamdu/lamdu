{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Binder.Params (convertVarToGetField, tagGForLambdaTagParam)
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

toGetGlobal :: MonadA m => DefI m -> ValIProperty m -> Transaction m ()
toGetGlobal defI exprP =
    ExprIRef.writeValBody exprI $ V.BLeaf $ V.LGlobal globalId
    where
        exprI = Property.value exprP
        globalId = ExprIRef.globalId defI

moveToGlobalScope ::
    MonadA m =>
    ConvertM.Context m -> V.Var -> Val (ValIProperty m) -> ValI m ->
    Transaction m (DefI m)
moveToGlobalScope ctx param letBodyStored letI =
    do
        paramName <- Anchors.assocNameRef param & Transaction.getP
        SubExprs.onGetVars
            (toGetGlobal
             (fromMaybe (error "recurseVar used not in definition context?!") (ctx ^. ConvertM.scDefI)))
            Builtins.recurseVar letBodyStored
        DataOps.newPublicDefinitionWithPane paramName
            (ctx ^. ConvertM.scCodeAnchors) letI

data NewLet m = NewLet
    { nlIRef :: ValI m
    , nlOnVar :: Val (Maybe (ValI m)) -> Val (Maybe (ValI m))
    , nlOnArgToVar :: Val (Maybe (ValI m)) -> Val (Maybe (ValI m))
    , nlMVarToTags :: Maybe VarToTags
    }

isVarAlwaysApplied :: V.Var -> Val a -> Bool
isVarAlwaysApplied var =
    go False
    where
        go isApplied (Val _ (V.BLeaf (V.LVar v))) | v == var = isApplied
        go _ (Val _ (V.BApp (V.Apply f a))) = go True f && go False a
        go _ v = all (go False) (v ^.. V.body . Lens.traverse)

isVarAlwaysRecordOfGetField :: V.Var -> Val a -> Bool
isVarAlwaysRecordOfGetField var =
    go False
    where
        go isGf (Val _ (V.BLeaf (V.LVar v))) | v == var = isGf
        go _ (Val _ (V.BGetField (V.GetField r _))) = go True r
        go _ v = all (go False) (v ^.. V.body . Lens.traverse)

convertLetToLam ::
    Monad m => V.Var -> Redex (ValIProperty m) -> Transaction m (NewLet m)
convertLetToLam varToReplace redex =
    do
        newParam <- ExprIRef.newVar
        let toNewParam prop =
                V.LVar newParam & V.BLeaf &
                ExprIRef.writeValBody (Property.value prop)
        SubExprs.onGetVars toNewParam varToReplace (redexArg redex)
        fixed <-
            redexArg redex ^. V.payload & Property.value
            & V.Lam newParam & V.BAbs & ExprIRef.newValBody
        return NewLet
            { nlIRef = fixed
            , nlOnVar =
                Val Nothing . V.BApp
                . (`V.Apply` Val Nothing (V.BLeaf (V.LVar varToReplace)))
            , nlOnArgToVar = id
            , nlMVarToTags = Nothing
            }

newTag :: MonadA m => Transaction m T.Tag
newTag = GenIds.transaction GenIds.randomTag

convertLetParamToRecord ::
    Monad m =>
    V.Var -> Redex (ValIProperty m) -> V.Lam (Val (ValIProperty m)) ->
    Transaction m (NewLet m)
convertLetParamToRecord varToReplace redex (V.Lam lamVar lamBody) =
    do
        prevParamTag <- newTag
        convertVarToGetField prevParamTag lamVar lamBody
        newParamTag <- newTag
        let toNewParam prop =
                V.LVar lamVar & V.BLeaf
                & ExprIRef.newValBody
                <&> (`V.GetField` newParamTag) <&> V.BGetField
                >>= ExprIRef.writeValBody (Property.value prop)
        SubExprs.onGetVars toNewParam varToReplace lamBody
        let onArg arg =
                V.BLeaf V.LRecEmpty & Val Nothing
                & V.RecExtend newParamTag
                    (Val Nothing (V.BLeaf (V.LVar varToReplace)))
                & V.BRecExtend & Val Nothing
                & V.RecExtend prevParamTag arg
                & V.BRecExtend & Val Nothing
        return NewLet
            { nlIRef = redexArg redex ^. V.payload & Property.value
            , nlOnVar = id
            , nlOnArgToVar = onArg
            , nlMVarToTags =
                Just VarToTags
                { vttNewTag = tagGForLambdaTagParam lamVar newParamTag
                , vttReplacedVar = lamVar
                , vttReplacedByTag = tagGForLambdaTagParam lamVar prevParamTag
                , vttReplacedVarEntityId = EntityId.ofLambdaParam lamVar
                }
            }

addLetParam ::
    Monad m => V.Var -> Redex (ValIProperty m) -> Transaction m (NewLet m)
addLetParam varToReplace redex =
    case redexArg redex ^. V.body of
    V.BAbs lam | isVarAlwaysApplied (redexParam redex) (redexBody redex) ->
        case redexArgType redex of
        T.TFun (T.TRecord _) _
            | isVarAlwaysRecordOfGetField
                (lam ^. V.lamParamId) (lam ^. V.lamResult) ->
            error "TODO addFieldToLetParams"
        _ -> convertLetParamToRecord varToReplace redex lam
    _ -> convertLetToLam varToReplace redex

sameLet :: Redex (ValIProperty m) -> NewLet m
sameLet redex =
    NewLet
    { nlIRef = redexArg redex ^. V.payload & Property.value
    , nlOnVar = id
    , nlOnArgToVar = id
    , nlMVarToTags = Nothing
    }

floatLetToOuterScope ::
    MonadA m =>
    ValIProperty m -> Redex (ValIProperty m) -> ConvertM.Context m ->
    Transaction m LetFloatResult
floatLetToOuterScope topLevelProp redex ctx =
    do
        newLet <-
            case varsToRemove of
            [] -> sameLet redex & return
            [x] -> addLetParam x redex
            _ -> error "multiple osiVarsUnderPos not expected!?"
        (newLeafBody, resultEntity) <-
            case outerScopeInfo ^. ConvertM.osiPos of
            Nothing ->
                do
                    newDefI <-
                        moveToGlobalScope ctx
                        (redexParam redex) (redexArg redex) (nlIRef newLet)
                    return
                        ( ExprIRef.globalId newDefI & V.LGlobal
                        , EntityId.ofIRef newDefI
                        )
            Just outerScope ->
                ( V.LVar (redexParam redex)
                , EntityId.ofLambdaParam (redexParam redex)
                ) <$
                DataOps.redexWrapWithGivenParam
                    (redexParam redex) (nlIRef newLet) outerScope
        let newBody = V.BLeaf newLeafBody
        let
            go (Val s (V.BLeaf (V.LVar v))) | v == redexParam redex =
                nlOnVar newLet (Val s newBody)
            go (Val s (V.BApp (V.Apply f@(Val _ (V.BLeaf (V.LVar v))) a)))
              | v == redexParam redex =
                V.Apply (go f) (nlOnArgToVar newLet a)
                & V.BApp & Val s
            go val = val & V.body . Lens.mapped %~ go
        _ <-
            redexBody redex
            <&> Just . Property.value
            & go
            & V.payload .~ Just (Property.value topLevelProp)
            <&> flip (,) ()
            & ExprIRef.writeValWithStoredSubexpressions
        return LetFloatResult
            { lfrNewEntity = resultEntity
            , lfrMVarToTags = nlMVarToTags newLet
            }
    where
        varsToRemove =
            filter (`Set.member` usedVars)
            (outerScopeInfo ^. ConvertM.osiVarsUnderPos)
        outerScopeInfo = ctx ^. ConvertM.scScopeInfo . ConvertM.siOuter
        usedVars =
            redexArg redex ^.. ExprLens.valLeafs . ExprLens._LVar
            & Set.fromList

makeFloatLetToOuterScope ::
    MonadA m =>
    ValIProperty m -> Redex (ValIProperty m) ->
    ConvertM m (Transaction m LetFloatResult)
makeFloatLetToOuterScope topLevelProp redex =
    ConvertM.readContext <&> floatLetToOuterScope topLevelProp redex
