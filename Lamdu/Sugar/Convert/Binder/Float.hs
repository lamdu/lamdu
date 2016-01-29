{-# LANGUAGE PatternGuards, NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Binder.Params as Params
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderedClosedFlatComposite)
import           Lamdu.Sugar.Types

import           Prelude.Compat

moveToGlobalScope ::
    MonadA m => ConvertM.Context m -> V.Var -> ValI m -> Transaction m (DefI m)
moveToGlobalScope ctx param letI =
    do
        paramName <- Anchors.assocNameRef param & Transaction.getP
        DataOps.newPublicDefinitionWithPane paramName
            (ctx ^. ConvertM.scCodeAnchors) letI

data NewLet m = NewLet
    { nlIRef :: ValI m
    , nlOnVar :: Val (Maybe (ValI m)) -> Val (Maybe (ValI m))
    , nlOnArgToVar :: Val (Maybe (ValI m)) -> Val (Maybe (ValI m))
    , nlMVarToTags :: Maybe VarToTags
    }

isVarAlwaysApplied :: V.Var -> Val a -> Bool
isVarAlwaysApplied var val =
    case val ^. V.body of
    V.BLeaf (V.LVar v) | v == var -> False
    V.BApp (V.Apply f a) -> checkChildren f && isVarAlwaysApplied var a
    _ -> checkChildren val
    where
        checkChildren x =
            all (isVarAlwaysApplied var) (x ^.. V.body . Lens.traverse)

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

convertVarToGetFieldParam ::
    Monad m =>
    V.Var -> T.Tag -> V.Lam (Val (ValIProperty m)) -> Transaction m ()
convertVarToGetFieldParam oldVar paramTag (V.Lam lamVar lamBody) =
    SubExprs.onGetVars toNewParam oldVar lamBody
    where
        toNewParam prop =
            V.LVar lamVar & V.BLeaf
            & ExprIRef.newValBody
            <&> (`V.GetField` paramTag) <&> V.BGetField
            >>= ExprIRef.writeValBody (Property.value prop)

convertLetParamToRecord ::
    Monad m => V.Var -> Params.StoredLam m -> Transaction m (NewLet m)
convertLetParamToRecord varToReplace storedLam =
    do
        vtt <- Params.convertToRecordParams storedLam Params.NewParamAfter
        let newParamTag = vttNewTag vtt ^. tagVal
        convertVarToGetFieldParam varToReplace newParamTag
            (storedLam ^. Params.slLam)
        let onArg arg =
                V.BLeaf V.LRecEmpty & Val Nothing
                & V.RecExtend newParamTag
                    (Val Nothing (V.BLeaf (V.LVar varToReplace)))
                & V.BRecExtend & Val Nothing
                & V.RecExtend (vttReplacedByTag vtt ^. tagVal) arg
                & V.BRecExtend & Val Nothing
        return NewLet
            { nlIRef = Params.slLambdaProp storedLam & Property.value
            , nlOnVar = id
            , nlOnArgToVar = onArg
            , nlMVarToTags = Just vtt
            }

addFieldToLetParamsRecord ::
    Monad m =>
    [T.Tag] -> V.Var -> Params.StoredLam m -> Transaction m (NewLet m)
addFieldToLetParamsRecord fieldTags varToReplace storedLam =
    do
        newParamTag <- Params.addFieldParam ((fieldTags ++) . return) storedLam
        convertVarToGetFieldParam varToReplace (newParamTag ^. tagVal)
            (storedLam ^. Params.slLam)
        return NewLet
            { nlIRef = Params.slLambdaProp storedLam & Property.value
            , nlOnVar = id
            , nlOnArgToVar =
                Val Nothing
                . V.BRecExtend
                . V.RecExtend (newParamTag ^. tagVal)
                    (V.LVar varToReplace & V.BLeaf & Val Nothing)
            , nlMVarToTags = Nothing
            }

addLetParam ::
    Monad m => V.Var -> Redex (ValIProperty m) -> Transaction m (NewLet m)
addLetParam varToReplace redex =
    case redexArg redex ^. V.body of
    V.BAbs lam | isVarAlwaysApplied (redexParam redex) (redexBody redex) ->
        case redexArgType redex of
        T.TFun (T.TRecord composite) _
            | Just fields <- composite ^? orderedClosedFlatComposite
            , Params.isParamAlwaysUsedWithGetField lam ->
            addFieldToLetParamsRecord (fields <&> fst) varToReplace storedLam
        _ -> convertLetParamToRecord varToReplace storedLam
        where
            storedLam = Params.StoredLam lam (redexArg redex ^. V.payload)
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
                        (redexParam redex) (nlIRef newLet)
                    return
                        ( ExprIRef.globalId newDefI & V.LVar
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
