{-# LANGUAGE PatternGuards, NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Float
    ( makeFloatLetToOuterScope
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import           Lamdu.Expr.Val.Annotated (Val(..))
import qualified Lamdu.Expr.Val.Annotated as Val
import qualified Lamdu.Sugar.Convert.Binder.Params as Params
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderedClosedFlatComposite)
import           Lamdu.Sugar.Types

import           Prelude.Compat

type T = Transaction

moveToGlobalScope :: Monad m => ConvertM.Context m -> V.Var -> ValI m -> T m ()
moveToGlobalScope ctx param letI =
    DataOps.newPublicDefinitionToIRef
    (ctx ^. ConvertM.scCodeAnchors) letI (ExprIRef.defI param)

data NewLet m = NewLet
    { nlIRef :: ValI m
    , nlMVarToTags :: Maybe VarToTags
    }

isVarAlwaysApplied :: V.Var -> Val a -> Bool
isVarAlwaysApplied var =
    go False
    where
        go isApplied (Val _ (V.BLeaf (V.LVar v))) | v == var = isApplied
        go _ (Val _ (V.BApp (V.Apply f a))) = go True f && go False a
        go _ v = all (go False) (v ^.. Val.body . Lens.traverse)

convertLetToLam ::
    Monad m => V.Var -> Redex (ValIProperty m) -> T m (NewLet m)
convertLetToLam varToReplace redex =
    do
        (ParamAddResultNewVar _ newParam, newValI) <-
            Params.convertBinderToFunction mkArg
            (BinderKindLet (redexLam redex)) (redexArg redex)
        let toNewParam prop =
                V.LVar newParam & V.BLeaf &
                ExprIRef.writeValBody (Property.value prop)
        SubExprs.onGetVars toNewParam varToReplace (redexArg redex)
        return NewLet
            { nlIRef = newValI
            , nlMVarToTags = Nothing
            }
    where
        mkArg = V.LVar varToReplace & V.BLeaf & ExprIRef.newValBody

convertVarToGetFieldParam ::
    Monad m =>
    V.Var -> T.Tag -> V.Lam (Val (ValIProperty m)) -> T m ()
convertVarToGetFieldParam oldVar paramTag (V.Lam lamVar lamBody) =
    SubExprs.onGetVars toNewParam oldVar lamBody
    where
        toNewParam prop =
            V.LVar lamVar & V.BLeaf
            & ExprIRef.newValBody
            <&> (`V.GetField` paramTag) <&> V.BGetField
            >>= ExprIRef.writeValBody (Property.value prop)

convertLetParamToRecord ::
    Monad m =>
    V.Var -> V.Lam (Val (ValIProperty m)) -> Params.StoredLam m -> T m (NewLet m)
convertLetParamToRecord varToReplace letLam storedLam =
    do
        vtt <-
            Params.convertToRecordParams
            mkNewArg (BinderKindLet letLam) storedLam Params.NewParamAfter
        convertVarToGetFieldParam varToReplace (vttNewTag vtt ^. tagVal)
            (storedLam ^. Params.slLam)
        return NewLet
            { nlIRef = Params.slLambdaProp storedLam & Property.value
            , nlMVarToTags = Just vtt
            }
    where
        mkNewArg = V.LVar varToReplace & V.BLeaf & ExprIRef.newValBody

addFieldToLetParamsRecord ::
    Monad m =>
    [T.Tag] -> V.Var -> V.Lam (Val (ValIProperty m)) -> Params.StoredLam m ->
    T m (NewLet m)
addFieldToLetParamsRecord fieldTags varToReplace letLam storedLam =
    do
        newParamTag <-
            Params.addFieldParam mkNewArg (BinderKindLet letLam)
            ((fieldTags ++) . return) storedLam
        convertVarToGetFieldParam varToReplace (newParamTag ^. tagVal)
            (storedLam ^. Params.slLam)
        return NewLet
            { nlIRef = Params.slLambdaProp storedLam & Property.value
            , nlMVarToTags = Nothing
            }
    where
        mkNewArg = V.LVar varToReplace & V.BLeaf & ExprIRef.newValBody

addLetParam ::
    Monad m => V.Var -> Redex (ValIProperty m) -> T m (NewLet m)
addLetParam varToReplace redex =
    case redexArg redex ^. Val.body of
    V.BLam lam | isVarAlwaysApplied param body ->
        case redexArgType redex of
        T.TFun (T.TRecord composite) _
            | Just fields <- composite ^? orderedClosedFlatComposite
            , Params.isParamAlwaysUsedWithGetField lam ->
            addFieldToLetParamsRecord
                (fields <&> fst) varToReplace (redexLam redex) storedLam
        _ -> convertLetParamToRecord varToReplace (redexLam redex) storedLam
        where
            storedLam = Params.StoredLam lam (redexArg redex ^. Val.payload)
    _ -> convertLetToLam varToReplace redex
    where
        V.Lam param body = redexLam redex

sameLet :: Redex (ValIProperty m) -> NewLet m
sameLet redex =
    NewLet
    { nlIRef = redexArg redex ^. Val.payload & Property.value
    , nlMVarToTags = Nothing
    }

floatLetToOuterScope ::
    Monad m =>
    ValIProperty m -> Redex (ValIProperty m) -> ConvertM.Context m ->
    T m LetFloatResult
floatLetToOuterScope topLevelProp redex ctx =
    do
        newLet <-
            case varsToRemove of
            [] -> sameLet redex & return
            [x] -> addLetParam x redex
            _ -> error "multiple osiVarsUnderPos not expected!?"
        redexLam redex ^. V.lamResult . Val.payload . Property.pVal
            & Property.set topLevelProp
        resultEntity <-
            case outerScopeInfo ^. ConvertM.osiPos of
            Nothing ->
                EntityId.ofIRef (ExprIRef.defI param) <$
                moveToGlobalScope ctx param (nlIRef newLet)
            Just outerScope ->
                EntityId.ofLambdaParam param <$
                DataOps.redexWrapWithGivenParam param (nlIRef newLet) outerScope
        return LetFloatResult
            { lfrNewEntity = resultEntity
            , lfrMVarToTags = nlMVarToTags newLet
            }
    where
        param = redexLam redex ^. V.lamParamId
        varsToRemove =
            filter (`Set.member` usedVars)
            (outerScopeInfo ^. ConvertM.osiVarsUnderPos)
        outerScopeInfo = ctx ^. ConvertM.scScopeInfo . ConvertM.siOuter
        usedVars =
            redexArg redex ^.. ExprLens.valLeafs . ExprLens._LVar
            & Set.fromList

makeFloatLetToOuterScope ::
    Monad m =>
    ValIProperty m -> Redex (ValIProperty m) ->
    ConvertM m (T m LetFloatResult)
makeFloatLetToOuterScope topLevelProp redex =
    ConvertM.readContext <&> floatLetToOuterScope topLevelProp redex
