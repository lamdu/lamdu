{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, RecordWildCards, PatternGuards #-}
module Lamdu.Sugar.Convert.Binder.Params
    ( ConventionalParams(..), cpParams, cpAddFirstParam
    , convertParams, convertLamParams
    , mkStoredLam, makeDeleteLambda
    , convertBinderToFunction
    , convertToRecordParams
    , StoredLam(..), slLam
    , NewParamPosition(..), addFieldParam
    , isParamAlwaysUsedWithGetField
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (transaction)
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import           Data.Maybe.Utils (unsafeUnjust)
import qualified Data.Set as Set
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, MkProperty)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.Eval.Results.Process as ResultsProcess
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.ParamList (ParamList)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.OrderTags (orderType, orderedClosedFlatComposite)
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

data ConventionalParams m = ConventionalParams
    { cpTags :: Set T.Tag
    , _cpParamInfos :: Map T.Tag ConvertM.TagFieldParam
    , _cpParams :: BinderParams UUID m
    , _cpAddFirstParam :: T m ParamAddResult
    , cpScopes :: BinderBodyScope
    , cpMLamParam :: Maybe V.Var
    }
Lens.makeLenses ''ConventionalParams

data FieldParam = FieldParam
    { fpTag :: T.Tag
    , fpFieldType :: Type
    , fpValue :: CurAndPrev (Map ER.ScopeId [(ER.ScopeId, ER.Val Type)])
    }

data StoredLam m = StoredLam
    { _slLam :: V.Lam (Val (ValIProperty m))
    , slLambdaProp :: ValIProperty m
    }
Lens.makeLenses ''StoredLam

slParamList :: Monad m => StoredLam m -> Transaction.MkProperty m (Maybe ParamList)
slParamList = Anchors.assocFieldParamList . Property.value . slLambdaProp

mkStoredLam ::
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> StoredLam m
mkStoredLam lam pl =
    StoredLam
    (lam & Lens.mapped . Lens.mapped %~ (^. Input.stored))
    (pl ^. Input.stored)

newTag :: Monad m => T m T.Tag
newTag = GenIds.transaction GenIds.randomTag

setParamList :: Monad m => MkProperty m (Maybe [T.Tag]) -> [T.Tag] -> T m ()
setParamList paramListProp newParamList =
    do
        zip newParamList [0..] & mapM_ (uncurry setParamOrder)
        Just newParamList & Transaction.setP paramListProp
    where
        setParamOrder = Transaction.setP . Anchors.assocTagOrder

isArgOfCallTo :: V.Var -> [Val ()] -> Bool
isArgOfCallTo funcVar (cur : parent : _) =
    not (Lens.has varT cur) &&
    Lens.has (ExprLens.valApply . V.applyFunc . varT) parent
    where
        varT = ExprLens.valVar . Lens.only funcVar
isArgOfCallTo _ _ = False

isUnappliedVar :: V.Var -> [Val ()] -> Bool
isUnappliedVar var (cur : parent : _) =
    Lens.has varT cur
    -- Var could not be both the arg and the func (will be type error)
    && not (Lens.has (ExprLens.valApply . V.applyFunc . varT) parent)
    && not (Lens.has (ExprLens.valApply . V.applyFunc . ExprLens.valHole) parent)
    where
        varT = ExprLens.valVar . Lens.only var
isUnappliedVar var [cur] = Lens.has (ExprLens.valVar . Lens.only var) cur
isUnappliedVar _ _ = False

wrapUnappliedUsesOfVar :: Monad m => V.Var -> Val (ValIProperty m) -> T m ()
wrapUnappliedUsesOfVar var =
    SubExprs.onMatchingSubexprsWithPath (DataOps.wrap <&> void) (isUnappliedVar var)

changeCallArgs ::
    Monad m =>
    (ValI m -> T m (ValI m)) -> Val (ValIProperty m) -> V.Var -> T m ()
changeCallArgs change val var  =
    do
        SubExprs.onMatchingSubexprsWithPath changeArg (isArgOfCallTo var) val
        wrapUnappliedUsesOfVar var val
    where
        changeArg prop =
            Property.value prop & change >>= Property.set prop

fixUsagesOfLamBinder ::
    Monad m => (ValI m -> T m (ValI m)) -> BinderKind m -> StoredLam m -> T m ()
fixUsagesOfLamBinder fixOp binderKind storedLam =
    case binderKind of
    BinderKindDef defI ->
        changeCallArgs fixOp (storedLam ^. slLam . V.lamResult) (ExprIRef.globalId defI)
    BinderKindLet redexLam ->
        changeCallArgs fixOp (redexLam ^. V.lamResult) (redexLam ^. V.lamParamId)
    BinderKindLambda -> return ()

addFieldParam ::
    Monad m =>
    T m (ValI m) -> BinderKind m -> (T.Tag -> ParamList) -> StoredLam m ->
    T m (Tag ())
addFieldParam mkArg binderKind mkNewTags storedLam =
    do
        tag <- newTag
        let tagS =
                Tag
                { _tagInstance =
                    EntityId.ofLambdaTagParam
                    (storedLam ^. slLam . V.lamParamId) tag
                , _tagVal = tag
                , _tagName = ()
                }
        mkNewTags tag & setParamList (slParamList storedLam)
        let addFieldToCall argI =
                do
                    newArg <- mkArg
                    V.RecExtend (tagS ^. tagVal) newArg argI
                        & V.BRecExtend & ExprIRef.newValBody
        fixUsagesOfLamBinder addFieldToCall binderKind storedLam
        return tagS

mkCpScopesOfLam :: Input.Payload m a -> CurAndPrev (Map ER.ScopeId [BinderParamScopeId])
mkCpScopesOfLam x =
    x ^. Input.evalResults <&> (^. Input.eAppliesOfLam) <&> (fmap . fmap) fst
    <&> (fmap . map) BinderParamScopeId

getFieldOnVar :: Lens.Traversal' (Val t) (V.Var, T.Tag)
getFieldOnVar = Val.body . V._BGetField . inGetField
    where
        inGetField f (V.GetField (Val pl (V.BLeaf (V.LVar v))) t) =
            f (v, t) <&> pack pl
        inGetField _ other = pure other
        pack pl (v, t) =
            V.GetField (Val pl (V.BLeaf (V.LVar v))) t

getFieldParamsToHole :: Monad m => T.Tag -> V.Lam (Val (ValIProperty m)) -> T m ()
getFieldParamsToHole tag (V.Lam param lamBody) =
    SubExprs.onMatchingSubexprs SubExprs.toHole (getFieldOnVar . Lens.only (param, tag)) lamBody

getFieldParamsToParams :: Monad m => V.Lam (Val (ValIProperty m)) -> T.Tag -> T m ()
getFieldParamsToParams (V.Lam param lamBody) tag =
    SubExprs.onMatchingSubexprs (toParam . Property.value)
    (getFieldOnVar . Lens.only (param, tag)) lamBody
    where
        toParam bodyI = ExprIRef.writeValBody bodyI $ V.BLeaf $ V.LVar param

fixCallArgRemoveField :: Monad m => T.Tag -> ValI m -> T m (ValI m)
fixCallArgRemoveField tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> return restI
                | otherwise ->
                    do
                        newRestI <- fixCallArgRemoveField tag restI
                        when (newRestI /= restI) $
                            ExprIRef.writeValBody argI $
                            V.BRecExtend $ V.RecExtend t v newRestI
                        return argI
            _ -> return argI

fixCallToSingleArg ::
    Monad m => T.Tag -> ValI m -> T m (ValI m)
fixCallToSingleArg tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> return v
                | otherwise -> fixCallToSingleArg tag restI
            _ -> return argI

tagForLambdaTagParam :: V.Var -> T.Tag -> Tag ()
tagForLambdaTagParam paramVar tag = Tag (EntityId.ofLambdaTagParam paramVar tag) tag ()

delFieldParamAndFixCalls ::
    Monad m =>
    BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m -> T m ParamDelResult
delFieldParamAndFixCalls binderKind tags fp storedLam =
    do
        Transaction.setP (slParamList storedLam) newTags
        getFieldParamsToHole tag (storedLam ^. slLam)
        mLastTag
            & traverse_ (getFieldParamsToParams (storedLam ^. slLam))
        fixUsagesOfLamBinder fixRecurseArg binderKind storedLam
        return delResult
    where
        paramVar = storedLam ^. slLam . V.lamParamId
        tag = fpTag fp
        fixRecurseArg =
            maybe (fixCallArgRemoveField tag)
            fixCallToSingleArg mLastTag
        (newTags, mLastTag, delResult) =
            case List.delete tag tags of
            [x] ->
                ( Nothing
                , Just x
                , ParamDelResultTagsToVar TagsToVar
                    { ttvReplacedTag = tagForLambdaTagParam paramVar x
                    , ttvReplacedByVar = paramVar
                    , ttvReplacedByVarEntityId = EntityId.ofLambdaParam paramVar
                    }
                )
            xs -> (Just xs, Nothing, ParamDelResultDelTag)

fieldParamActions ::
    Monad m =>
    BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m -> FuncParamActions m
fieldParamActions binderKind tags fp storedLam =
    FuncParamActions
    { _fpAddNext =
        addFieldParam DataOps.newHole binderKind mkNewTags storedLam
        <&> ParamAddResultNewTag
    , _fpDelete = delFieldParamAndFixCalls binderKind tags fp storedLam
    , _fpMOrderBefore =
        case tagsBefore of
        [] -> Nothing
        b ->
            init b ++ (fpTag fp : last b : tagsAfter)
            & setParamList (slParamList storedLam) & Just
    , _fpMOrderAfter =
        case tagsAfter of
        [] -> Nothing
        (x:xs) ->
            tagsBefore ++ (x : fpTag fp : xs)
            & setParamList (slParamList storedLam) & Just
    }
    where
        (tagsBefore, _:tagsAfter) = break (== fpTag fp) tags
        mkNewTags tag = tagsBefore ++ [fpTag fp, tag] ++ tagsAfter

fpIdEntityId :: V.Var -> FieldParam -> EntityId
fpIdEntityId param = EntityId.ofLambdaTagParam param . fpTag

mkParamInfo :: V.Var -> FieldParam -> Map T.Tag ConvertM.TagParamInfo
mkParamInfo param fp =
    fpIdEntityId param fp
    & ConvertM.TagParamInfo param
    & Map.singleton (fpTag fp)

convertRecordParams ::
    Monad m =>
    BinderKind m -> [FieldParam] ->
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConventionalParams m
convertRecordParams binderKind fieldParams lam@(V.Lam param _) pl =
    ConventionalParams
    { cpTags = Set.fromList tags
    , _cpParamInfos = fieldParams <&> mkFieldParamInfo & mconcat
    , _cpParams = FieldParams (fieldParams <&> mkParam)
    , _cpAddFirstParam =
        addFieldParam DataOps.newHole binderKind (:tags) storedLam
        <&> ParamAddResultNewTag
    , cpScopes = BinderBodyScope $ mkCpScopesOfLam pl
    , cpMLamParam = Just param
    }
    where
        tags = fieldParams <&> fpTag
        mkFieldParamInfo fp = mkParamInfo param fp <&> ConvertM.TagFieldParam
        storedLam = mkStoredLam lam pl
        mkParam fp =
            FuncParam
            { _fpInfo =
                FieldParamInfo
                { _fpiActions = fieldParamActions binderKind tags fp storedLam
                , _fpiTag =
                    Tag
                    { _tagInstance = fpIdEntityId param fp
                    , _tagVal = fpTag fp
                    , _tagName = UniqueId.toUUID $ fpTag fp
                    }
                }
            , _fpAnnotation =
                Annotation
                { _aInferredType = fpFieldType fp
                , _aMEvaluationResult =
                    fpValue fp <&>
                    \x ->
                    do
                        Map.null x & not & guard
                        x ^.. Lens.traversed . Lens.traversed
                            & Map.fromList & Just
                }
            }

removeCallsToVar :: Monad m => V.Var -> Val (ValIProperty m) -> T m ()
removeCallsToVar funcVar val =
    do
        SubExprs.onMatchingSubexprs changeRecursion
            ( Val.body . V._BApp . V.applyFunc . ExprLens.valVar
            . Lens.only funcVar
            ) val
        wrapUnappliedUsesOfVar funcVar val
    where
        changeRecursion prop =
            do
                body <- ExprIRef.readValBody (Property.value prop)
                case body of
                    V.BApp (V.Apply f _) -> Property.set prop f
                    _ -> error "assertion: expected BApp"

makeDeleteLambda ::
    Monad m => BinderKind m -> StoredLam m ->
    ConvertM m (T m ParamDelResult)
makeDeleteLambda binderKind (StoredLam (V.Lam paramVar lamBodyStored) lambdaProp) =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        return $
            do
                SubExprs.getVarsToHole paramVar lamBodyStored
                case binderKind of
                    BinderKindDef defI ->
                        removeCallsToVar
                        (ExprIRef.globalId defI) lamBodyStored
                    BinderKindLet redexLam ->
                        removeCallsToVar
                        (redexLam ^. V.lamParamId) (redexLam ^. V.lamResult)
                    BinderKindLambda -> return ()
                let lamBodyI = Property.value (lamBodyStored ^. Val.payload)
                _ <- protectedSetToVal lambdaProp lamBodyI
                return ParamDelResultDelVar

convertVarToGetField ::
    Monad m => T.Tag -> V.Var -> Val (Property (T m) (ValI m)) -> T m ()
convertVarToGetField tagForVar paramVar =
    SubExprs.onGetVars (convertVar . Property.value) paramVar
    where
        convertVar bodyI =
            ExprIRef.newValBody (V.BLeaf (V.LVar paramVar))
            <&> (`V.GetField` tagForVar) <&> V.BGetField
            >>= ExprIRef.writeValBody bodyI

wrapArgWithRecord ::
    Monad m => T m (ValI m) -> VarToTags -> ValI m -> T m (ValI m)
wrapArgWithRecord mkNewArg varToTags oldArg =
    do
        newArg <- mkNewArg
        ExprIRef.newValBody (V.BLeaf V.LRecEmpty)
            >>= ExprIRef.newValBody . V.BRecExtend
                . V.RecExtend (vttNewTag varToTags ^. tagVal) newArg
            >>= ExprIRef.newValBody . V.BRecExtend
                . V.RecExtend (vttReplacedByTag varToTags ^. tagVal) oldArg

data NewParamPosition = NewParamBefore | NewParamAfter

convertToRecordParams ::
    Monad m =>
    T m (ValI m) -> BinderKind m -> StoredLam m -> NewParamPosition ->
    T m VarToTags
convertToRecordParams mkNewArg binderKind storedLam newParamPosition =
    do
        tagForVar <- newTag
        tagForNewVar <- newTag
        let
            varToTags =
                VarToTags
                { vttReplacedVar = paramVar
                , vttReplacedVarEntityId = EntityId.ofLambdaParam paramVar
                , vttReplacedByTag = tagForLambdaTagParam paramVar tagForVar
                , vttNewTag = tagForLambdaTagParam paramVar tagForNewVar
                }
        case newParamPosition of
            NewParamBefore -> [tagForNewVar, tagForVar]
            NewParamAfter -> [tagForVar, tagForNewVar]
            & setParamList paramList
        convertVarToGetField tagForVar paramVar
            (storedLam ^. slLam . V.lamResult)
        fixUsagesOfLamBinder (wrapArgWithRecord mkNewArg varToTags)
            binderKind storedLam
        return varToTags
    where
        paramVar = storedLam ^. slLam . V.lamParamId
        paramList =
            slLambdaProp storedLam & Property.value
            & Anchors.assocFieldParamList

lamParamType :: Input.Payload m a -> Type
lamParamType lamExprPl =
    unsafeUnjust "Lambda value not inferred to a function type?!" $
    lamExprPl ^? Input.inferredType . T._TFun . _1

makeNonRecordParamActions ::
    Monad m => BinderKind m -> StoredLam m -> ConvertM m (FuncParamActions m)
makeNonRecordParamActions binderKind storedLam =
    do
        delete <- makeDeleteLambda binderKind storedLam
        return
            FuncParamActions
            { _fpAddNext =
                convertToRecordParams DataOps.newHole
                binderKind storedLam NewParamAfter
                <&> ParamAddResultVarToTags
            , _fpDelete = delete
            , _fpMOrderBefore = Nothing
            , _fpMOrderAfter = Nothing
            }

mkFuncParam :: Monad m => Input.Payload m a -> info -> ConvertM m (FuncParam info)
mkFuncParam lamExprPl info =
    do
        noms <- ConvertM.readContext <&> (^. ConvertM.scNominalsMap)
        return FuncParam
            { _fpInfo = info
            , _fpAnnotation =
                Annotation
                { _aInferredType = typ
                , _aMEvaluationResult =
                    lamExprPl ^. Input.evalResults
                    <&> (^. Input.eAppliesOfLam)
                    <&> \lamApplies ->
                    do
                        Map.null lamApplies & not & guard
                        lamApplies ^..
                            Lens.traversed . Lens.traversed & Map.fromList
                            <&> ResultsProcess.addTypes noms typ
                            & Just
                }
            }
    where
        typ = lamParamType lamExprPl

convertNonRecordParam ::
    Monad m => BinderKind m ->
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertNonRecordParam binderKind lam@(V.Lam param _) lamExprPl =
    do
        funcParamActions <- makeNonRecordParamActions binderKind storedLam
        funcParam <-
            case lamParamType lamExprPl of
            T.TRecord T.CEmpty
                | null (lamExprPl ^. Input.varRefsOfLambda) ->
                  funcParamActions ^. fpDelete
                  & void
                  & NullParamActions
                  & mkFuncParam lamExprPl
                  <&> NullParam
            _ ->
                VarParamInfo
                { _vpiName = UniqueId.toUUID param
                , _vpiActions = funcParamActions
                , _vpiId = paramEntityId
                } & mkFuncParam lamExprPl
                <&> VarParam
        pure ConventionalParams
            { cpTags = mempty
            , _cpParamInfos = Map.empty
            , _cpParams = funcParam
            , _cpAddFirstParam =
                convertToRecordParams DataOps.newHole
                binderKind storedLam NewParamBefore
                <&> ParamAddResultVarToTags
            , cpScopes = BinderBodyScope $ mkCpScopesOfLam lamExprPl
            , cpMLamParam = Just param
            }
    where
        storedLam = mkStoredLam lam lamExprPl
        paramEntityId = EntityId.ofLambdaParam param

isParamAlwaysUsedWithGetField :: V.Lam (Val a) -> Bool
isParamAlwaysUsedWithGetField (V.Lam param body) =
    go False body
    where
        go isGetFieldChild val =
            case val ^. Val.body of
            V.BLeaf (V.LVar v) | v == param -> isGetFieldChild
            V.BGetField (V.GetField r _) -> go True r
            x -> all (go False) (x ^.. Lens.traverse)

-- Post process param add and delete actions to wrap lambda in hole.
-- This isn't done for all actions as some already perform this function.
-- TODO: clean up responsibilities - make it clear why some actions already
-- take care of wrapping and some don't.
postProcessActions ::
    Monad m => T m () -> ConventionalParams m -> ConventionalParams m
postProcessActions post x =
    x
    & cpAddFirstParam %~ (<* post)
    & cpParams . SugarLens.binderFuncParamActions . fpAddNext %~ (<* post)
    & if Lens.has (cpParams . _FieldParams) x
        then cpParams . SugarLens.binderFuncParamActions . fpDelete %~ (<* post)
        else id

convertLamParams ::
    Monad m =>
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertLamParams lambda lambdaPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let maybeWrap = protectedSetToVal lambdaProp (Property.value lambdaProp)
        convertNonEmptyParams BinderKindLambda lambda lambdaPl
            <&> postProcessActions (void maybeWrap)
    where
        lambdaProp = lambdaPl ^. Input.stored

convertNonEmptyParams ::
    Monad m =>
    BinderKind m -> V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertNonEmptyParams binderKind lambda lambdaPl =
    do
        ctx <- ConvertM.readContext
        let tagsInOuterScope =
                ctx ^. ConvertM.scScopeInfo . ConvertM.siTagParamInfos
                & Map.keysSet
        let noms = ctx ^. ConvertM.scNominalsMap
        let makeFieldParam (tag, typeExpr) =
                FieldParam
                { fpTag = tag
                , fpFieldType = typeExpr
                , fpValue =
                        lambdaPl ^. Input.evalResults
                        <&> (^. Input.eAppliesOfLam)
                        <&> Lens.traversed . Lens.mapped . _2 %~
                            ResultsProcess.addTypes noms typeExpr .
                            ER.extractField tag
                        <&> Lens.traversed %~
                            filter (Lens.nullOf (_2 . ER.body . ER._RError))
                }
        orderedType <-
            lambdaPl ^. Input.inferredType & orderType & transaction
        case orderedType of
            T.TFun (T.TRecord composite) _
                | Just fields <- composite ^? orderedClosedFlatComposite
                , ListUtils.isLengthAtLeast 2 fields
                , isParamAlwaysUsedWithGetField lambda
                , let myTags = fields <&> fst & Set.fromList
                , let fieldParams = map makeFieldParam fields
                -> if Set.null (tagsInOuterScope `Set.intersection` myTags)
                      && Set.null (tagsInInnerScope `Set.intersection` myTags)
                   then convertRecordParams binderKind fieldParams lambda lambdaPl & return
                   else
                       convertNonRecordParam binderKind lambda lambdaPl
                       <&> cpParamInfos <>~ (fieldParams & map mkCollidingInfo & mconcat)
                where
                    tagsInInnerScope =
                        lambda ^.. V.lamResult . ExprLens.subExprs
                        . Lens.filtered (Lens.has ExprLens.valAbs)
                        . Val.payload
                        . Input.inferredType . T._TFun . _1
                        . T._TRecord . ExprLens.compositeFieldTags
                        & Set.fromList
            _ -> convertNonRecordParam binderKind lambda lambdaPl
    where
        param = lambda ^. V.lamParamId
        mkCollidingInfo fp = mkParamInfo param fp <&> ConvertM.CollidingFieldParam

convertVarToCalls ::
    Monad m => T m (ValI m) -> V.Var -> Val (ValIProperty m) -> T m ()
convertVarToCalls mkArg var =
    SubExprs.onMatchingSubexprs change (ExprLens.valVar . Lens.only var)
    where
        change prop =
            mkArg
            >>= ExprIRef.newValBody . V.BApp . V.Apply (Property.value prop)
            >>= Property.set prop

convertBinderToFunction ::
    Monad m =>
    T m (ValI m) -> BinderKind m -> Val (ValIProperty m) ->
    T m (ParamAddResult, ValI m)
convertBinderToFunction mkArg binderKind val =
    do
        (newParam, newValI) <- DataOps.lambdaWrap (val ^. Val.payload)
        case binderKind of
            BinderKindDef defI ->
                convertVarToCalls mkArg (ExprIRef.globalId defI) val
            BinderKindLet redexLam ->
                convertVarToCalls mkArg
                (redexLam ^. V.lamParamId) (redexLam ^. V.lamResult)
            BinderKindLambda -> error "Lambda will never be an empty-params binder"
        return
            ( ParamAddResultNewVar (EntityId.ofLambdaParam newParam) newParam
            , newValI
            )

convertEmptyParams ::
    Monad m => BinderKind m -> Val (Input.Payload m a) -> ConventionalParams m
convertEmptyParams binderKind val =
    ConventionalParams
    { cpTags = mempty
    , _cpParamInfos = Map.empty
    , _cpParams = BinderWithoutParams
    , _cpAddFirstParam =
        val <&> (^. Input.stored)
        & convertBinderToFunction DataOps.newHole binderKind
        <&> fst
    , cpScopes = SameAsParentScope
    , cpMLamParam = Nothing
    }

convertParams ::
    Monad m =>
    BinderKind m -> Val (Input.Payload m a) ->
    ConvertM m
    ( ConventionalParams m
    , Val (Input.Payload m a)
    )
convertParams binderKind expr =
    do
        postProcess <- ConvertM.postProcess
        case expr ^. Val.body of
            V.BLam lambda ->
                convertNonEmptyParams binderKind lambda (expr ^. Val.payload)
                <&> flip (,) (lambda ^. V.lamResult)
            _ -> return (convertEmptyParams binderKind expr, expr)
            <&> _1 %~ postProcessActions postProcess
