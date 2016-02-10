{-# LANGUAGE NoImplicitPrelude, RecordWildCards, PatternGuards #-}
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

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, void, when)
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev)
import           Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe.Utils (unsafeUnjust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Guid (Guid)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, MkProperty)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.Eval.Results.Process as ResultsProcess
import           Lamdu.Eval.Val (ScopeId)
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.ParamList (ParamList)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.OrderTags (orderType, orderedClosedFlatComposite)
import           Lamdu.Sugar.Types

import           Prelude.Compat

type T = Transaction

data ConventionalParams m = ConventionalParams
    { cpTags :: Set T.Tag
    , _cpParamInfos :: Map T.Tag ConvertM.TagFieldParam
    , _cpParams :: BinderParams Guid m
    , _cpAddFirstParam :: T m ParamAddResult
    , cpScopes :: BinderBodyScope
    , cpMLamParam :: Maybe V.Var
    }

cpParams :: Lens' (ConventionalParams m) (BinderParams Guid m)
cpParams f ConventionalParams {..} = f _cpParams <&> \_cpParams -> ConventionalParams{..}

cpParamInfos :: Lens' (ConventionalParams m) (Map T.Tag ConvertM.TagFieldParam)
cpParamInfos f ConventionalParams {..} = f _cpParamInfos <&> \_cpParamInfos -> ConventionalParams{..}

cpAddFirstParam :: Lens' (ConventionalParams m) (T m ParamAddResult)
cpAddFirstParam f ConventionalParams {..} = f _cpAddFirstParam <&> \_cpAddFirstParam -> ConventionalParams{..}

data FieldParam = FieldParam
    { fpTag :: T.Tag
    , fpFieldType :: Type
    , fpValue :: CurAndPrev (Map ScopeId [(ScopeId, ER.Val Type)])
    }

data StoredLam m = StoredLam
    { _slLam :: V.Lam (Val (ValIProperty m))
    , slLambdaProp :: ValIProperty m
    }

slLam :: Lens' (StoredLam m) (V.Lam (Val (ValIProperty m)))
slLam f StoredLam{..} = f _slLam <&> \_slLam -> StoredLam{..}

slParamList :: MonadA m => StoredLam m -> Transaction.MkProperty m (Maybe ParamList)
slParamList = Anchors.assocFieldParamList . Property.value . slLambdaProp

mkStoredLam ::
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> StoredLam m
mkStoredLam lam pl =
    StoredLam
    (lam & Lens.mapped . Lens.mapped %~ (^. Input.stored))
    (pl ^. Input.stored)

newTag :: MonadA m => T m T.Tag
newTag = GenIds.transaction GenIds.randomTag

setParamList :: MonadA m => MkProperty m (Maybe [T.Tag]) -> [T.Tag] -> T m ()
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

wrapUnappliedUsesOfVar :: MonadA m => V.Var -> Val (ValIProperty m) -> T m ()
wrapUnappliedUsesOfVar var =
    SubExprs.onMatchingSubexprsWithPath holeWrap (isUnappliedVar var)
    where
        holeWrap prop =
            V.BLeaf V.LHole & ExprIRef.newValBody
            <&> (`V.Apply` Property.value prop) <&> V.BApp
            >>= ExprIRef.newValBody
            >>= Property.set prop

changeCallArgs ::
    MonadA m =>
    (ValI m -> T m (ValI m)) -> Val (ValIProperty m) -> V.Var -> T m ()
changeCallArgs change val var  =
    do
        SubExprs.onMatchingSubexprsWithPath changeArg (isArgOfCallTo var) val
        wrapUnappliedUsesOfVar var val
    where
        changeArg prop =
            Property.value prop & change >>= Property.set prop

fixUsagesOfLamBinder ::
    MonadA m => (ValI m -> T m (ValI m)) -> BinderKind m -> StoredLam m -> T m ()
fixUsagesOfLamBinder fixOp binderKind storedLam =
    case binderKind of
    BinderKindDef defI ->
        changeCallArgs fixOp (storedLam ^. slLam . V.lamResult) (ExprIRef.globalId defI)
    BinderKindLet redexLam ->
        changeCallArgs fixOp (redexLam ^. V.lamResult) (redexLam ^. V.lamParamId)
    BinderKindLambda -> return ()

addFieldParam ::
    MonadA m =>
    T m (ValI m) -> BinderKind m -> (T.Tag -> ParamList) -> StoredLam m ->
    T m (TagG ())
addFieldParam mkArg binderKind mkNewTags storedLam =
    do
        tag <- newTag
        let tagG =
                TagG
                { _tagInstance =
                    EntityId.ofLambdaTagParam
                    (storedLam ^. slLam . V.lamParamId) tag
                , _tagVal = tag
                , _tagGName = ()
                }
        mkNewTags tag & setParamList (slParamList storedLam)
        let addFieldToCall argI =
                do
                    newArg <- mkArg
                    V.RecExtend (tagG ^. tagVal) newArg argI
                        & V.BRecExtend & ExprIRef.newValBody
        fixUsagesOfLamBinder addFieldToCall binderKind storedLam
        return tagG

mkCpScopesOfLam :: Input.Payload m a -> CurAndPrev (Map ScopeId [BinderParamScopeId])
mkCpScopesOfLam x =
    x ^. Input.evalResults <&> (^. Input.eAppliesOfLam) <&> (fmap . fmap) fst
    <&> (fmap . map) BinderParamScopeId

getFieldOnVar :: Lens.Traversal' (Val t) (V.Var, T.Tag)
getFieldOnVar = V.body . ExprLens._BGetField . inGetField
    where
        inGetField f (V.GetField (Val pl (V.BLeaf (V.LVar v))) t) =
            f (v, t) <&> pack pl
        inGetField _ other = pure other
        pack pl (v, t) =
            V.GetField (Val pl (V.BLeaf (V.LVar v))) t

getFieldParamsToHole :: MonadA m => T.Tag -> V.Lam (Val (ValIProperty m)) -> T m ()
getFieldParamsToHole tag (V.Lam param lamBody) =
    SubExprs.onMatchingSubexprs SubExprs.toHole (getFieldOnVar . Lens.only (param, tag)) lamBody

getFieldParamsToParams :: MonadA m => V.Lam (Val (ValIProperty m)) -> T.Tag -> T m ()
getFieldParamsToParams (V.Lam param lamBody) tag =
    SubExprs.onMatchingSubexprs (toParam . Property.value)
    (getFieldOnVar . Lens.only (param, tag)) lamBody
    where
        toParam bodyI = ExprIRef.writeValBody bodyI $ V.BLeaf $ V.LVar param

fixCallArgRemoveField :: MonadA m => T.Tag -> ValI m -> T m (ValI m)
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
    MonadA m => T.Tag -> ValI m -> T m (ValI m)
fixCallToSingleArg tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> return v
                | otherwise -> fixCallToSingleArg tag restI
            _ -> return argI

tagGForLambdaTagParam :: V.Var -> T.Tag -> TagG ()
tagGForLambdaTagParam paramVar tag = TagG (EntityId.ofLambdaTagParam paramVar tag) tag ()

delFieldParamAndFixCalls ::
    MonadA m =>
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
                    { ttvReplacedTag = tagGForLambdaTagParam paramVar x
                    , ttvReplacedByVar = paramVar
                    , ttvReplacedByVarEntityId = EntityId.ofLambdaParam paramVar
                    , ttvDeletedTag = tagGForLambdaTagParam paramVar tag
                    }
                )
            xs -> (Just xs, Nothing, ParamDelResultDelTag)

fieldParamActions ::
    MonadA m =>
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
    (MonadA m, Monoid a) =>
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
            ( fpTag fp
            , FuncParam
                { _fpInfo =
                  NamedParamInfo
                  { _npiName = UniqueId.toGuid $ fpTag fp
                  , _npiActions = fieldParamActions binderKind tags fp storedLam
                  }
                , _fpId = fpIdEntityId param fp
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
                , _fpHiddenIds = []
                }
            )

removeCallsToVar :: MonadA m => V.Var -> Val (ValIProperty m) -> T m ()
removeCallsToVar funcVar val =
    do
        SubExprs.onMatchingSubexprs changeRecursion
            ( V.body . ExprLens._BApp . V.applyFunc . ExprLens.valVar
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
    MonadA m => BinderKind m -> StoredLam m ->
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
                let lamBodyI = Property.value (lamBodyStored ^. V.payload)
                _ <- protectedSetToVal lambdaProp lamBodyI
                return ParamDelResultDelVar

convertVarToGetField ::
    MonadA m => T.Tag -> V.Var -> Val (Property (T m) (ValI m)) -> T m ()
convertVarToGetField tagForVar paramVar =
    SubExprs.onGetVars (convertVar . Property.value) paramVar
    where
        convertVar bodyI =
            ExprIRef.newValBody (V.BLeaf (V.LVar paramVar))
            <&> (`V.GetField` tagForVar) <&> V.BGetField
            >>= ExprIRef.writeValBody bodyI

wrapArgWithRecord ::
    MonadA m => T m (ValI m) -> VarToTags -> ValI m -> T m (ValI m)
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
    MonadA m =>
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
                , vttReplacedByTag = tagGForLambdaTagParam paramVar tagForVar
                , vttNewTag = tagGForLambdaTagParam paramVar tagForNewVar
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
    lamExprPl ^? Input.inferredType . ExprLens._TFun . _1

makeNonRecordParamActions ::
    MonadA m => BinderKind m -> StoredLam m -> ConvertM m (FuncParamActions m)
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

mkFuncParam ::
    MonadA m =>
    EntityId -> Input.Payload m a -> info -> ConvertM m (FuncParam info)
mkFuncParam paramEntityId lamExprPl info =
    do
        noms <- ConvertM.readContext <&> (^. ConvertM.scNominalsMap)
        return FuncParam
            { _fpInfo = info
            , _fpId = paramEntityId
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
            , _fpHiddenIds = []
            }
    where
        typ = lamParamType lamExprPl

convertNonRecordParam ::
    MonadA m => BinderKind m ->
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
                  & NullParamInfo
                  & mkFuncParam paramEntityId lamExprPl
                  <&> NullParam
            _ ->
                NamedParamInfo
                { _npiName = UniqueId.toGuid param
                , _npiActions = funcParamActions
                } & mkFuncParam paramEntityId lamExprPl
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
    go body
    where
        go val =
            case val ^. V.body of
            V.BLeaf (V.LVar v) | v == param -> False
            V.BGetField (V.GetField r _) -> checkChildren r
            _ -> checkChildren val
            where
                checkChildren x = all go (x ^.. V.body . Lens.traverse)

convertLamParams ::
    (MonadA m, Monoid a) =>
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertLamParams lambda lambdaPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let maybeWrap = protectedSetToVal lambdaProp (Property.value lambdaProp)
        convertNonEmptyParams BinderKindLambda lambda lambdaPl
            <&> cpAddFirstParam %~ (<* maybeWrap)
            <&> cpParams . SugarLens.binderNamedParamsActions . fpAddNext %~
                (<* maybeWrap)
    where
        lambdaProp = lambdaPl ^. Input.stored

convertNonEmptyParams ::
    (MonadA m, Monoid a) =>
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
                        <&> Lens.traversed . Lens.mapped . Lens._2 %~
                            ResultsProcess.addTypes noms typeExpr .
                            ER.extractField tag
                }
        orderedType <-
            lambdaPl ^. Input.inferredType & orderType & ConvertM.liftTransaction
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
                        . V.payload
                        . Input.inferredType . ExprLens._TFun . _1
                        . ExprLens._TRecord . ExprLens.compositeTags
                        & Set.fromList
            _ -> convertNonRecordParam binderKind lambda lambdaPl
    where
        param = lambda ^. V.lamParamId
        mkCollidingInfo fp = mkParamInfo param fp <&> ConvertM.CollidingFieldParam

convertVarToCalls ::
    MonadA m => T m (ValI m) -> V.Var -> Val (ValIProperty m) -> T m ()
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
        (newParam, newValI) <- DataOps.lambdaWrap (val ^. V.payload)
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
    (MonadA m, Monoid a) =>
    BinderKind m -> Val (Input.Payload m a) ->
    ConvertM m
    ( ConventionalParams m
    , Val (Input.Payload m a)
    )
convertParams binderKind expr =
    case expr ^. V.body of
    V.BAbs lambda ->
        do
            params <-
                convertNonEmptyParams binderKind lambda (expr ^. V.payload)
                -- The lambda disappears here, so add its id to the first
                -- param's hidden ids:
                <&> cpParams . _VarParam . fpHiddenIds <>~ hiddenIds
                <&> cpParams . _FieldParams . Lens.ix 0 . _2 . fpHiddenIds <>~ hiddenIds
            return (params, lambda ^. V.lamResult)
        where
              hiddenIds = [expr ^. V.payload . Input.entityId]
    _ ->
        return (convertEmptyParams binderKind expr, expr)
