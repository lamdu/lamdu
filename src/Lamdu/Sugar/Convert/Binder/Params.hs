{-# LANGUAGE TemplateHaskell, RecordWildCards, PatternGuards #-}
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
import           Control.Monad.Transaction (getP, setP)
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import           Data.Maybe.Utils (unsafeUnjust)
import           Data.Property (Property, MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.ParamList (ParamList)
import           Lamdu.Sugar.Convert.Tag (convertTag, convertTaggedEntity, convertTagSelection, AllowAnonTag(..))
import           Lamdu.Sugar.Convert.Type (convertType)
import           Lamdu.Sugar.Convert.Eval (convertEvalParam)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.OrderTags (orderedClosedFlatComposite)
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ConventionalParams m = ConventionalParams
    { cpTags :: Set T.Tag
    , _cpParamInfos :: Map T.Tag ConvertM.TagFieldParam
    , _cpParams :: BinderParams InternalName (T m) (T m)
    , _cpAddFirstParam :: AddFirstParam InternalName (T m) (T m)
    , cpScopes :: BinderBodyScope
    , cpMLamParam :: Maybe ({- lambda's -}EntityId, V.Var)
    }
Lens.makeLenses ''ConventionalParams

data FieldParam = FieldParam
    { fpTag :: T.Tag
    , fpFieldType :: T.Type
    , fpValue :: CurAndPrev (Map ScopeId [(ScopeId, ER.Val T.Type)])
    }

data StoredLam m = StoredLam
    { _slLam :: V.Lam (Val (ValIProperty m))
    , slLambdaProp :: ValIProperty m
    }
Lens.makeLenses ''StoredLam

slParamList :: Monad m => StoredLam m -> MkProperty' (T m) (Maybe ParamList)
slParamList = Anchors.assocFieldParamList . Property.value . slLambdaProp

mkStoredLam ::
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> StoredLam m
mkStoredLam lam pl =
    StoredLam
    (lam <&> Lens.mapped %~ (^. Input.stored))
    (pl ^. Input.stored)

setParamList ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    MkProperty' (T m) (Maybe [T.Tag]) -> [T.Tag] -> T m ()
setParamList mPresMode paramListProp newParamList =
    do
        zip newParamList [0..] & mapM_ (uncurry setParamOrder)
        Just newParamList & setP paramListProp
        case mPresMode of
            Nothing -> pure ()
            Just presModeProp ->
                do
                    presMode <- getP presModeProp
                    case presMode of
                        Object f | [f] /= take 1 newParamList -> setP presModeProp Verbose
                        Infix f0 f1 | [f0, f1] /= take 2 newParamList -> setP presModeProp Verbose
                        _ -> pure ()
    where
        setParamOrder = setP . Anchors.assocTagOrder

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
    SubExprs.onMatchingSubexprsWithPath (DataOps.applyHoleTo <&> void) (isUnappliedVar var)

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
    BinderKindLambda -> pure ()

addFieldParam ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    T m (ValI m) -> BinderKind m -> StoredLam m -> (T.Tag -> ParamList) -> T.Tag ->
    T m ()
addFieldParam mPresMode mkArg binderKind storedLam mkNewTags tag =
    do
        mkNewTags tag & setParamList mPresMode (slParamList storedLam)
        fixUsagesOfLamBinder addFieldToCall binderKind storedLam
    where
        addFieldToCall argI =
            do
                newArg <- mkArg
                V.RecExtend tag newArg argI
                    & V.BRecExtend & ExprIRef.newValBody

mkCpScopesOfLam :: Input.Payload m a -> CurAndPrev (Map ScopeId [BinderParamScopeId])
mkCpScopesOfLam lamPl =
    lamPl ^. Input.evalResults <&> (^. Input.eAppliesOfLam) <&> (fmap . fmap) fst
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
                | t == tag -> pure restI
                | otherwise ->
                    do
                        newRestI <- fixCallArgRemoveField tag restI
                        when (newRestI /= restI) $
                            ExprIRef.writeValBody argI $
                            V.BRecExtend $ V.RecExtend t v newRestI
                        pure argI
            _ -> pure argI

fixCallToSingleArg ::
    Monad m => T.Tag -> ValI m -> T m (ValI m)
fixCallToSingleArg tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> pure v
                | otherwise -> fixCallToSingleArg tag restI
            _ -> pure argI

delFieldParamAndFixCalls ::
    Monad m => BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m -> T m ()
delFieldParamAndFixCalls binderKind tags fp storedLam =
    do
        setP (slParamList storedLam) newTags
        getFieldParamsToHole tag (storedLam ^. slLam)
        traverse_ onLastTag mLastTag
        fixUsagesOfLamBinder fixRecurseArg binderKind storedLam
    where
        onLastTag lastTag =
            do
                getFieldParamsToParams (storedLam ^. slLam) lastTag
                setP (Anchors.assocTag (storedLam ^. slLam . V.lamParamId)) lastTag
        tag = fpTag fp
        fixRecurseArg =
            maybe (fixCallArgRemoveField tag)
            fixCallToSingleArg mLastTag
        (newTags, mLastTag) =
            case List.delete tag tags of
            [x] -> (Nothing, Just x)
            xs -> (Just xs, Nothing)

fieldParamActions ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m ->
    ConvertM m (FuncParamActions InternalName (T m) (T m))
fieldParamActions mPresMode binderKind tags fp storedLam =
    do
        postProcess <- ConvertM.postProcess
        let addParamAfter newTag =
                do
                    -- Reread list for when both choosing param and adding new one
                    -- using pre-events.
                    getP (slParamList storedLam)
                        <&> fromMaybe (error "no params?")
                        <&> flip (ListUtils.insertAt (length tagsBefore + 1))
                        >>= (addFieldParam mPresMode DataOps.newHole binderKind
                             storedLam ?? newTag)
                    postProcess
        addNext <-
            convertTagSelection (nameWithContext param)
            (Set.fromList tags) RequireTag (EntityId.ofTaggedEntity param) addParamAfter
        pure FuncParamActions
            { _fpAddNext = AddNext addNext
            , _fpDelete = delFieldParamAndFixCalls binderKind tags fp storedLam
            , _fpMOrderBefore =
                case tagsBefore of
                [] -> Nothing
                b ->
                    init b ++ (fpTag fp : last b : tagsAfter)
                    & setParamList mPresMode (slParamList storedLam) & Just
            , _fpMOrderAfter =
                case tagsAfter of
                [] -> Nothing
                (x:xs) ->
                    tagsBefore ++ (x : fpTag fp : xs)
                    & setParamList mPresMode (slParamList storedLam) & Just
            }
    where
        param = storedLam ^. slLam . V.lamParamId
        (tagsBefore, _:tagsAfter) = break (== fpTag fp) tags

fpIdEntityId :: V.Var -> FieldParam -> EntityId
fpIdEntityId param = EntityId.ofTaggedEntity param . fpTag

mkParamInfo :: V.Var -> FieldParam -> Map T.Tag ConvertM.TagParamInfo
mkParamInfo param fp =
    fpIdEntityId param fp
    & ConvertM.TagParamInfo param
    & Map.singleton (fpTag fp)

changeGetFieldTags ::
    Monad m => V.Var -> T.Tag -> T.Tag -> Val (ValIProperty m) -> T m ()
changeGetFieldTags param prevTag chosenTag val =
    case val ^. Val.body of
    V.BGetField (V.GetField getVar@(Val _ (V.BLeaf (V.LVar v))) t)
        | v == param && t == prevTag ->
            V.GetField (getVar ^. Val.payload . Property.pVal) chosenTag & V.BGetField
            & ExprIRef.writeValBody (val ^. Val.payload . Property.pVal)
        | otherwise -> pure ()
    V.BLeaf (V.LVar v)
        | v == param -> DataOps.applyHoleTo (val ^. Val.payload) & void
    b -> traverse_ (changeGetFieldTags param prevTag chosenTag) b

setFieldParamTag ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) -> BinderKind m ->
    StoredLam m -> [T.Tag] -> T.Tag -> ConvertM m (T.Tag -> T m ())
setFieldParamTag mPresMode binderKind storedLam prevTagList prevTag =
    ConvertM.postProcess
    <&>
    \postProcess chosenTag ->
    do
        tagsBefore ++ chosenTag : tagsAfter & setParamList mPresMode (slParamList storedLam)
        let fixArg argI (V.BRecExtend recExtend)
                | recExtend ^. V.recTag == prevTag =
                    argI <$ ExprIRef.writeValBody argI (V.BRecExtend (recExtend & V.recTag .~ chosenTag))
                | otherwise =
                    argI <$
                    ( changeFieldToCall (recExtend ^. V.recRest)
                        <&> (\x -> recExtend & V.recRest .~ x)
                        <&> V.BRecExtend
                        >>= ExprIRef.writeValBody argI
                    )
            fixArg argI _ =
                DataOps.newHole
                <&> (`V.Apply` argI) <&> V.BApp
                >>= ExprIRef.newValBody
            changeFieldToCall argI = ExprIRef.readValBody argI >>= fixArg argI
        fixUsagesOfLamBinder changeFieldToCall binderKind storedLam
        changeGetFieldTags (storedLam ^. slLam . V.lamParamId) prevTag chosenTag (storedLam ^. slLam . V.lamResult)
        postProcess
    where
        (tagsBefore, _:tagsAfter) = break (== prevTag) prevTagList

convertRecordParams ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    BinderKind m -> [FieldParam] ->
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertRecordParams mPresMode binderKind fieldParams lam@(V.Lam param _) lamPl =
    do
        params <- mapM mkParam fieldParams
        postProcess <- ConvertM.postProcess
        let addFirst tag =
                do
                    getP (slParamList storedLam)
                        <&> fromMaybe (error "no params?")
                        <&> flip (:)
                        >>= (addFieldParam mPresMode DataOps.newHole binderKind storedLam ?? tag)
                    postProcess
        addFirstSelection <-
            convertTagSelection (nameWithContext param)
            (Set.fromList tags) RequireTag (EntityId.ofTaggedEntity param)
            addFirst
        pure ConventionalParams
            { cpTags = Set.fromList tags
            , _cpParamInfos = fieldParams <&> mkParInfo & mconcat
            , _cpParams = Params params
            , _cpAddFirstParam = PrependParam addFirstSelection
            , cpScopes = BinderBodyScope $ mkCpScopesOfLam lamPl
            , cpMLamParam = Just (entityId, param)
            }
    where
        entityId = lamPl ^. Input.entityId
        tags = fieldParams <&> fpTag
        mkParInfo fp = mkParamInfo param fp <&> ConvertM.TagFieldParam
        storedLam = mkStoredLam lam lamPl
        mkParam fp =
            do
                paramInfo <-
                    ParamInfo
                    <$> ( setFieldParamTag mPresMode binderKind storedLam tagList tag
                            >>= convertTag tag (nameWithContext param)
                                (Set.delete tag (Set.fromList tagList))
                                (EntityId.ofTaggedEntity param)
                        )
                    <*> fieldParamActions mPresMode binderKind tags fp storedLam
                let paramEntityId = paramInfo ^. piTag . tagInfo . tagInstance
                typeS <- convertType (EntityId.ofTypeOf paramEntityId) (fpFieldType fp)
                evalResults <-
                    fpValue fp & convertEvalParam (EntityId.ofEvalOf paramEntityId)
                FuncParam Annotation
                    { _aInferredType = typeS
                    , _aMEvaluationResult = evalResults
                    } paramInfo & pure
            where
                tag = fpTag fp
                tagList = fieldParams <&> fpTag

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

makeDeleteLambda :: Monad m => BinderKind m -> StoredLam m -> ConvertM m (T m ())
makeDeleteLambda binderKind (StoredLam (V.Lam paramVar lamBodyStored) lambdaProp) =
    ConvertM.typeProtectedSetToVal
    <&> \protectedSetToVal ->
    do
        SubExprs.getVarsToHole paramVar lamBodyStored
        case binderKind of
            BinderKindDef defI ->
                removeCallsToVar
                (ExprIRef.globalId defI) lamBodyStored
            BinderKindLet redexLam ->
                removeCallsToVar
                (redexLam ^. V.lamParamId) (redexLam ^. V.lamResult)
            BinderKindLambda -> pure ()
        let lamBodyI = Property.value (lamBodyStored ^. Val.payload)
        protectedSetToVal lambdaProp lamBodyI & void

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
    Monad m => T m (ValI m) -> T.Tag -> T.Tag -> ValI m -> T m (ValI m)
wrapArgWithRecord mkNewArg oldParam newParam oldArg =
    do
        newArg <- mkNewArg
        ExprIRef.newValBody (V.BLeaf V.LRecEmpty)
            >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend newParam newArg
            >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend oldParam oldArg

data NewParamPosition = NewParamBefore | NewParamAfter

convertToRecordParams ::
    Monad m =>
    T m (ValI m) -> BinderKind m -> StoredLam m -> NewParamPosition -> T.Tag ->
    T m ()
convertToRecordParams mkNewArg binderKind storedLam newParamPosition newParam =
    do
        oldParam <- Anchors.assocTag paramVar & getP
        -- the associated tag becomes an actual field in the new
        -- params record, remove the duplicate associated tag so that
        -- the params record is not named the same as the first param
        setP (Anchors.assocTag paramVar) Anchors.anonTag
        case newParamPosition of
            NewParamBefore -> [newParam, oldParam]
            NewParamAfter -> [oldParam, newParam]
            & setParamList Nothing paramList
        convertVarToGetField oldParam paramVar
            (storedLam ^. slLam . V.lamResult)
        fixUsagesOfLamBinder (wrapArgWithRecord mkNewArg oldParam newParam)
            binderKind storedLam
    where
        paramVar = storedLam ^. slLam . V.lamParamId
        paramList =
            slLambdaProp storedLam & Property.value
            & Anchors.assocFieldParamList

lamParamType :: Input.Payload m a -> T.Type
lamParamType lamExprPl =
    unsafeUnjust "Lambda value not inferred to a function type?!" $
    lamExprPl ^? Input.inferredType . T._TFun . _1

makeNonRecordParamActions ::
    Monad m =>
    BinderKind m -> StoredLam m ->
    ConvertM m (FuncParamActions InternalName (T m) (T m))
makeNonRecordParamActions binderKind storedLam =
    do
        del <- makeDeleteLambda binderKind storedLam
        postProcess <- ConvertM.postProcess
        let addParamAfter tag =
                do
                    convertToRecordParams DataOps.newHole binderKind storedLam
                        NewParamAfter tag
                    postProcess
        oldParam <- Anchors.assocTag param & getP
        addNext <-
            if oldParam == Anchors.anonTag
            then EntityId.ofTaggedEntity param oldParam & NeedToPickTagToAddNext & pure
            else
                convertTagSelection (nameWithContext param)
                (Set.singleton oldParam) RequireTag (EntityId.ofTaggedEntity param)
                addParamAfter
                <&> AddNext
        pure FuncParamActions
            { _fpAddNext = addNext
            , _fpDelete = del
            , _fpMOrderBefore = Nothing
            , _fpMOrderAfter = Nothing
            }
    where
        param = storedLam ^. slLam . V.lamParamId

mkFuncParam ::
    Monad m =>
    EntityId -> Input.Payload m a -> info ->
    ConvertM m (FuncParam InternalName info)
mkFuncParam entityId lamExprPl info =
    do
        typS <- convertType (EntityId.ofTypeOf entityId) typ
        evalResults <-
            lamExprPl ^. Input.evalResults <&> (^. Input.eAppliesOfLam)
            & convertEvalParam (EntityId.ofEvalOf entityId)
        pure FuncParam
            { _fpInfo = info
            , _fpAnnotation =
                Annotation
                { _aInferredType = typS
                , _aMEvaluationResult = evalResults
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
                    mkFuncParam (EntityId.ofBinder param) lamExprPl info <&> NullParam
                where
                    info = funcParamActions ^. fpDelete & void & NullParamActions
            _ ->
                do
                    tag <- convertTaggedEntity param
                    mkFuncParam (tag ^. tagInfo . tagInstance) lamExprPl
                        ParamInfo
                        { _piTag = tag
                        , _piActions = funcParamActions
                        }
                <&> (:[])
                <&> Params
        oldParam <- Anchors.assocTag param & getP
        postProcess <- ConvertM.postProcess
        let addFirst tag =
                do
                    convertToRecordParams DataOps.newHole binderKind storedLam
                        NewParamBefore tag
                    postProcess
        addFirstSelection <-
            convertTagSelection (nameWithContext param) (Set.singleton oldParam)
            RequireTag (EntityId.ofTaggedEntity param) addFirst
        pure ConventionalParams
            { cpTags = mempty
            , _cpParamInfos = Map.empty
            , _cpParams = funcParam
            , _cpAddFirstParam =
                if oldParam == Anchors.anonTag
                then NeedToPickTagToAddFirst (EntityId.ofTaggedEntity param oldParam)
                else PrependParam addFirstSelection
            , cpScopes = BinderBodyScope $ mkCpScopesOfLam lamExprPl
            , cpMLamParam = Just (lamExprPl ^. Input.entityId, param)
            }
    where
        storedLam = mkStoredLam lam lamExprPl

isParamAlwaysUsedWithGetField :: V.Lam (Val a) -> Bool
isParamAlwaysUsedWithGetField (V.Lam param body) =
    go False body
    where
        go isGetFieldChild val =
            case val ^. Val.body of
            V.BLeaf (V.LVar v) | v == param -> isGetFieldChild
            V.BGetField (V.GetField r _) -> go True r
            x -> all (go False) (x ^.. Lens.traverse)

-- Post process param add and delete actions to detach lambda.
-- This isn't done for all actions as some already perform this function.
-- TODO: clean up responsibilities - make it clear why some actions already
-- take care of wrapping and some don't.
postProcessActions ::
    Monad m => T m () -> ConventionalParams m -> ConventionalParams m
postProcessActions post x
    | Lens.has (cpParams . _Params) x =
        x & cpParams . SugarLens.binderFuncParamActions . fpDelete %~ (<* post)
    | otherwise = x

convertLamParams ::
    Monad m =>
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertLamParams lambda lambdaPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let maybeWrap = protectedSetToVal lambdaProp (Property.value lambdaProp)
        convertNonEmptyParams Nothing BinderKindLambda lambda lambdaPl
            <&> postProcessActions (void maybeWrap)
    where
        lambdaProp = lambdaPl ^. Input.stored

makeFieldParam :: Input.Payload m a -> (T.Tag, T.Type) -> FieldParam
makeFieldParam lambdaPl (tag, typeExpr) =
    FieldParam
    { fpTag = tag
    , fpFieldType = typeExpr
    , fpValue =
        lambdaPl ^. Input.evalResults
        <&> (^. Input.eAppliesOfLam)
        <&> Lens.mapped . Lens.mapped . _2 %~ ER.extractField typeExpr tag
        <&> Lens.mapped %~
            filter (Lens.nullOf (_2 . ER.body . ER._RError))
    }

convertNonEmptyParams ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    BinderKind m -> V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertNonEmptyParams mPresMode binderKind lambda lambdaPl =
    do
        tagsInOuterScope <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siTagParamInfos)
            <&> Map.keysSet
        presModeTags <- Lens._Just getP mPresMode <&> mPresModeToTags
        let presModeOrder tag = takeWhile (/= tag) presModeTags & length
        case lambdaPl ^. Input.inferredType of
            T.TFun (T.TRecord composite) _
                | Just fields <- composite ^? orderedClosedFlatComposite <&> List.sortOn (presModeOrder . fst)
                , ListUtils.isLengthAtLeast 2 fields
                , isParamAlwaysUsedWithGetField lambda
                , let myTags = fields <&> fst & Set.fromList
                , let fieldParams = fields <&> makeFieldParam lambdaPl
                -> if Set.null (tagsInOuterScope `Set.intersection` myTags)
                   then convertRecordParams mPresMode binderKind fieldParams lambda lambdaPl
                   else
                       convertNonRecordParam binderKind lambda lambdaPl
                       <&> cpParamInfos <>~ (fieldParams & map mkCollidingInfo & mconcat)
            _ -> convertNonRecordParam binderKind lambda lambdaPl
    where
        param = lambda ^. V.lamParamId
        mkCollidingInfo fp = mkParamInfo param fp <&> ConvertM.CollidingFieldParam
        mPresModeToTags p =
            case p of
            Just (Object t) -> [t]
            Just (Infix t0 t1) -> [t0, t1]
            _ -> []

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
    T m (V.Var, ValI m)
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
        pure (newParam, newValI)

convertEmptyParams ::
    Monad m =>
    BinderKind m -> Val (Input.Payload m a) -> ConvertM m (ConventionalParams m)
convertEmptyParams binderKind val =
    ConvertM.postProcess
    <&>
    \postProcess ->
    ConventionalParams
    { cpTags = mempty
    , _cpParamInfos = Map.empty
    , _cpParams = BinderWithoutParams
    , _cpAddFirstParam =
        do
            (newParam, _) <-
                val <&> (^. Input.stored)
                & convertBinderToFunction DataOps.newHole binderKind
            postProcess
            EntityId.ofTaggedEntity newParam Anchors.anonTag & pure
        & AddInitialParam
    , cpScopes = SameAsParentScope
    , cpMLamParam = Nothing
    }

convertParams ::
    Monad m =>
    BinderKind m -> V.Var -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , ConventionalParams m
    , Val (Input.Payload m a)
    )
convertParams binderKind defVar expr =
    do
        postProcess <- ConvertM.postProcess
        case expr ^. Val.body of
            V.BLam lambda ->
                convertNonEmptyParams (Just presMode) binderKind lambda (expr ^. Val.payload)
                <&> f
                where
                    f convParams =
                        (mPresMode convParams, convParams, lambda ^. V.lamResult)
                    mPresMode convParams =
                        presMode <$ convParams ^? cpParams . _Params . Lens.ix 1
                    presMode = Anchors.assocPresentationMode defVar
            _ -> convertEmptyParams binderKind expr <&> \x -> (Nothing, x, expr)
            <&> _2 %~ postProcessActions postProcess
