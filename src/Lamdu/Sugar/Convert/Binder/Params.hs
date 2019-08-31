{-# LANGUAGE TemplateHaskell, PatternGuards, TupleSections, TypeFamilies #-}
module Lamdu.Sugar.Convert.Binder.Params
    ( ConventionalParams(..), cpParams, cpAddFirstParam
    , convertParams, convertLamParams
    , mkStoredLam, makeDeleteLambda
    , convertBinderToFunction
    , convertToRecordParams
    , StoredLam(..), slLam, slLambdaProp
    , NewParamPosition(..), addFieldParam
    , isParamAlwaysUsedWithGetField
    , mkVarInfo
    ) where

import           AST (Tree, Pure(..), _Pure, traverseK1)
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import           AST.Term.FuncType (FuncType(..), funcIn)
import           AST.Term.Row (RowExtend(..), FlatRowExtends(..))
import qualified AST.Term.Row as Row
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (getP, setP)
import qualified Data.List.Extended as List
import qualified Data.Map as Map
import           Data.Maybe.Extended (unsafeUnjust)
import           Data.Property (Property, MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import qualified Lamdu.Sugar.Convert.Eval as ConvertEval
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.ParamList (ParamList)
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Convert.Type (convertType)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ConventionalParams m = ConventionalParams
    { cpTags :: Set T.Tag
    , _cpParamInfos :: Map T.Tag ConvertM.TagFieldParam
    , _cpParams :: Maybe (BinderParams InternalName (T m) (T m))
    , _cpAddFirstParam :: AddFirstParam InternalName (T m) (T m)
    , cpScopes :: BinderBodyScope
    , cpMLamParam :: Maybe ({- lambda's -}EntityId, V.Var)
    }
Lens.makeLenses ''ConventionalParams

data FieldParam = FieldParam
    { fpTag :: T.Tag
    , fpFieldType :: Tree Pure T.Type
    , fpValue :: EvalScopes [(ScopeId, ER.Val (Tree Pure T.Type))]
    }

data StoredLam m = StoredLam
    { _slLam :: Tree (V.Lam V.Var V.Term) (Ann (ValP m))
    , _slLambdaProp :: ValP m
    }
Lens.makeLenses ''StoredLam

slParamList :: Monad m => StoredLam m -> MkProperty' (T m) (Maybe ParamList)
slParamList = Anchors.assocFieldParamList . (^. slLambdaProp . Property.pVal)

mkStoredLam ::
    Tree (V.Lam V.Var V.Term) (Ann (Input.Payload m a)) ->
    Input.Payload m a -> StoredLam m
mkStoredLam lam pl =
    StoredLam
    (lam & V.lamOut . annotations %~ (^. Input.stored))
    (pl ^. Input.stored)

setParamList ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    MkProperty' (T m) (Maybe [T.Tag]) -> [T.Tag] -> T m ()
setParamList mPresMode paramListProp newParamList =
    do
        Lens.itraverse_ (flip DataOps.setTagOrder) newParamList
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

isArgOfCallTo :: V.Var -> [Val ()] -> Bool
isArgOfCallTo funcVar (cur : parent : _) =
    not (Lens.has varT cur) &&
    Lens.has (ExprLens.valApply . V.appFunc . varT) parent
    where
        varT = ExprLens.valVar . Lens.only funcVar
isArgOfCallTo _ _ = False

isUnappliedVar :: V.Var -> [Val ()] -> Bool
isUnappliedVar var (cur : parent : _) =
    Lens.has varT cur
    -- Var could not be both the arg and the func (will be type error)
    && not (Lens.has (ExprLens.valApply . V.appFunc . varT) parent)
    && not (Lens.has (ExprLens.valApply . V.appFunc . ExprLens.valHole) parent)
    where
        varT = ExprLens.valVar . Lens.only var
isUnappliedVar var [cur] = Lens.has (ExprLens.valVar . Lens.only var) cur
isUnappliedVar _ _ = False

wrapUnappliedUsesOfVar :: Monad m => V.Var -> Val (ValP m) -> T m ()
wrapUnappliedUsesOfVar var =
    SubExprs.onMatchingSubexprsWithPath (DataOps.applyHoleTo <&> void) (isUnappliedVar var)

changeCallArgs ::
    Monad m =>
    (ValI m -> T m (ValI m)) -> Val (ValP m) -> V.Var -> T m ()
changeCallArgs change v var  =
    do
        SubExprs.onMatchingSubexprsWithPath (Property.modify_ ?? change)
            (isArgOfCallTo var) v
        wrapUnappliedUsesOfVar var v

-- | If the lam is bound to a variable, we can fix all uses of the
--   variable. When it isn't, we may need to fix the lam itself
fixLamUsages ::
    Monad m =>
    ConvertM m
    ((ValI m -> T m (ValI m)) -> BinderKind m -> StoredLam m -> T m ())
fixLamUsages =
    ConvertM.typeProtectedSetToVal
    <&> \protectedSetToVal fixOp binderKind storedLam ->
    case binderKind of
    BinderKindDef defI ->
        changeCallArgs fixOp (storedLam ^. slLam . V.lamOut) (ExprIRef.globalId defI)
    BinderKindLet redexLam ->
        changeCallArgs fixOp (redexLam ^. V.lamOut) (redexLam ^. V.lamIn)
    BinderKindLambda ->
        protectedSetToVal prop (prop ^. Property.pVal) & void
        where
            prop = storedLam ^. slLambdaProp

addFieldParam ::
    Monad m =>
    ConvertM m
    (Maybe (MkProperty' (T m) PresentationMode) -> T m (ValI m) ->
        BinderKind m -> StoredLam m -> (T.Tag -> ParamList) -> T.Tag -> T m ())
addFieldParam =
    fixLamUsages
    <&> \fixUsages mPresMode mkArg binderKind storedLam mkNewTags tag ->
    do
        let addFieldToCall argI =
                do
                    newArg <- mkArg
                    RowExtend tag newArg argI
                        & V.BRecExtend & ExprIRef.newValI
        setParamList mPresMode (slParamList storedLam) (mkNewTags tag)
        fixUsages addFieldToCall binderKind storedLam

mkCpScopesOfLam :: Input.Payload m a -> EvalScopes  [BinderParamScopeId]
mkCpScopesOfLam lamPl =
    lamPl ^. Input.evalResults <&> (^. Input.eAppliesOfLam) <&> (fmap . fmap) fst
    <&> (fmap . map) BinderParamScopeId

getFieldOnVar :: Lens.Traversal' (Val t) (V.Var, T.Tag)
getFieldOnVar = val . V._BGetField . inGetField
    where
        inGetField f (V.GetField (Ann pl (V.BLeaf (V.LVar v))) t) =
            f (v, t) <&> pack pl
        inGetField _ other = pure other
        pack pl (v, t) =
            V.GetField (Ann pl (V.BLeaf (V.LVar v))) t

getFieldParamsToHole ::
    Monad m =>
    T.Tag -> Tree (V.Lam V.Var V.Term) (Ann (ValP m)) -> T m ()
getFieldParamsToHole tag (V.Lam param lamBody) =
    SubExprs.onMatchingSubexprs SubExprs.toHole (getFieldOnVar . Lens.only (param, tag)) lamBody

getFieldParamsToParams ::
    Monad m =>
    Tree (V.Lam V.Var V.Term) (Ann (ValP m)) -> T.Tag -> T m ()
getFieldParamsToParams (V.Lam param lamBody) tag =
    SubExprs.onMatchingSubexprs (toParam . Property.value)
    (getFieldOnVar . Lens.only (param, tag)) lamBody
    where
        toParam bodyI = ExprIRef.writeValI bodyI $ V.BLeaf $ V.LVar param

fixCallArgRemoveField :: Monad m => T.Tag -> ValI m -> T m (ValI m)
fixCallArgRemoveField tag argI =
    ExprIRef.readValI argI
    >>= \case
    V.BRecExtend (RowExtend t v restI)
        | t == tag -> pure restI
        | otherwise ->
            do
                newRestI <- fixCallArgRemoveField tag restI
                when (newRestI /= restI) $
                    ExprIRef.writeValI argI $
                    V.BRecExtend $ RowExtend t v newRestI
                pure argI
    _ -> pure argI

fixCallToSingleArg ::
    Monad m => T.Tag -> ValI m -> T m (ValI m)
fixCallToSingleArg tag argI =
    ExprIRef.readValI argI
    >>= \case
    V.BRecExtend (RowExtend t v restI)
        | t == tag -> pure v
        | otherwise -> fixCallToSingleArg tag restI
    _ -> pure argI

delFieldParamAndFixCalls ::
    Monad m =>
    BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m ->
    ConvertM m (T m ())
delFieldParamAndFixCalls binderKind tags fp storedLam =
    fixLamUsages
    <&> \fixUsages ->
    do
        setP (slParamList storedLam) newTags
        getFieldParamsToHole tag (storedLam ^. slLam)
        traverse_ onLastTag mLastTag
        fixUsages fixRecurseArg binderKind storedLam
    where
        onLastTag lastTag =
            do
                getFieldParamsToParams (storedLam ^. slLam) lastTag
                setP (Anchors.assocTag (storedLam ^. slLam . V.lamIn)) lastTag
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
        postProcess <- ConvertM.postProcessAssert
        add <- addFieldParam
        let addParamAfter newTag =
                do
                    -- Reread list for when both choosing param and adding new one
                    -- using pre-events.
                    getP (slParamList storedLam)
                        <&> fromMaybe (error "no params?")
                        <&> flip (List.insertAt (length tagsBefore + 1))
                        >>= (add mPresMode DataOps.newHole binderKind
                             storedLam ?? newTag)
                    postProcess
        addNext <-
            ConvertTag.replace (nameWithContext param)
            (Set.fromList tags) ConvertTag.RequireTag
            (EntityId.ofTaggedEntity param) addParamAfter
        del <- delFieldParamAndFixCalls binderKind tags fp storedLam
        pure FuncParamActions
            { _fpAddNext = AddNext addNext
            , _fpDelete = del
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
        param = storedLam ^. slLam . V.lamIn
        (tagsBefore, tagsAfter) = break (== fpTag fp) tags & _2 %~ tail

fpIdEntityId :: V.Var -> FieldParam -> EntityId
fpIdEntityId param = EntityId.ofTaggedEntity param . fpTag

mkParamInfo :: V.Var -> FieldParam -> Map T.Tag ConvertM.TagParamInfo
mkParamInfo param fp =
    fpIdEntityId param fp
    & ConvertM.TagParamInfo param
    & Map.singleton (fpTag fp)

changeGetFieldTags ::
    Monad m => V.Var -> T.Tag -> T.Tag -> Val (ValP m) -> T m ()
changeGetFieldTags param prevTag chosenTag x =
    case x ^. val of
    V.BGetField (V.GetField (Ann a (V.BLeaf (V.LVar v))) t)
        | v == param && t == prevTag ->
            V.GetField (a ^. Property.pVal) chosenTag & V.BGetField
            & ExprIRef.writeValI (x ^. ann . Property.pVal)
        | otherwise -> pure ()
    V.BLeaf (V.LVar v)
        | v == param -> DataOps.applyHoleTo (x ^. ann) & void
    b ->
        traverse_
        (changeGetFieldTags param prevTag chosenTag)
        (b ^.. traverseK1)

setFieldParamTag ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) -> BinderKind m ->
    StoredLam m -> [T.Tag] -> T.Tag -> ConvertM m (T.Tag -> T m ())
setFieldParamTag mPresMode binderKind storedLam prevTagList prevTag =
    (,) <$> fixLamUsages <*> ConvertM.postProcessAssert
    <&> \(fixUsages, postProcess) chosenTag ->
    do
        tagsBefore ++ chosenTag : tagsAfter
            & setParamList mPresMode (slParamList storedLam)
        let fixArg argI (V.BRecExtend recExtend)
                | recExtend ^. Row.eKey == prevTag =
                    argI <$
                    ExprIRef.writeValI argI
                    (V.BRecExtend (recExtend & Row.eKey .~ chosenTag))
                | otherwise =
                    argI <$
                    ( changeFieldToCall (recExtend ^. Row.eRest)
                        <&> (\x -> recExtend & Row.eRest .~ x)
                        <&> V.BRecExtend
                        >>= ExprIRef.writeValI argI
                    )
            fixArg argI _ =
                DataOps.newHole
                <&> (`V.App` argI) <&> V.BApp
                >>= ExprIRef.newValI
            changeFieldToCall argI = ExprIRef.readValI argI >>= fixArg argI
        fixUsages changeFieldToCall binderKind storedLam
        changeGetFieldTags
            (storedLam ^. slLam . V.lamIn) prevTag chosenTag
            (storedLam ^. slLam . V.lamOut)
        postProcess
    where
        (tagsBefore, tagsAfter) = break (== prevTag) prevTagList & _2 %~ tail

convertRecordParams ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    BinderKind m -> [FieldParam] ->
    Tree (V.Lam V.Var V.Term) (Ann (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertRecordParams mPresMode binderKind fieldParams lam@(V.Lam param _) lamPl =
    do
        params <- traverse mkParam fieldParams
        postProcess <- ConvertM.postProcessAssert
        add <- addFieldParam
        let addFirst tag =
                do
                    getP (slParamList storedLam)
                        <&> fromMaybe (error "no params?")
                        <&> flip (:)
                        >>= (add mPresMode DataOps.newHole binderKind storedLam ?? tag)
                    postProcess
        addFirstSelection <-
            ConvertTag.replace (nameWithContext param)
            (Set.fromList tags) ConvertTag.RequireTag
            (EntityId.ofTaggedEntity param)
            addFirst
        pure ConventionalParams
            { cpTags = Set.fromList tags
            , _cpParamInfos = fieldParams <&> mkParInfo & mconcat
            , _cpParams = Params params & Just
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
                            >>= ConvertTag.ref tag (nameWithContext param)
                                (Set.delete tag (Set.fromList tagList))
                                (EntityId.ofTaggedEntity param)
                        )
                    <*> fieldParamActions mPresMode binderKind tags fp storedLam
                let paramEntityId = paramInfo ^. piTag . tagRefTag . tagInstance
                typeS <- convertType (EntityId.ofTypeOf paramEntityId) (fpFieldType fp)
                pure FuncParam
                    { _fpAnnotation =
                        AnnotationVal ValAnnotation
                        { _annotationType = Just typeS
                        , _annotationVal =
                            fpValue fp & ConvertEval.param (EntityId.ofEvalOf paramEntityId)
                        }
                    , _fpInfo = paramInfo
                    , _fpVarInfo = mkVarInfo typeS
                    }
            where
                tag = fpTag fp
                tagList = fieldParams <&> fpTag

removeCallsToVar :: Monad m => V.Var -> Val (ValP m) -> T m ()
removeCallsToVar funcVar x =
    do
        SubExprs.onMatchingSubexprs changeRecursion
            ( val . V._BApp . V.appFunc . ExprLens.valVar
            . Lens.only funcVar
            ) x
        wrapUnappliedUsesOfVar funcVar x
    where
        changeRecursion prop =
            ExprIRef.readValI (Property.value prop)
            >>= \case
            V.BApp (V.App f _) -> (prop ^. Property.pSet) f
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
                (redexLam ^. V.lamIn) (redexLam ^. V.lamOut)
            BinderKindLambda -> pure ()
        let lamBodyI = Property.value (lamBodyStored ^. ann)
        protectedSetToVal lambdaProp lamBodyI & void

convertVarToGetField ::
    Monad m => T.Tag -> V.Var -> Val (Property (T m) (ValI m)) -> T m ()
convertVarToGetField tagForVar paramVar =
    SubExprs.onGetVars (convertVar . Property.value) paramVar
    where
        convertVar bodyI =
            ExprIRef.newValI (V.BLeaf (V.LVar paramVar))
            <&> (`V.GetField` tagForVar) <&> V.BGetField
            >>= ExprIRef.writeValI bodyI

wrapArgWithRecord ::
    Monad m => T m (ValI m) -> T.Tag -> T.Tag -> ValI m -> T m (ValI m)
wrapArgWithRecord mkNewArg oldParam newParam oldArg =
    do
        newArg <- mkNewArg
        ExprIRef.newValI (V.BLeaf V.LRecEmpty)
            >>= ExprIRef.newValI . V.BRecExtend . RowExtend newParam newArg
            >>= ExprIRef.newValI . V.BRecExtend . RowExtend oldParam oldArg

data NewParamPosition = NewParamBefore | NewParamAfter

convertToRecordParams ::
    Monad m =>
    ConvertM m
    (T m (ValI m) -> BinderKind m -> StoredLam m -> NewParamPosition -> T.Tag ->
        T m ())
convertToRecordParams =
    fixLamUsages <&>
    \fixUsages mkNewArg binderKind storedLam newParamPosition newParam ->
    do
        let paramVar = storedLam ^. slLam . V.lamIn
        oldParam <-
            getP (Anchors.assocTag paramVar)
            >>=
            \x ->
            if x == Anchors.anonTag
            then DataOps.genNewTag
            else pure x
        -- the associated tag becomes an actual field in the new
        -- params record, remove the duplicate associated tag so that
        -- the params record is not named the same as the first param
        setP (Anchors.assocTag paramVar) Anchors.anonTag
        case newParamPosition of
            NewParamBefore -> [newParam, oldParam]
            NewParamAfter -> [oldParam, newParam]
            & setParamList Nothing (slParamList storedLam)
        convertVarToGetField oldParam paramVar
            (storedLam ^. slLam . V.lamOut)
        fixUsages (wrapArgWithRecord mkNewArg oldParam newParam)
            binderKind storedLam

lamParamType :: Input.Payload m a -> Tree Pure T.Type
lamParamType lamExprPl =
    unsafeUnjust "Lambda value not inferred to a function type?!" $
    lamExprPl ^? Input.inferredType . _Pure . T._TFun . funcIn

makeNonRecordParamActions ::
    Monad m =>
    BinderKind m -> StoredLam m ->
    ConvertM m (FuncParamActions InternalName (T m) (T m))
makeNonRecordParamActions binderKind storedLam =
    do
        del <- makeDeleteLambda binderKind storedLam
        postProcess <- ConvertM.postProcessAssert
        addParamAfter <-
            convertToRecordParams ?? DataOps.newHole ?? binderKind ?? storedLam
            ?? NewParamAfter
            <&> Lens.mapped %~ (<* postProcess)
        oldParam <- Anchors.assocTag param & getP
        addNext <-
            if oldParam == Anchors.anonTag
            then EntityId.ofTaggedEntity param oldParam & NeedToPickTagToAddNext & pure
            else
                ConvertTag.replace (nameWithContext param)
                (Set.singleton oldParam) ConvertTag.RequireTag
                (EntityId.ofTaggedEntity param) addParamAfter <&> AddNext
        pure FuncParamActions
            { _fpAddNext = addNext
            , _fpDelete = del
            , _fpMOrderBefore = Nothing
            , _fpMOrderAfter = Nothing
            }
    where
        param = storedLam ^. slLam . V.lamIn

mkVarInfo :: Tree (Ann a) (Type InternalName) -> VarInfo
mkVarInfo (Ann _ TFun{}) = VarFunction
mkVarInfo (Ann _ TRecord{}) = VarRecord
mkVarInfo (Ann _ TVariant{}) = VarVariant
mkVarInfo (Ann _ TVar{}) = VarGeneric
mkVarInfo (Ann _ (TInst (TId name tid) _)) = VarNominal tid (name ^. inTag)

mkFuncParam ::
    Monad m =>
    EntityId -> Input.Payload m a -> info ->
    ConvertM m (FuncParam InternalName (T m) info)
mkFuncParam entityId lamExprPl info =
    (,)
    <$> Lens.view ConvertM.scAnnotationsMode
    <*> convertType (EntityId.ofTypeOf entityId) typ
    <&> \(annMode, typS) ->
    FuncParam
    { _fpInfo = info
    , _fpAnnotation =
        case annMode of
        Annotations.None -> AnnotationNone
        Annotations.Types -> AnnotationType typS
        Annotations.Evaluation ->
            AnnotationVal ValAnnotation
            { _annotationType = Nothing
            , _annotationVal =
                lamExprPl ^. Input.evalResults <&> (^. Input.eAppliesOfLam)
                & ConvertEval.param (EntityId.ofEvalOf entityId)
            }
    , _fpVarInfo = mkVarInfo typS
    }
    where
        typ = lamParamType lamExprPl

convertNonRecordParam ::
    Monad m => BinderKind m ->
    Tree (V.Lam V.Var V.Term) (Ann (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertNonRecordParam binderKind lam@(V.Lam param _) lamExprPl =
    do
        funcParamActions <- makeNonRecordParamActions binderKind storedLam
        nullParamSugar <-
            Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.nullaryParameter)
        funcParam <-
            case lamParamType lamExprPl ^. _Pure of
            T.TRecord (Pure T.REmpty)
                | nullParamSugar && null (lamExprPl ^. Input.varRefsOfLambda) ->
                    mkFuncParam (EntityId.ofBinder param) lamExprPl info <&> NullParam
                where
                    info = funcParamActions ^. fpDelete & void & NullParamActions
            _ ->
                do
                    tag <- ConvertTag.taggedEntity param
                    mkFuncParam (tag ^. tagRefTag . tagInstance) lamExprPl
                        ParamInfo
                        { _piTag = tag
                        , _piActions = funcParamActions
                        }
                <&> (:[])
                <&> Params
        oldParam <- Anchors.assocTag param & getP
        postProcess <- ConvertM.postProcessAssert
        addFirst <-
            convertToRecordParams ?? DataOps.newHole ?? binderKind ?? storedLam
            ?? NewParamBefore
            <&> Lens.mapped %~ (<* postProcess)
        addFirstSelection <-
            ConvertTag.replace (nameWithContext param) (Set.singleton oldParam)
            ConvertTag.RequireTag (EntityId.ofTaggedEntity param) addFirst
        pure ConventionalParams
            { cpTags = mempty
            , _cpParamInfos = Map.empty
            , _cpParams = Just funcParam
            , _cpAddFirstParam =
                if oldParam == Anchors.anonTag
                then NeedToPickTagToAddFirst (EntityId.ofTaggedEntity param oldParam)
                else PrependParam addFirstSelection
            , cpScopes = BinderBodyScope $ mkCpScopesOfLam lamExprPl
            , cpMLamParam = Just (lamExprPl ^. Input.entityId, param)
            }
    where
        storedLam = mkStoredLam lam lamExprPl

isParamAlwaysUsedWithGetField :: Tree (V.Lam V.Var V.Term) (Ann a) -> Bool
isParamAlwaysUsedWithGetField (V.Lam param bod) =
    go False bod
    where
        go isGetFieldChild expr =
            case expr ^. val of
            V.BLeaf (V.LVar v) | v == param -> isGetFieldChild
            V.BGetField (V.GetField r _) -> go True r
            x -> all (go False) (x ^.. traverseK1)

-- Post process param add and delete actions to detach lambda.
-- This isn't done for all actions as some already perform this function.
-- TODO: clean up responsibilities - make it clear why some actions already
-- take care of wrapping and some don't.
postProcessActions ::
    Monad m => T m () -> ConventionalParams m -> ConventionalParams m
postProcessActions post x
    | Lens.has (cpParams . Lens._Just . _Params) x =
        x & cpParams . Lens._Just . SugarLens.binderFuncParamActions . fpDelete %~ (<* post)
    | otherwise = x

convertLamParams ::
    Monad m =>
    Tree (V.Lam V.Var V.Term) (Ann (Input.Payload m a)) ->
    Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertLamParams = convertNonEmptyParams Nothing BinderKindLambda

makeFieldParam :: Input.Payload m a -> (T.Tag, Tree Pure T.Type) -> FieldParam
makeFieldParam lambdaPl (tag, typeExpr) =
    FieldParam
    { fpTag = tag
    , fpFieldType = typeExpr
    , fpValue =
        lambdaPl ^. Input.evalResults
        <&> (^. Input.eAppliesOfLam)
        <&> Lens.mapped . Lens.mapped . _2 %~ ER.extractField typeExpr tag
        <&> Lens.mapped %~
            filter (Lens.nullOf (_2 . val . ER._RError))
    }

convertNonEmptyParams ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    BinderKind m ->
    Tree (V.Lam V.Var V.Term) (Ann (Input.Payload m a)) ->
    Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertNonEmptyParams mPresMode binderKind lambda lambdaPl =
    do
        tagsInOuterScope <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siTagParamInfos)
            <&> Map.keysSet
        sugarParamsRecord <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.parametersRecord)
        case lambdaPl ^. Input.inferredType . _Pure of
            T.TFun (FuncType (Pure (T.TRecord composite)) _)
                | sugarParamsRecord
                , FlatRowExtends fieldsMap (Pure T.REmpty) <- composite ^. T.flatRow
                , let fields = Map.toList fieldsMap
                , List.isLengthAtLeast 2 fields
                , isParamAlwaysUsedWithGetField lambda
                , let myTags = fields <&> fst & Set.fromList
                , let fieldParams = fields <&> makeFieldParam lambdaPl
                ->
                    if Set.null (tagsInOuterScope `Set.intersection` myTags)
                    then
                        do
                            presModeTags <- Lens._Just getP mPresMode <&> mPresModeToTags
                            paramList <-
                                getP (Anchors.assocFieldParamList (lambdaPl ^. Input.stored . Property.pVal))
                                <&> (^.. Lens._Just . traverse)
                            let presModeOrder tag =
                                    ( takeWhile (/= tag) presModeTags & length
                                    , takeWhile (/= tag) paramList & length
                                    )
                            convertRecordParams mPresMode binderKind
                                (fieldParams & List.sortOn (presModeOrder . fpTag))
                                lambda lambdaPl
                    else
                        convertNonRecordParam binderKind lambda lambdaPl
                        <&> cpParamInfos <>~ (fieldParams & map mkCollidingInfo & mconcat)
            _ -> convertNonRecordParam binderKind lambda lambdaPl
    where
        param = lambda ^. V.lamIn
        mkCollidingInfo fp = mkParamInfo param fp <&> ConvertM.CollidingFieldParam
        mPresModeToTags p =
            case p of
            Just (Object t) -> [t]
            Just (Infix t0 t1) -> [t0, t1]
            _ -> []

convertVarToCalls ::
    Monad m => T m (ValI m) -> V.Var -> Val (ValP m) -> T m ()
convertVarToCalls mkArg var =
    SubExprs.onMatchingSubexprs (Property.modify_ ?? change) (ExprLens.valVar . Lens.only var)
    where
        change x = mkArg >>= ExprIRef.newValI . V.BApp . V.App x

convertBinderToFunction ::
    Monad m =>
    T m (ValI m) -> BinderKind m -> Val (ValP m) ->
    T m (V.Var, ValP m)
convertBinderToFunction mkArg binderKind x =
    do
        (newParam, newValP) <- DataOps.lambdaWrap (x ^. ann)
        case binderKind of
            BinderKindDef defI ->
                convertVarToCalls mkArg (ExprIRef.globalId defI) x
            BinderKindLet redexLam ->
                convertVarToCalls mkArg
                (redexLam ^. V.lamIn) (redexLam ^. V.lamOut)
            BinderKindLambda -> error "Lambda will never be an empty-params binder"
        pure (newParam, newValP)

convertEmptyParams ::
    Monad m =>
    BinderKind m -> Val (Input.Payload m a) -> ConvertM m (ConventionalParams m)
convertEmptyParams binderKind x =
    ConvertM.postProcessAssert
    <&>
    \postProcess ->
    ConventionalParams
    { cpTags = mempty
    , _cpParamInfos = Map.empty
    , _cpParams = Nothing
    , _cpAddFirstParam =
        do
            (newParam, _) <-
                x & annotations %~ (^. Input.stored)
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
        postProcess <- ConvertM.postProcessAssert
        case expr ^. val of
            V.BLam lambda ->
                convertNonEmptyParams (Just presMode) binderKind lambda (expr ^. ann)
                <&> f
                where
                    f convParams =
                        (mPresMode convParams, convParams, lambda ^. V.lamOut)
                    mPresMode convParams =
                        presMode <$ convParams ^? cpParams . Lens._Just . _Params . Lens.ix 1
                    presMode = Anchors.assocPresentationMode defVar
            _ -> convertEmptyParams binderKind expr <&> (Nothing, , expr)
            <&> _2 %~ postProcessActions postProcess
