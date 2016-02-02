{-# LANGUAGE NoImplicitPrelude, RecordWildCards, PatternGuards #-}
module Lamdu.Sugar.Convert.Binder.Params
    ( ConventionalParams(..), cpParams
    , convertParams, convertLamParams
    , mkStoredLam, makeDeleteLambda
    , StoredLam(..), slLam
    , NewParamPosition(..), convertToRecordParams, addFieldParam
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
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
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
import           Lamdu.Sugar.OrderTags (orderType, orderedClosedFlatComposite)
import           Lamdu.Sugar.Types

import           Prelude.Compat

type T = Transaction

data ConventionalParams m = ConventionalParams
    { cpTags :: Set T.Tag
    , _cpParamInfos :: Map T.Tag ConvertM.TagFieldParam
    , _cpParams :: BinderParams Guid m
    , cpAddFirstParam :: T m ParamAddResult
    , cpScopes :: BinderBodyScope
    , cpMLamParam :: Maybe V.Var
    }

cpParams :: Lens' (ConventionalParams m) (BinderParams Guid m)
cpParams f ConventionalParams {..} = f _cpParams <&> \_cpParams -> ConventionalParams{..}

cpParamInfos :: Lens' (ConventionalParams m) (Map T.Tag ConvertM.TagFieldParam)
cpParamInfos f ConventionalParams {..} = f _cpParamInfos <&> \_cpParamInfos -> ConventionalParams{..}

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
    Lens.nullOf (ExprLens.valGlobals Set.empty . Lens.only funcVar) cur &&
    Lens.has
    (ExprLens.valApply . V.applyFunc . ExprLens.valVar . Lens.only funcVar)
    parent
isArgOfCallTo _ _ = False

changeCallArgs ::
    MonadA m =>
    (ValI m -> T m (ValI m)) -> Val (ValIProperty m) -> V.Var -> T m ()
changeCallArgs change val var =
    SubExprs.onMatchingSubexprsWithPath changeArg (isArgOfCallTo var) val
    where
        changeArg prop =
            Property.value prop & change >>= Property.set prop

changeRecursionsFromCalls ::
    MonadA m => DefI m -> Val (ValIProperty m) -> T m ()
changeRecursionsFromCalls defI =
    SubExprs.onMatchingSubexprs changeRecursion
    ( V.body . ExprLens._BApp . V.applyFunc . ExprLens.valVar
    . Lens.only (ExprIRef.globalId defI)
    )
    where
        changeRecursion prop =
            do
                body <- ExprIRef.readValBody (Property.value prop)
                case body of
                    V.BApp (V.Apply f _) -> Property.set prop f
                    _ -> error "assertion: expected BApp"

changeRecursionsToCalls :: MonadA m => DefI m -> Val (ValIProperty m) -> T m ()
changeRecursionsToCalls defI =
    SubExprs.onMatchingSubexprs changeRecursion
    (ExprLens.valVar . Lens.only (ExprIRef.globalId defI))
    where
        changeRecursion prop =
            DataOps.newHole
            >>= ExprIRef.newValBody . V.BApp . V.Apply (Property.value prop)
            >>= Property.set prop

addFieldParam ::
    MonadA m => (T.Tag -> ParamList) -> StoredLam m -> T m (TagG ())
addFieldParam mkNewTags storedLam =
    do
        tag <- newTag
        mkNewTags tag & setParamList (slParamList storedLam)
        return TagG
            { _tagInstance =
                EntityId.ofLambdaTagParam
                (storedLam ^. slLam . V.lamParamId) tag
            , _tagVal = tag
            , _tagGName = ()
            }

fixUsagesOfLamBinder ::
    MonadA m => (ValI m -> T m (ValI m)) -> BinderKind m -> StoredLam m -> T m ()
fixUsagesOfLamBinder fixOp binderKind storedLam =
    case binderKind of
    BinderKindDef defI ->
        changeCallArgs fixOp (storedLam ^. slLam . V.lamResult) (ExprIRef.globalId defI)
    BinderKindLet redexLam ->
        changeCallArgs fixOp (redexLam ^. V.lamResult) (redexLam ^. V.lamParamId)
    BinderKindLambda -> return ()

makeAddFieldParam ::
    MonadA m =>
    BinderKind m -> (T.Tag -> ParamList) -> StoredLam m ->
    ConvertM m (T m ParamAddResult)
makeAddFieldParam binderKind mkNewTags storedLam =
    do
        wrapOnError <- ConvertM.wrapOnTypeError
        return $
            do
                tagG <- addFieldParam mkNewTags storedLam
                let addFieldToCall argI =
                        do
                            hole <- DataOps.newHole
                            V.RecExtend (tagG ^. tagVal) hole argI
                                & V.BRecExtend & ExprIRef.newValBody
                fixUsagesOfLamBinder addFieldToCall binderKind storedLam
                void $ wrapOnError $ slLambdaProp storedLam
                ParamAddResultNewTag tagG & return

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

fixRecursiveCallRemoveField ::
    MonadA m =>
    T.Tag -> ValI m -> T m (ValI m)
fixRecursiveCallRemoveField tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> return restI
                | otherwise ->
                    do
                        newRestI <- fixRecursiveCallRemoveField tag restI
                        when (newRestI /= restI) $
                            ExprIRef.writeValBody argI $
                            V.BRecExtend $ V.RecExtend t v newRestI
                        return argI
            _ -> return argI

fixRecursiveCallToSingleArg ::
    MonadA m => T.Tag -> ValI m -> T m (ValI m)
fixRecursiveCallToSingleArg tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> return v
                | otherwise -> fixRecursiveCallToSingleArg tag restI
            _ -> return argI

tagGForLambdaTagParam :: V.Var -> T.Tag -> TagG ()
tagGForLambdaTagParam paramVar tag = TagG (EntityId.ofLambdaTagParam paramVar tag) tag ()

makeDelFieldParam ::
    MonadA m =>
    BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m ->
    ConvertM m (T m ParamDelResult)
makeDelFieldParam binderKind tags fp storedLam =
    do
        wrapOnError <- ConvertM.wrapOnTypeError
        return $
            do
                Transaction.setP (slParamList storedLam) newTags
                getFieldParamsToHole tag (storedLam ^. slLam)
                mLastTag
                    & traverse_ (getFieldParamsToParams (storedLam ^. slLam))
                fixUsagesOfLamBinder fixRecurseArg binderKind storedLam
                _ <- wrapOnError $ slLambdaProp storedLam
                return delResult
    where
        paramVar = storedLam ^. slLam . V.lamParamId
        tag = fpTag fp
        fixRecurseArg =
            maybe (fixRecursiveCallRemoveField tag)
            fixRecursiveCallToSingleArg mLastTag
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

makeFieldParamActions ::
    MonadA m =>
    BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m ->
    ConvertM m (FuncParamActions m)
makeFieldParamActions binderKind tags fp storedLam =
    do
        addParam <- makeAddFieldParam binderKind mkNewTags storedLam
        delParam <- makeDelFieldParam binderKind tags fp storedLam
        pure FuncParamActions
            { _fpAddNext = addParam
            , _fpDelete = delParam
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
    ConvertM m (ConventionalParams m)
convertRecordParams binderKind fieldParams lam@(V.Lam param _) pl =
    do
        params <- traverse mkParam fieldParams
        addFirstParam <- makeAddFieldParam binderKind (:tags) storedLam
        pure ConventionalParams
            { cpTags = Set.fromList tags
            , _cpParamInfos = fieldParams <&> mkFieldParamInfo & mconcat
            , _cpParams = FieldParams params
            , cpAddFirstParam = addFirstParam
            , cpScopes = BinderBodyScope $ mkCpScopesOfLam pl
            , cpMLamParam = Just param
            }
    where
        tags = fieldParams <&> fpTag
        mkFieldParamInfo fp = mkParamInfo param fp <&> ConvertM.TagFieldParam
        storedLam = mkStoredLam lam pl
        mkParam fp =
            do
                actions <- makeFieldParamActions binderKind tags fp storedLam
                pure
                    ( fpTag fp
                    , FuncParam
                        { _fpInfo =
                          NamedParamInfo
                          { _npiName = UniqueId.toGuid $ fpTag fp
                          , _npiActions = actions
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
                        changeRecursionsFromCalls defI lamBodyStored
                    BinderKindLambda -> return ()
                    BinderKindLet _ -> return ()
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

wrapArgWithRecord :: MonadA m => VarToTags -> ValI m -> T m (ValI m)
wrapArgWithRecord varToTags oldArg =
    do
        hole <- DataOps.newHole
        ExprIRef.newValBody (V.BLeaf V.LRecEmpty)
            >>= ExprIRef.newValBody . V.BRecExtend
                . V.RecExtend (vttNewTag varToTags ^. tagVal) hole
            >>= ExprIRef.newValBody . V.BRecExtend
                . V.RecExtend (vttReplacedByTag varToTags ^. tagVal) oldArg

data NewParamPosition = NewParamBefore | NewParamAfter

convertToRecordParams ::
    MonadA m => StoredLam m -> NewParamPosition -> T m VarToTags
convertToRecordParams storedLam newParamPosition =
    do
        tagForVar <- newTag
        tagForNewVar <- newTag
        case newParamPosition of
            NewParamBefore -> [tagForNewVar, tagForVar]
            NewParamAfter -> [tagForVar, tagForNewVar]
            & setParamList paramList
        convertVarToGetField tagForVar paramVar
            (storedLam ^. slLam . V.lamResult)
        return VarToTags
            { vttReplacedVar = paramVar
            , vttReplacedVarEntityId = EntityId.ofLambdaParam paramVar
            , vttReplacedByTag = tagGForLambdaTagParam paramVar tagForVar
            , vttNewTag = tagGForLambdaTagParam paramVar tagForNewVar
            }
    where
        paramVar = storedLam ^. slLam . V.lamParamId
        paramList =
            slLambdaProp storedLam & Property.value
            & Anchors.assocFieldParamList

makeConvertToRecordParams ::
    MonadA m => BinderKind m -> StoredLam m ->
    ConvertM m (NewParamPosition -> T m ParamAddResult)
makeConvertToRecordParams binderKind storedLam =
    do
        wrapOnError <- ConvertM.wrapOnTypeError
        return $ \newParamPosition ->
            do
                varToTags <- convertToRecordParams storedLam newParamPosition
                fixUsagesOfLamBinder (wrapArgWithRecord varToTags) binderKind storedLam
                _ <- wrapOnError (slLambdaProp storedLam)
                ParamAddResultVarToTags varToTags & return

lamParamType :: Input.Payload m a -> Type
lamParamType lamExprPl =
    unsafeUnjust "Lambda value not inferred to a function type?!" $
    lamExprPl ^? Input.inferredType . ExprLens._TFun . _1

makeNonRecordParamActions ::
    MonadA m => BinderKind m -> StoredLam m ->
    ConvertM m (FuncParamActions m, T m ParamAddResult)
makeNonRecordParamActions binderKind storedLam =
    do
        delete <- makeDeleteLambda binderKind storedLam
        addParam <- makeConvertToRecordParams binderKind storedLam
        return
            ( FuncParamActions
                { _fpAddNext = addParam NewParamAfter
                , _fpDelete = delete
                , _fpMOrderBefore = Nothing
                , _fpMOrderAfter = Nothing
                }
            , addParam NewParamBefore
            )

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
        (funcParamActions, addParam) <- makeNonRecordParamActions binderKind storedLam
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
            , cpAddFirstParam = addParam
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
    BinderKind m -> V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a ->
    ConvertM m (ConventionalParams m)
convertLamParams binderKind lambda lambdaPl =
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
        let param = lambda ^. V.lamParamId
        let mkCollidingInfo fp = mkParamInfo param fp <&> ConvertM.CollidingFieldParam
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
                   then convertRecordParams binderKind fieldParams lambda lambdaPl
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

convertEmptyParams :: MonadA m =>
    BinderKind m -> Val (Input.Payload m a) -> ConvertM m (ConventionalParams m)
convertEmptyParams binderKind val =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let makeAddFirstParam storedVal =
                do
                    (newParam, dst) <- DataOps.lambdaWrap (storedVal ^. V.payload)
                    case binderKind of
                        BinderKindDef defI -> changeRecursionsToCalls defI storedVal
                        BinderKindLet _ -> return ()
                        BinderKindLambda -> return ()
                    void $ protectedSetToVal (storedVal ^. V.payload) dst
                    return $
                        ParamAddResultNewVar (EntityId.ofLambdaParam newParam) newParam

        pure
            ConventionalParams
            { cpTags = mempty
            , _cpParamInfos = Map.empty
            , _cpParams = BinderWithoutParams
            , cpAddFirstParam = val <&> (^. Input.stored) & makeAddFirstParam
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
                convertLamParams binderKind lambda (expr ^. V.payload)
                -- The lambda disappears here, so add its id to the first
                -- param's hidden ids:
                <&> cpParams . _VarParam . fpHiddenIds <>~ hiddenIds
                <&> cpParams . _FieldParams . Lens.ix 0 . _2 . fpHiddenIds <>~ hiddenIds
            return (params, lambda ^. V.lamResult)
        where
              hiddenIds = [expr ^. V.payload . Input.entityId]
    _ ->
        do
            params <- convertEmptyParams binderKind expr
            return (params, expr)
