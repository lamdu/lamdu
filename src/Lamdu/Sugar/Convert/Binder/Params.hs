{-# LANGUAGE TemplateHaskell, TupleSections, TypeFamilies, TypeApplications #-}
module Lamdu.Sugar.Convert.Binder.Params
    ( ConventionalParams(..), cpParams, cpAddFirstParam, cpMLamParam
    , convertLamParams, convertNonEmptyParams, convertEmptyParams
    , mkStoredLam, makeDeleteLambda
    , convertBinderToFunction
    , convertToRecordParams
    , StoredLam(..), slLam, slLambdaProp
    , NewParamPosition(..), addFieldParam
    , isParamAlwaysUsedWithGetField
    , mkVarInfo
    ) where

import qualified Control.Lens.Extended as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import qualified Data.List.Extended as List
import           Data.Maybe.Extended (unsafeUnjust)
import           Data.Property (MkProperty', modP)
import qualified Data.Set as Set
import           Hyper
import           Hyper.Syntax (FuncType(..), funcIn)
import           Hyper.Syntax.Nominal (NominalInst(..))
import           Hyper.Syntax.Row (RowExtend(..), FlatRowExtends(..))
import qualified Hyper.Syntax.Row as Row
import           Hyper.Type.Functor (F)
import           Hyper.Type.Prune (Prune(..), _Unpruned)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.TaggedList as ConvertTaggedList
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.IRef (IRef)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ConventionalParams m = ConventionalParams
    { cpTags :: Set T.Tag
    , _cpParams :: Maybe (LhsNames InternalName (OnceT (T m)) (T m) EvalPrep)
    , _cpAddFirstParam :: AddParam InternalName (OnceT (T m)) (T m)
    , _cpMLamParam :: Maybe ({- lambda's -}EntityId, V.Var)
    }
Lens.makeLenses ''ConventionalParams

data FieldParam = FieldParam
    { fpTag :: T.Tag
    , fpFieldType :: Pure # T.Type
    }

data StoredLam m = StoredLam
    { _slLam :: V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m)
    , _slLambdaProp :: HRef m # V.Term
    }
Lens.makeLenses ''StoredLam

mkStoredLam ::
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term -> StoredLam m
mkStoredLam lam pl =
    StoredLam
    (hmap (Proxy @(Recursively HFunctor) #>  hflipped %~ hmap (const (^. Input.stored))) lam)
    (pl ^. Input.stored)

unappliedUsesOfVar :: V.Var -> Ann a # V.Term -> [a # V.Term]
unappliedUsesOfVar var (Ann pl (V.BLeaf (V.LVar v)))
    | v == var = [pl]
unappliedUsesOfVar var (Ann _ (V.BApp (App f x))) =
    rf <> rx
    where
        rf  | Lens.has ExprLens.valVar f = []
            | otherwise = unappliedUsesOfVar var f
        rx  | Lens.has ExprLens.valHole f && Lens.has ExprLens.valVar x = []
            | otherwise = unappliedUsesOfVar var x
unappliedUsesOfVar var x =
    hfoldMap
    ( \case
        HWitness V.W_Term_Term -> unappliedUsesOfVar var
        _ -> const []
    ) (x ^. hVal)

wrapUnappliedUsesOfVar :: Monad m => V.Var -> Ann (HRef m) # V.Term -> T m ()
wrapUnappliedUsesOfVar var = traverse_ DataOps.applyHoleTo . unappliedUsesOfVar var

argsOfCallTo :: V.Var -> Ann a # V.Term -> [a # V.Term]
argsOfCallTo var (Ann _ (V.BApp (App (Ann _ (V.BLeaf (V.LVar v))) x)))
    | v == var = [x ^. hAnn]
argsOfCallTo var x =
    hfoldMap
    ( \case
        HWitness V.W_Term_Term -> argsOfCallTo var
        _ -> const []
    ) (x ^. hVal)

changeCallArgs ::
    Monad m =>
    (ValI m -> T m (ValI m)) -> Ann (HRef m) # V.Term -> V.Var -> T m ()
changeCallArgs change v var =
    do
        argsOfCallTo var v & traverse_ (\x -> x ^. ExprIRef.iref & change >>= x ^. ExprIRef.setIref)
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
        changeCallArgs fixOp (storedLam ^. slLam . V.tlOut) (ExprIRef.globalId defI)
    BinderKindLet redexLam ->
        changeCallArgs fixOp (redexLam ^. V.tlOut) (redexLam ^. V.tlIn)
    BinderKindLambda ->
        protectedSetToVal prop (prop ^. ExprIRef.iref) & void
        where
            prop = storedLam ^. slLambdaProp

writeNewParamList ::
    Monad m =>
    [T.Tag] ->
    T m (F (IRef m) # HCompose Prune T.Type)
writeNewParamList tags =
    hcomposed _Unpruned . T._TRecord . _HCompose #
    foldl extend (newTerm (hcomposed _Unpruned # T.REmpty)) tags
    & newTerm
    & ExprIRef.writeRecursively
    <&> (^. hAnn . _1)
    where
        newTerm = Ann (ExprIRef.WriteNew :*: Const ())
        extend rest f =
            hcomposed _Unpruned . T._RExtend #
            RowExtend f (_HCompose # newTerm (_HCompose # Pruned)) (_HCompose # rest)
            & newTerm

addFieldParam ::
    Monad m =>
    ConvertM m
    (T m (ValI m) -> BinderKind m -> StoredLam m -> (T.Tag -> [T.Tag]) -> T.Tag -> T m ())
addFieldParam =
    fixLamUsages
    <&>
    \fixUsages mkArg binderKind storedLam mkTags tag ->
    do
        let t = storedLam ^. slLam . V.tlInType
        case t ^. hVal . _HCompose of
            Pruned ->
                writeNewParamList (mkTags tag)
                >>= t ^. hAnn . ExprIRef.setIref
            Unpruned (HCompose (T.TRecord (HCompose r))) ->
                do
                    fieldType <- _HCompose # Pruned & ExprIRef.newValI
                    hcomposed _Unpruned . T._RExtend #
                        RowExtend tag (_HCompose # fieldType) (_HCompose # (r ^. hAnn . ExprIRef.iref))
                        & ExprIRef.newValI
                        >>= r ^. hAnn . ExprIRef.setIref
            _ -> error "adding field to type that isn't a record!"
        let addFieldToCall argI =
                do
                    newArg <- mkArg
                    RowExtend tag newArg argI
                        & V.BRecExtend & ExprIRef.newValI
        fixUsages addFieldToCall binderKind storedLam

getFieldOnVar :: Lens.Traversal' (Pure # V.Term) (V.Var, T.Tag)
getFieldOnVar =
    _Pure . V._BApp . inApp
    where
        inApp f (V.App (Pure (V.BLeaf (V.LGetField t))) (Pure (V.BLeaf (V.LVar v)))) =
            f (v, t) <&> pack
        inApp _ other = pure other
        pack (v, t) = V.App (Pure (V.BLeaf (V.LGetField t))) (Pure (V.BLeaf (V.LVar v)))

getFieldParamsToHole ::
    Monad m =>
    T.Tag -> V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) -> T m ()
getFieldParamsToHole tag (V.TypedLam param _paramTyp lamBody) =
    SubExprs.onMatchingSubexprs SubExprs.toHole (getFieldOnVar . Lens.only (param, tag)) lamBody

getFieldParamsToParams ::
    Monad m =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (HRef m) -> T.Tag -> T m ()
getFieldParamsToParams (V.TypedLam param _paramTyp lamBody) tag =
    SubExprs.onMatchingSubexprs (toParam . (^. ExprIRef.iref))
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
        case (mNewTags, storedParamType ^. hVal . _HCompose) of
            (Just newTags, Pruned) ->
                writeNewParamList newTags
                >>= storedParamType ^. hAnn . ExprIRef.setIref
            (Just{}, Unpruned (HCompose (T.TRecord (HCompose r)))) ->
                r & hflipped %~ hmap (\_ i -> ExprIRef.ExistingRef (i ^. ExprIRef.iref) :*: Const ())
                & removeField
                & ExprIRef.writeRecursively
                <&> (^. hAnn . _1)
                >>= r ^. hAnn . ExprIRef.setIref
            (Just{}, Unpruned _) -> error "removing field from type that isn't a record!"
            (Nothing, _) ->
                _HCompose # Pruned & ExprIRef.newValI
                >>= storedParamType ^. hAnn . ExprIRef.setIref
        getFieldParamsToHole tag (storedLam ^. slLam)
        traverse_ onLastTag mLastTag
        fixUsages fixRecurseArg binderKind storedLam
    where
        removeField (Ann a (HCompose (Unpruned (HCompose (T.RExtend (RowExtend f t rest))))))
            | f == fpTag fp = rest ^. _HCompose
            | otherwise =
                rest & _HCompose %~ removeField
                & RowExtend f t
                & Ann a . (hcomposed _Unpruned . T._RExtend #)
        removeField (Ann _ _) = error "expected field not preset!"
        storedParamType = storedLam ^. slLam . V.tlInType
        onLastTag lastTag =
            do
                getFieldParamsToParams (storedLam ^. slLam) lastTag
                setP (Anchors.assocTag (storedLam ^. slLam . V.tlIn)) lastTag
        tag = fpTag fp
        fixRecurseArg =
            maybe (fixCallArgRemoveField tag)
            fixCallToSingleArg mLastTag
        (mNewTags, mLastTag) =
            case List.delete tag tags of
            [x] -> (Nothing, Just x)
            xs -> (Just xs, Nothing)

fieldParamInfo ::
    Monad m =>
    BinderKind m -> [T.Tag] -> FieldParam -> StoredLam m ->
    TagRef InternalName (OnceT (T m)) (T m) ->
    ConvertM m (TaggedItem InternalName (OnceT (T m)) (T m)
        (LhsField InternalName (OnceT (T m)) (T m) EvalPrep))
fieldParamInfo binderKind tags fp storedLam tag =
    do
        postProcess <- ConvertM.postProcessAssert
        add <- addFieldParam
        let resultInfo () newTag =
                ConvertTag.TagResultInfo
                (EntityId.ofTaggedEntity param newTag)
                (add DataOps.newHole binderKind storedLam (: tags) newTag >> postProcess)
        addNext <-
            ConvertTag.replace (nameWithContext Nothing param) (Set.fromList tags) (pure ()) resultInfo
            >>= ConvertM . lift
        del <- delFieldParamAndFixCalls binderKind tags fp storedLam
        vinfo <- mkVarInfo (fpFieldType fp)
        pure TaggedItem
            { _tiTag = tag
            , _tiAddAfter = addNext
            , _tiDelete = del <* postProcess
            , _tiValue =
                LhsField
                { _fParam =
                    FuncParam
                    { _fpAnnotation = EvalPrep (fpFieldType fp) (tag ^. tagRefTag . tagInstance)
                    , _fpUsages = []
                    , _fpVarInfo = vinfo
                    }
                , _fSubFields = Nothing
                }
            }
    where
        param = storedLam ^. slLam . V.tlIn

changeGetFieldTags ::
    Monad m =>
    V.Var -> T.Tag -> T.Tag -> Ann (HRef m) # V.Term -> T m ()
changeGetFieldTags param prevTag chosenTag x =
    case x ^. hVal of
    V.BApp (V.App (Ann a (V.BLeaf (V.LGetField t))) (Ann _ (V.BLeaf (V.LVar v))))
        | v == param && t == prevTag ->
            V.LGetField chosenTag & V.BLeaf & ExprIRef.writeValI (a ^. ExprIRef.iref)
        | otherwise -> pure ()
    V.BLeaf (V.LVar v)
        | v == param -> DataOps.applyHoleTo (x ^. hAnn) & void
    b ->
        htraverse_
        ( \case
            HWitness V.W_Term_Term -> changeGetFieldTags param prevTag chosenTag
            _ -> const (pure ())
        ) b

setFieldParamTag ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) -> BinderKind m ->
    StoredLam m -> [T.Tag] -> T.Tag -> ConvertM m (T.Tag -> T m ())
setFieldParamTag mPresMode binderKind storedLam prevTagList prevTag =
    (,) <$> fixLamUsages <*> ConvertM.postProcessAssert
    <&>
    \(fixUsages, postProcess) chosenTag ->
    do
        traverse_ (`modP` (<&> Lens.filteredBy (Lens.only prevTag) .~ chosenTag)) mPresMode
        case storedParamType ^. hVal . _HCompose of
            Pruned ->
                writeNewParamList (prevTagList <&> Lens.filteredBy (Lens.only prevTag) .~ chosenTag)
                >>= storedParamType ^. hAnn . ExprIRef.setIref
            Unpruned (HCompose (T.TRecord (HCompose r))) ->
                r & hflipped %~ hmap (\_ i -> ExprIRef.ExistingRef (i ^. ExprIRef.iref) :*: Const ())
                & changeField
                & ExprIRef.writeRecursively
                <&> (^. hAnn . _1)
                >>= r ^. hAnn . ExprIRef.setIref
                where
                    changeField (Ann a (HCompose (Unpruned (HCompose (T.RExtend (RowExtend f t rest)))))) =
                        ( if prevTag == f
                            then RowExtend chosenTag t rest
                            else rest & _HCompose %~ changeField & RowExtend f t
                        ) & Ann a . (hcomposed _Unpruned . T._RExtend #)
                    changeField (Ann _ _) = error "expected field not preset!"
            _ -> error "changing field in type that isn't a record!"
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
            (storedLam ^. slLam . V.tlIn) prevTag chosenTag
            (storedLam ^. slLam . V.tlOut)
        postProcess
    where
        storedParamType = storedLam ^. slLam . V.tlInType

convertRecordParams ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    BinderKind m -> [FieldParam] ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ConventionalParams m)
convertRecordParams mPresMode binderKind fieldParams lam@(V.TypedLam param _ _) lamPl =
    do
        ps <- traverse mkParam fieldParams
        postProcess <- ConvertM.postProcessAssert
        add <- addFieldParam
        let resultInfo () tag =
                ConvertTag.TagResultInfo
                (EntityId.ofTaggedEntity param tag)
                (add DataOps.newHole binderKind storedLam (: (fieldParams <&> fpTag)) tag >> postProcess)
        addFirstSelection <-
            ConvertTag.replace (nameWithContext Nothing param) (Set.fromList tags) (pure ()) resultInfo >>= ConvertM . lift
        pure ConventionalParams
            { cpTags = Set.fromList tags
            , _cpParams = ConvertTaggedList.convert addFirstSelection ps & LhsRecord & Just
            , _cpAddFirstParam = AddNext addFirstSelection
            , _cpMLamParam = Just (entityId, param)
            }
    where
        entityId = lamPl ^. Input.entityId
        tags = fieldParams <&> fpTag
        storedLam = mkStoredLam lam lamPl
        mkParam fp =
            do
                setField <- setFieldParamTag mPresMode binderKind storedLam tagList tag
                let resultInfo () = ConvertTag.TagResultInfo <$> EntityId.ofTaggedEntity param <*> setField
                ConvertTag.ref tag (Just (ConvertTag.NameContext Nothing (UniqueId.toUUID param)))
                    (Set.delete tag (Set.fromList tagList)) (pure ()) resultInfo
                    >>= ConvertM . lift
                    >>= fieldParamInfo binderKind tags fp storedLam
            where
                tag = fpTag fp
                tagList = fieldParams <&> fpTag

removeCallsToVar ::
    Monad m =>
    V.Var -> Ann (HRef m) # V.Term -> T m ()
removeCallsToVar funcVar x =
    do
        SubExprs.onMatchingSubexprs changeRecursion
            ( _Pure . V._BApp . V.appFunc
            . _Pure . V._BLeaf . V._LVar . Lens.only funcVar
            ) x
        wrapUnappliedUsesOfVar funcVar x
    where
        changeRecursion prop =
            ExprIRef.readValI (prop ^. ExprIRef.iref)
            >>= \case
            V.BApp (V.App f _) -> (prop ^. ExprIRef.setIref) f
            _ -> error "assertion: expected BApp"

makeDeleteLambda :: Monad m => BinderKind m -> StoredLam m -> ConvertM m (T m ())
makeDeleteLambda binderKind (StoredLam (V.TypedLam paramVar _paramTyp lamBodyStored) lambdaProp) =
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
                (redexLam ^. V.tlIn) (redexLam ^. V.tlOut)
            BinderKindLambda -> pure ()
        let lamBodyI = lamBodyStored ^. hAnn . ExprIRef.iref
        protectedSetToVal lambdaProp lamBodyI & void

convertVarToGetField ::
    Monad m =>
    T.Tag -> V.Var -> Ann (HRef m) # V.Term -> T m ()
convertVarToGetField tagForVar paramVar =
    SubExprs.onGetVars (convertVar . (^. ExprIRef.iref)) paramVar
    where
        convertVar bodyI =
            V.App
            <$> ExprIRef.newValI (V.BLeaf (V.LGetField tagForVar))
            <*> ExprIRef.newValI (V.BLeaf (V.LVar paramVar))
            <&> V.BApp
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
        let paramVar = storedLam ^. slLam . V.tlIn
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
            & Lens.itraverse_ (flip DataOps.setTagOrder)
        let t = storedLam ^. slLam . V.tlInType
        newParamType <- _HCompose # Pruned & ExprIRef.newValI
        hcomposed _Unpruned # T.REmpty
            & ExprIRef.newValI
            >>= extend oldParam (t ^. hAnn . ExprIRef.iref)
            >>= extend newParam newParamType
            <&> (hcomposed _Unpruned . T._TRecord . _HCompose #)
            >>= ExprIRef.newValI
            >>= t ^. hAnn . ExprIRef.setIref
        convertVarToGetField oldParam paramVar
            (storedLam ^. slLam . V.tlOut)
        fixUsages (wrapArgWithRecord mkNewArg oldParam newParam)
            binderKind storedLam
    where
        extend tag typ rest =
            hcomposed _Unpruned . T._RExtend #
            RowExtend tag (_HCompose # typ) (_HCompose # rest)
            & ExprIRef.newValI

lamParamType :: Input.Payload m # V.Term -> Pure # T.Type
lamParamType lamExprPl =
    unsafeUnjust "Lambda value not inferred to a function type?!" $
    lamExprPl ^? Input.inferredType . _Pure . T._TFun . funcIn

mkVarInfo :: MonadTransaction n m => Pure # T.Type -> m VarInfo
mkVarInfo (Pure T.TFun{}) = pure VarFunction
mkVarInfo (Pure T.TVar{}) = pure VarGeneric
mkVarInfo (Pure (T.TRecord (Pure T.REmpty))) = pure VarUnit
mkVarInfo (Pure T.TRecord{}) = pure VarRecord
mkVarInfo (Pure (T.TVariant (Pure T.REmpty))) = pure VarVoid
mkVarInfo (Pure T.TVariant{}) = pure VarVariant
mkVarInfo (Pure (T.TInst (NominalInst tid _))) =
    ConvertTId.convert tid
    <&> VarNominal . (tidName %~ (^. inTag))

convertNonRecordParam ::
    Monad m =>
    BinderKind m ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ConventionalParams m)
convertNonRecordParam binderKind lam@(V.TypedLam param _ _) lamExprPl =
    do
        nullParamSugar <-
            Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.nullaryParameter)
        let typ = lamParamType lamExprPl
        varInfo <- mkVarInfo typ
        tag <- ConvertTag.taggedEntity (Just varInfo) param >>= ConvertM . lift
        del <- makeDeleteLambda binderKind storedLam
        postProcess <- ConvertM.postProcessAssert
        oldParam <- Anchors.assocTag param & getP
        let mkAddParam pos
                | oldParam == Anchors.anonTag = EntityId.ofTaggedEntity param oldParam & NeedToPickTagToAddNext & pure
                | otherwise =
                    do
                        addParamAfter <- convertToRecordParams ?? DataOps.newHole ?? binderKind ?? storedLam ?? pos
                        let resultInfo () =
                                ConvertTag.TagResultInfo <$> EntityId.ofTaggedEntity param <*> (addParamAfter <&> (<* postProcess))
                        ConvertTag.replace (nameWithContext Nothing param) (Set.singleton oldParam) (pure ()) resultInfo
                            >>= ConvertM . lift <&> AddNext
        addPrev <- mkAddParam NewParamBefore
        addNext <- mkAddParam NewParamAfter
        let funcParam =
                LhsVar Var
                { _vParam =
                    FuncParam
                    { _fpAnnotation =
                        EvalPrep
                        { _eType = typ
                        , _eEvalId = tag ^. oTag . tagRefTag . tagInstance
                        }
                    , _fpVarInfo = varInfo
                    , _fpUsages =
                        -- TODO: Replace varRefsOfLambda mechanism with one that traverses the sugar,
                        -- So it goes to actual first use after reordering by sugar.
                        lamExprPl ^. Input.varRefsOfLambda
                    }
                , _vTag = tag
                , _vAddPrev = addPrev
                , _vAddNext = addNext
                , _vDelete = del <* postProcess
                , _vIsNullParam =
                    nullParamSugar
                    && Lens.has (_Pure . T._TRecord . _Pure . T._REmpty) typ
                    && null (lamExprPl ^. Input.varRefsOfLambda)
                }
        addFirst <-
            convertToRecordParams ?? DataOps.newHole ?? binderKind ?? storedLam
            ?? NewParamBefore
            <&> Lens.mapped %~ (<* postProcess)
        let resultInfo () = ConvertTag.TagResultInfo <$> EntityId.ofTaggedEntity param <*> addFirst
        addFirstParam <-
            do
                if oldParam == Anchors.anonTag
                    then NeedToPickTagToAddNext (EntityId.ofTaggedEntity param oldParam) & pure
                    else
                        ConvertTag.replace (nameWithContext (Just varInfo) param) (Set.singleton oldParam) (pure ()) resultInfo
                        >>= ConvertM . lift
                        <&> AddNext
        pure ConventionalParams
            { cpTags = mempty
            , _cpParams = Just funcParam
            , _cpAddFirstParam = addFirstParam
            , _cpMLamParam = Just (lamExprPl ^. Input.entityId, param)
            }
    where
        storedLam = mkStoredLam lam lamExprPl

isParamAlwaysUsedWithGetField :: V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann a -> Bool
isParamAlwaysUsedWithGetField (V.TypedLam param _paramTyp bod) =
    go False bod
    where
        go isGetFieldChild expr =
            case expr ^. hVal of
            V.BLeaf (V.LVar v) | v == param -> isGetFieldChild
            V.BApp (V.App (Ann _ (V.BLeaf V.LGetField{})) r) -> go True r
            x ->
                hfoldMap @_ @[Bool]
                ( \case
                    HWitness V.W_Term_Term -> (:[]) . go False
                    HWitness V.W_Term_HCompose_Prune_Type -> const []
                ) x
                & and

convertLamParams ::
    Monad m =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ConventionalParams m)
convertLamParams = convertNonEmptyParams Nothing BinderKindLambda

convertNonEmptyParams ::
    Monad m =>
    Maybe (MkProperty' (T m) PresentationMode) ->
    BinderKind m ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ConventionalParams m)
convertNonEmptyParams mPresMode binderKind lambda lambdaPl =
    do
        sugarLhsRecord <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.parametersRecord)
        case lambdaPl ^. Input.inferredType . _Pure of
            T.TFun (FuncType (Pure (T.TRecord composite)) _)
                | sugarLhsRecord
                , FlatRowExtends fieldsMap (Pure T.REmpty) <- composite ^. T.flatRow
                , let fields = fieldsMap ^@.. Lens.itraversed
                , List.isLengthAtLeast 2 fields
                , isParamAlwaysUsedWithGetField lambda
                , let fieldParams = fields <&> uncurry FieldParam
                -> convertRecordParams mPresMode binderKind fieldParams lambda lambdaPl
            _ -> convertNonRecordParam binderKind lambda lambdaPl

convertVarToCalls ::
    Monad m => T m (ValI m) -> V.Var -> Ann (HRef m) # V.Term -> T m ()
convertVarToCalls mkArg var =
    SubExprs.onMatchingSubexprs (\x -> x ^. ExprIRef.iref & change >>= x ^. ExprIRef.setIref)
    (_Pure . V._BLeaf . V._LVar . Lens.only var)
    where
        change x = mkArg >>= ExprIRef.newValI . V.BApp . V.App x

convertBinderToFunction ::
    (HasCallStack, Monad m) =>
    T m (ValI m) -> BinderKind m -> Ann (HRef m) # V.Term ->
    T m (V.Var, HRef m # V.Term)
convertBinderToFunction mkArg binderKind x =
    do
        (newParam, newValP) <- DataOps.lambdaWrap (x ^. hAnn)
        case binderKind of
            BinderKindDef defI ->
                convertVarToCalls mkArg (ExprIRef.globalId defI) x
            BinderKindLet redexLam ->
                convertVarToCalls mkArg
                (redexLam ^. V.tlIn) (redexLam ^. V.tlOut)
            BinderKindLambda -> error "Lambda will never be an empty-params binder"
        pure (newParam, newValP)

convertEmptyParams ::
    Monad m =>
    BinderKind m -> Ann (Input.Payload m) # V.Term -> ConvertM m (Transaction m EntityId)
convertEmptyParams binderKind x =
    ConvertM.postProcessAssert
    <&>
    \postProcess ->
    do
        (newParam, _) <-
            x & hflipped %~ hmap (const (^. Input.stored))
            & convertBinderToFunction DataOps.newHole binderKind
        postProcess
        EntityId.ofTaggedEntity newParam Anchors.anonTag & pure
