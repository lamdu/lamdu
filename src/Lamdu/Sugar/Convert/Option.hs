{-# LANGUAGE TemplateHaskell, TypeApplications, ScopedTypeVariables, GADTs, DerivingVia #-}

module Lamdu.Sugar.Convert.Option
    ( Result(..), rTexts, rExpr, rDeps, rAllowEmptyQuery, rWithTypeAnnotations
    , ResultQuery(..), _QueryTexts, _QueryNewTag
    , simpleResult
    , ResultGroups(..), filterResults
    , Matches, matchResult
    , TypeMatch(..)
    , makeTagRes, makeNoms, makeForType, makeLocals
    , suggestVal, genLamVar, suggestRec, suggestCase
    , getListing, makeGlobals
    , tagTexts, recTexts, caseTexts, ifTexts, symTexts, lamTexts
    , makeOption
    , taggedVar
    ) where

import qualified Control.Lens as Lens
import           Control.Monad ((>=>))
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.ByteString.Extended as BS
import           Data.Containers.ListUtils (nubOrd)
import           Data.List (sortOn)
import           Data.Property (MkProperty', getP, modP, pureModify, pVal)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import           GUI.Momentu.Direction (Layout(..))
import           Hyper
import           Hyper.Infer
import           Hyper.Recurse
import           Hyper.Syntax (FuncType(..), funcIn, funcOut)
import           Hyper.Syntax.Nominal (NominalInst, nId, nScheme)
import           Hyper.Syntax.Row (RowExtend(..), freExtends)
import           Hyper.Syntax.Scheme (sTyp)
import           Hyper.Type.Functor (_F)
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify (UVar, applyBindings, unify)
import           Hyper.Unify.Generalize (instantiate)
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Calc.Definition (Deps, depsNominals, depsGlobalTypes)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Infer as Infer
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.UniqueId (ToUUID(..))
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.I18N.UnicodeAlts (unicodeAlts)
import           Lamdu.Sugar.Annotations (ShowAnnotation, MarkAnnotations(..), alwaysShowAnnotations)
import           Lamdu.Sugar.Convert.Annotation (makeAnnotation)
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import           Lamdu.Sugar.Convert.Binder.Params (mkVarInfo)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import           Lamdu.Sugar.Lens.Annotations (HAnnotations(..))
import           Lamdu.Sugar.Types
import           Revision.Deltum.Hyper (Write(..), writeRecursively)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Matches a = Matches
    { _mExact :: a
    , _mPrefix :: a
    , _mInfix :: a
    , _mCreateNew :: a
    } deriving (Functor, Foldable, Generic, Traversable)
    deriving (Monoid, Semigroup) via (Generically (Matches a))
Lens.makeLenses ''Matches

data ResultQuery
    = QueryTexts !(TagSuffixes -> QueryLangInfo -> [Text])
    | QueryNewTag T.Tag
Lens.makePrisms ''ResultQuery

data Result a = Result
    { _rDeps :: !Deps
    , _rExpr :: !a
    , _rTexts :: !ResultQuery
    , _rWithTypeAnnotations :: !Bool
    , _rAllowEmptyQuery :: !Bool
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Result

simpleResult :: a -> (TagSuffixes -> QueryLangInfo -> [Text]) -> Result a
simpleResult expr texts =
    Result
    { _rDeps = mempty
    , _rExpr = expr
    , _rTexts = QueryTexts texts
    , _rWithTypeAnnotations = False
    , _rAllowEmptyQuery = True
    }

data TypeMatch = TypeMatches | TypeMismatch deriving (Eq, Ord)

data ResultGroups a = ResultGroups
    { gSyntax :: a
    , gDefs :: a
    , gLocals :: a
    , gInjects :: a
    , gToNoms :: a
    , gFromNoms :: a
    , gForType :: a
    , gGetFields :: a
    , gWrapInRecs :: a
    } deriving (Functor, Foldable, Traversable, Generic)
    deriving (Semigroup, Monoid) via (Generically (ResultGroups a))

filterResults ::
    (Monad m, Ord b) =>
    MkProperty' (T m) (Set T.Tag) ->
    (TypeMatch -> a -> b) ->
    ResultGroups (OnceT (T m) [Result (a, Option t name i (T m))]) -> Query ->
    OnceT (T m) [Option t name i (T m)]
filterResults tagsProp order res query =
    resGroups <&> (^. traverse)
    where
        resGroups
            | "" == query ^. qSearchTerm = groups (gForType <> gSyntax <> gLocals)
            | "." `Text.isPrefixOf` (query ^. qSearchTerm) =
                groups (gForType <> gSyntax <> gDefs <> gFromNoms <> gGetFields)
            | "'" `Text.isPrefixOf` (query ^. qSearchTerm) = groups (gForType <> gToNoms <> gInjects)
            | "{" `Text.isPrefixOf` (query ^. qSearchTerm) =
                groups (gForType <> gSyntax <> gWrapInRecs)
            | otherwise =
                -- Within certain search-term matching level (exact/prefix/infix),
                -- prefer locals over globals even for type mismatches
                groups (gForType <> gLocals) <> groups (gSyntax <> gDefs <> gToNoms)
        groups f =
            f res
            <&> Lens.mapped . Lens.filteredBy (rTexts . _QueryNewTag) <. rExpr . _2 . optionPick %@~
                (\t -> (modP tagsProp (Lens.contains t .~ True) <>))
            <&> foldMap (matchResult query)
            <&> fmap ((^.. traverse . _2) . sortOn s)
        s (i, opt) = order (if opt ^. optionTypeMatch then TypeMatches else TypeMismatch) i

matchResult :: Query -> Result a -> Matches [a]
matchResult query result
    | query ^. qSearchTerm == "" && not (result ^. rAllowEmptyQuery) = mempty
    | otherwise =
        case result ^. rTexts of
        QueryTexts makeTexts
            | s `elem` texts -> mempty & mExact .~ e
            | any (Text.isPrefixOf s) texts -> mempty & mPrefix .~ e
            | any (Text.isInfixOf s) texts -> mempty & mInfix .~ e
            | otherwise -> mempty
            where
                texts =
                    makeTexts (query ^. qTagSuffixes) (query ^. qLangInfo)
                    <&> Text.toLower >>= unicodeAlts
        QueryNewTag{} -> mempty & mCreateNew .~ e
    where
        e = [result ^. rExpr]
        s = query ^. qSearchTerm & Text.toLower

-- Suggest expression to fit a type.
-- Not used for subexpressions of suggested expression,
-- so may suggest multiple expressions.
suggestTopLevelVal :: Monad m => Pure # T.Type -> T m [(Deps, Pure # V.Term)]
suggestTopLevelVal t =
    (t ^.. _Pure . T._TFun . funcIn . _Pure . T._TInst & foldMap suggestFromNom) <>
    (t ^.. _Pure . T._TVariant & foldMap suggestVariantValues <&> Lens.mapped %~ (,) mempty) <>
    ( suggestVal t
        <&> (^? Lens.filtered (Lens.nullOf (_Pure . V._BLeaf . V._LHole)))
        <&> Lens._Just %~ (,) mempty
        <&> (^.. Lens._Just)
    )
    <&>
    (<> ((t ^.. _Pure . T._TFun . funcOut . _Pure . T._TVariant >>= suggestInjectOrGetFields V.LInject)
            <> (t ^.. _Pure . T._TFun . funcIn . _Pure . T._TRecord >>= suggestInjectOrGetFields V.LGetField)
            <&> (,) mempty
        )
    )

suggestFromNom :: Monad m => NominalInst NominalId T.Types # Pure -> Transaction m [(Deps, Pure # V.Term)]
suggestFromNom n =
    Load.nominal tid <&> (^.. Lens._Right) <&> Lens.mapped %~
    \s -> (mempty & depsNominals . Lens.at tid ?~ s, _Pure . V._BLeaf . V._LFromNom # tid)
    where
        tid = n ^. nId

suggestInjectOrGetFields :: (T.Tag -> V.Leaf) -> Pure # T.Row -> [Pure # V.Term]
suggestInjectOrGetFields o t =
    case t ^. _Pure of
    T.RExtend (RowExtend tag _ rest) -> Pure (V.BLeaf (o tag)) : suggestInjectOrGetFields o rest
    _ -> []

suggestVariantValues :: Monad m => Pure # T.Row -> T m [Pure # V.Term]
suggestVariantValues t =
    case t ^. _Pure of
    T.RExtend (RowExtend tag val rest) ->
        (:)
        <$> (suggestVal val <&> Pure . V.BApp . V.App (Pure (V.BLeaf (V.LInject tag))))
        <*> suggestVariantValues rest
    _ -> pure []

-- Suggest an expression to fit a type.
-- Used in suggested sub-expressions, so does not suggest to-noms.
suggestVal :: Monad m => Pure # T.Type -> T m (Pure # V.Term)
suggestVal t =
    case t ^. _Pure of
    T.TRecord r -> suggestRec r
    T.TFun f ->
        case f ^? funcIn . _Pure . T._TVariant of
        Just r -> suggestCase r (f ^. funcOut)
        Nothing ->
            genLamVar <&>
            \v -> _Pure . V._BLam # V.TypedLam v (_Pure . _HCompose # Pruned) (_Pure # V.BLeaf V.LHole)
    _ -> _Pure # V.BLeaf V.LHole & pure

suggestCase :: Monad m => Pure # T.Row -> Pure # T.Type -> T m (Pure # V.Term)
suggestCase r t =
    case r ^. _Pure of
    T.RVar{} -> _Pure # V.BLeaf V.LHole & pure
    T.REmpty -> _Pure # V.BLeaf V.LAbsurd & pure
    T.RExtend (RowExtend tag fieldType rest) ->
        RowExtend tag
        <$> suggestVal (_Pure . T._TFun # FuncType fieldType t)
        <*> suggestCase rest t
        <&> (_Pure . V._BCase #)

suggestRec :: Monad m => Pure # T.Row -> T m (Pure # V.Term)
suggestRec t =
    case t ^. _Pure of
    T.RVar{} -> _Pure # V.BLeaf V.LHole & pure
    T.REmpty -> _Pure # V.BLeaf V.LRecEmpty & pure
    T.RExtend (RowExtend tag fieldType rest) ->
        RowExtend tag
        <$> suggestVal fieldType
        <*> suggestRec rest
        <&> (_Pure . V._BRecExtend #)

genLamVar :: Monad m => T m V.Var
genLamVar = Transaction.newKey <&> V.Var . Identifier . BS.strictify . UUID.toByteString

makeTagRes ::
    Monad m =>
    T.Tag ->
    Text ->
    (T.Tag -> a) ->
    ConvertM m [Result a]
makeTagRes newTag prefix f =
    getListing Anchors.tags >>= traverse mk
    <&> (newTagRes :)
    where
        mk tag =
            ExprIRef.readTagData tag & transaction <&> tagTexts Nothing
            <&> Lens.mapped . Lens.mapped . traverse %~ (prefix <>)
            <&> simpleResult (f tag)
        newTagRes =
            Result
            { _rDeps = mempty
            , _rAllowEmptyQuery = False
            , _rExpr = f newTag
            , _rWithTypeAnnotations = False
            , _rTexts = QueryNewTag newTag
            }

taggedVar :: (Monad m, ToUUID a) => a -> T.Tag -> Transaction m (TagSuffixes -> QueryLangInfo -> [Text])
taggedVar v t = ExprIRef.readTagData t <&> tagTexts (Just (TaggedVarId (toUUID v) t))

symTexts :: (Monad m, ToUUID a) => Text -> a -> T m (TagSuffixes -> QueryLangInfo -> [Text])
symTexts prefix tid =
    getP (Anchors.assocTag tid) >>= taggedVar tid
    <&> Lens.mapped . Lens.mapped . traverse %~ (prefix <>)

makeNoms ::
    Monad m =>
    [T.NominalId] ->
    Text ->
    (Pure # T.Type -> NominalId -> T m [Result a]) ->
    ConvertM m [Result a]
makeNoms avoid prefix f =
    getListing Anchors.tids >>= traverse (transaction . mk) <&> (^.. traverse . Lens._Just . traverse)
    where
        mk tid
            | tid `elem` avoid = pure Nothing
            | otherwise =
                Load.nominal tid <&> (^? Lens._Right) >>= Lens._Just %%~
                \d ->
                do
                    texts <- symTexts prefix tid
                    f (d ^. _Pure . nScheme . sTyp) tid
                        <&> traverse %~ (rDeps . depsNominals . Lens.at tid ?~ d) . (rTexts . _QueryTexts <>~ texts)

makeGlobals :: Monad m => (V.Var -> Pure # T.Type -> T m (Maybe a)) -> ConvertM m [Result a]
makeGlobals f =
    do
        deps <-
            do
                recRef <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
                Lens.view (ConvertM.scFrozenDeps . pVal . depsGlobalTypes) <&> maybe id addRecRef recRef
        -- For globals currently in deps, use their frozen type,
        -- to avoid using parameters inconsistent with frozen type.
        let filt d = Lens.nullOf (Lens.ix (ExprIRef.globalId d)) deps
        sequenceA
            [ deps ^@.. Lens.itraversed & traverse existingGlobal & transaction
            , getListing Anchors.globals <&> filter filt >>= transaction . traverse newGlobal
            ] <&> mconcat
            <&> (^.. traverse . Lens._Just)
            <&> Lens.mapped %~ rWithTypeAnnotations .~ True
    where
        addRecRef r = Lens.at (ExprIRef.globalId  (r ^. ConvertM.rrDefI)) ?~ r ^. ConvertM.rrDefType
        existingGlobal (x, s) = f x (s ^. _Pure . sTyp) >>= Lens._Just (\r -> symTexts "" x <&> simpleResult r)
        newGlobal x =
            do
                s <- Transaction.readIRef x <&> (^. Def.defType)
                f v (s ^. _Pure . sTyp)
                    >>= Lens._Just (\r -> symTexts "" x <&> (simpleResult r <&> rDeps . depsGlobalTypes . Lens.at v ?~ s))
            where
                v = ExprIRef.globalId x

getListing ::
    Monad m =>
    (Anchors.CodeAnchors m -> MkProperty' (T m) (Set a)) ->
    ConvertM m [a]
getListing anchor =
    Lens.view Anchors.codeAnchors
    >>= transaction . getP . anchor
    <&> (^.. Lens.folded)

makeForType :: Monad m => Pure # T.Type -> T m [Result (Pure # V.Term)]
makeForType t =
    suggestTopLevelVal t
    >>= traverse (\(deps, v) -> mkTexts v <&> (simpleResult v <&> rDeps .~ deps))
    where
        mkTexts v =
            case v ^. _Pure of
            V.BRecExtend{} -> pure (const recTexts)
            V.BLeaf V.LRecEmpty -> pure (const recTexts)
            V.BCase{} -> pure (const caseTexts)
            V.BLeaf V.LAbsurd -> pure (const caseTexts)
            V.BLeaf (V.LFromNom nomId) -> symTexts "." nomId
            V.BLeaf (V.LGetField tag) -> symTexts "." tag
            V.BLeaf (V.LInject tag) -> symTexts "'" tag
            V.BApp (V.App (Pure (V.BLeaf (V.LInject tag))) _) -> symTexts "'" tag
            _ -> mempty

tagTexts :: Maybe TaggedVarId -> Tag.Tag -> TagSuffixes -> QueryLangInfo -> [Text]
tagTexts v t suffixes l
    | null names = l ^.. qNameTexts . Texts.unnamed
    | otherwise = names
    where
        names =
            t ^..
            ( Tag.tagTexts . Lens.ix (l ^. qLangId) . (Tag.name <> Tag.abbreviation . Lens._Just)
                <> Tag.tagSymbol . (Tag._UniversalSymbol <> Tag._DirectionalSymbol . dir)
            ) <&> addSuffix
        addSuffix =
            case v of
            Nothing -> id
            Just tv -> suffixes ^. Lens.at tv & maybe id (flip mappend . Text.pack . show)
        dir =
            case l ^. qLangDir of
            LeftToRight -> Tag.opLeftToRight
            RightToLeft -> Tag.opRightToLeft

recTexts :: QueryLangInfo -> [Text]
recTexts = (^.. qCodeTexts . Texts.recordOpener) <> (^.. qCodeTexts . Texts.recordCloser)

caseTexts :: QueryLangInfo -> [Text]
caseTexts = (<&> ("." <>)) . (^.. qCodeTexts . Texts.case_)

lamTexts :: Pure # T.Type -> QueryLangInfo -> [Text]
lamTexts typ =
    (^.. qUITexts . Texts.lambda) <> const ("\\" : pipe)
    where
        pipe = ["|" | Lens.has (_Pure . T._TFun . funcIn . _Pure . T._TRecord . _Pure . T._REmpty) typ]

ifTexts :: QueryLangInfo -> [Text]
ifTexts = (^.. qCodeTexts . Texts.if_)

makeOption ::
    forall a m.
    Monad m =>
    Input.Payload m # V.Term ->
    Result [(a, Ann (Write m) # V.Term)] ->
    ConvertM m (Result (a, Option HoleOpt InternalName (OnceT (T m)) (T m)))
makeOption dstPl res =
    do
        curCtx <- Lens.view ConvertM.scInferContext
        let (scope, ctx0) =
                Infer.runPureInfer (dstPl ^. Input.inferScope) curCtx
                (Infer.loadDeps (res ^. rDeps) ?? dstPl ^. Input.inferScope)
                ^?! Lens._Right
        let errInfo = res ^.. rExpr . traverse . _2 <&> (hPlain #) . unwrap (const (^. hVal)) & show
        let ((iExpr, ctx1, i), (inferred, _)) =
                ((res ^. rExpr <&> _2 %~ Infer.runPureInfer scope ctx0 . infer)
                    ^@.. traverse . Lens.filteredBy _1 <. _2 . Lens._Right <&>
                    \(idx, (e, ctx)) ->
                    Infer.runPureInfer () ctx (inferUVarsApplyBindings e) <&> (,) (e, ctx, idx)
                ) ^? traverse . Lens._Right
                & fromMaybe (error ("inference of all options failed: " <> errInfo))
        let unifyResult =
                Infer.runPureInfer () ctx1
                (unify (dstPl ^. Input.inferredTypeUVar) (inferred ^. hAnn . _2 . inferResult . _2)
                    *> inferUVarsApplyBindings iExpr)
        let inferred1 =
                case unifyResult of
                Left _err -> inferred
                Right (newInferred, _) -> newInferred
        (written, changes) <-
            inferred1 & hflipped %~ hmap (const markToPrune)
            & hAnn . _2 . _1 .~ Const False
            & writeRecursively
            <&> prune
            <&> ExprIRef.toHRefs (dstPl ^. Input.stored . ExprIRef.setIref)
            <&> hflipped %~ hmap (const mkPayload)
            -- The forked transaction serves two purposes:
            -- No actual data is written to the db for generating an option
            -- The results cache is not invalidated due to writing to the database
            & Transaction.fork & transaction
        let resExpr =
                case written ^? hVal . V._BApp of
                Just (V.App (Ann _ f) x) ->
                    -- For applying arguments to fragmented funcs,
                    -- prune replaces the func in the expr with a hole or fragmented hole.
                    -- We extract the argument of it.
                    case f of
                    V.BLeaf V.LHole -> x
                    V.BApp (V.App (Ann _ (V.BLeaf V.LHole)) (Ann _ (V.BLeaf V.LHole))) -> x
                    _ -> written
                _ -> written
                & Input.preprocess (dstPl ^. Input.inferScope) (dstPl ^. Input.localsInScope)

        recordVars <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecordParams)
        let recordVarTags =
                do
                    v <- resExpr ^? hVal . V._BLeaf . V._LVar
                    recordVars ^. Lens.at v
                        <&> map (nameWithContext Nothing v) . nubOrd . map head . (^.. Lens.folded)

        s <-
            convertBinder resExpr <&> annValue %~
                case recordVarTags of
                Just t -> const (HoleVarsRecord t)
                Nothing -> HoleBinder
            & local (ConvertM.scInferContext .~ ctx1)
            & -- Updated deps are required to sugar labeled apply
                Lens.locally (ConvertM.scFrozenDeps . pVal) (<> res ^. rDeps)
            <&> markNodeAnnotations @_ @(HoleOpt (ShowAnnotation, EvalPrep) InternalName (OnceT (T m)) (T m))
            <&> hflipped %~ hmap (const (Lens._Wrapped %~
                    \(showAnn, x) -> convertPayload x & plAnnotation %~ (,) showAnn
                ))
            -- We explicitly do want annotations of variables such as global defs to appear
            <&> Lens.filteredBy (hVal . _HoleBinder . bBody . _BinderTerm . _BodyLeaf . _LeafGetVar) .
                annotation . plAnnotation . _1 .~ alwaysShowAnnotations
            <&> hVal . _HoleBinder . bBody . _BinderTerm . _BodySimpleApply . appFunc .
                Lens.filteredBy (hVal . _BodyLeaf . _LeafGetVar) .
                annotation . plAnnotation . _1 .~ alwaysShowAnnotations
            >>= hAnnotations mkAnn
        depsProp <- Lens.view ConvertM.scFrozenDeps
        pick <- ConvertM.typeProtectedSetToVal ?? dstPl ^. Input.stored <&> Lens.mapped %~ void
        res & rExpr .~
            ( i
            , Option
                { _optionPick =
                    do
                        pureModify depsProp (<> res ^. rDeps)
                        Transaction.merge changes
                        pick (written ^. hAnn . Input.stored . ExprIRef.iref)
                , _optionExpr = s
                , _optionTypeMatch = Lens.has Lens._Right unifyResult
                , _optionMNewTag = res ^? rTexts . _QueryNewTag
                }
            ) & pure
    where
        mkAnn x
            | res ^. rWithTypeAnnotations = makeAnnotation Annotations.Evaluation x <&> _AnnotationVal .~ ()
            | otherwise = pure AnnotationNone
        mkPayload (stored :*: inferRes) =
            Input.Payload
            { Input._entityId = stored ^. ExprIRef.iref . _F & IRef.uuid & EntityId
            , Input._stored = stored
            , Input._inferScope = V.emptyScope
            , Input._varRefsOfLambda = []
            , Input._localsInScope = []
            , Input._inferRes = inferRes
            }
        markToPrune (w :*: a) = w :*: Const (Lens.has ExprIRef._ExistingRef w) :*: a

-- Replace emplaced fragment expression with hole.
-- This avoids sugaring it (it may be large) but also conversion to
-- fragment value is based on it (in case of chained postfix funcs).
prune :: Ann (a :*: Const Bool :*: b) # V.Term -> Ann (a :*: b) # V.Term
prune (Ann (a :*: Const p :*: b) x) =
    (if p
        then V.BLeaf V.LHole
        else
            hmap
            ( \case
                HWitness V.W_Term_Term -> prune
                HWitness V.W_Term_HCompose_Prune_Type -> hflipped %~ hmap (const (\(la :*: _ :*: lb) -> la :*: lb))
            ) x
    ) & Ann (a :*: b)

makeLocals ::
    Monad m =>
    (Pure # T.Type -> Pure # V.Term -> T m a) ->
    V.Scope # UVar -> ConvertM m [Result a]
makeLocals f scope =
    do
        ctx <- Lens.view ConvertM.scInferContext
        fieldParams <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siRecordParams)
            <&> (^@.. Lens.itraversed <. Lens.folded)
            >>= transaction . traverse (mkGetField ctx)
        recRef <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
            <&> (^? Lens._Just . ConvertM.rrDefI . Lens.to ExprIRef.globalId)
        deps <- Lens.view (ConvertM.scFrozenDeps . pVal . depsGlobalTypes)
        Infer.runPureInfer scope ctx
            (scope ^@.. V.scopeVarTypes . Lens.itraversed
                & filter (\(k, _) ->
                    -- Avoid repeating globals
                    Lens.nullOf (Lens.ix k) deps &&
                    Just k /= recRef)
                & (traverse . _2) (instantiate . (^. _HFlip) >=> applyBindings)
            ) ^?! Lens._Right . _1
            -- Avoid unit variables (like those hidden in pipe syntax)
            & filter (Lens.hasn't (_2 . _Pure . T._TRecord . _Pure . T._REmpty))
            & traverse mkVar
            <&> (<> fieldParams)
    where
        mkVar (var, typ) =
            simpleResult
            <$> transaction (f typ (_Pure . V._BLeaf . V._LVar # var))
            <*> localName typ var
        mkGetField ctx (var, tags) =
            simpleResult
            <$> f typ (foldr V.BAppP (V.BLeafP (V.LVar var)) (reverse tags <&> V.BLeafP . V.LGetField) ^. hPlain)
            <*> taggedVar var (last tags)
            where
                typ =
                    foldl (\ty t -> ty ^?! _Pure . T._TRecord . T.flatRow . freExtends . Lens.ix t)
                    (Infer.runPureInfer scope ctx
                    (instantiate (scope ^?! V.scopeVarTypes . Lens.ix var . _HFlip) >>= applyBindings)
                        ^?! Lens._Right . _1)
                    tags

mkEvalPrep :: ConvertPayload m -> EvalPrep
mkEvalPrep pl =
    EvalPrep
    { _eType = pl ^. pUnsugared . hAnn . Input.inferredType
    , _eEvalId = pl ^. pEntityId
    }

convertPayload :: ConvertPayload m -> Payload EvalPrep (T m)
convertPayload pl =
    Payload
    { _plAnnotation = mkEvalPrep pl
    , _plActions = pl ^. pActions
    , _plEntityId = pl ^. pEntityId
    , _plParenInfo = ParenInfo 0 False
    , _plHiddenEntityIds = []
    }

 -- Duplicate name-gen behaviour for locals
localName :: MonadTransaction n m => Pure # T.Type -> V.Var -> m (TagSuffixes -> QueryLangInfo -> [Text])
localName typ var =
    do
        tag <- Anchors.assocTag var & getP & transaction
        if tag == Anchors.anonTag
            then mkVarInfo typ <&> autoName
            else pure tag
    >>= transaction . taggedVar var
