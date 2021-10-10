{-# LANGUAGE TemplateHaskell, TypeApplications, ScopedTypeVariables, GADTs, DerivingVia #-}

module Lamdu.Sugar.Convert.Option
    ( Result(..), rTexts, rExpr, rDeps
    , ResultGroups(..), filterResults
    , Matches, matchResult
    , TypeMatch(..)
    , makeTagRes, makeNoms, makeForType, makeLocals
    , suggestVal, genLamVar, suggestRec, suggestCase
    , getListing, makeGlobals
    , tagTexts, recTexts, caseTexts, ifTexts, symTexts, lamTexts
    , makeOption
    ) where

import qualified Control.Lens as Lens
import           Control.Monad ((>=>))
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.ByteString.Extended as BS
import           Data.List (sortOn)
import           Data.Property (MkProperty', getP, pureModify, pVal)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import           GUI.Momentu.Direction (Layout(..))
import           Hyper
import           Hyper.Recurse
import           Hyper.Infer
import           Hyper.Syntax (FuncType(..), funcIn, funcOut)
import           Hyper.Syntax.Nominal (NominalInst, nId, nScheme)
import           Hyper.Syntax.Row (RowExtend(..), freExtends)
import           Hyper.Syntax.Scheme (sTyp)
import           Hyper.Type.Functor (_F)
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify (UVar, applyBindings, unify)
import           Hyper.Unify.Generalize (instantiate)
import           Lamdu.Calc.Definition (Deps, depsNominals, depsGlobalTypes)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Infer as Infer
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.UniqueId (ToUUID)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import           Lamdu.Sugar.Convert.Binder.Params (mkVarInfo)
import           Lamdu.Sugar.Convert.Expression.Actions (convertPayload)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId (EntityId(..))
import qualified Lamdu.Sugar.Lens as SugarLens
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
    } deriving (Functor, Foldable, Generic, Traversable)
    deriving (Monoid, Semigroup) via (Generically (Matches a))
Lens.makeLenses ''Matches

data Result a = Result
    { _rDeps :: Deps
    , _rExpr :: a
    , _rTexts :: QueryLangInfo Text -> [Text]
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Result

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
    } deriving (Functor, Foldable, Traversable)

filterResults ::
    (Monad m, Ord b) =>
    (TypeMatch -> a -> b) ->
    ResultGroups (OnceT (T m) [Result (a, Option t name i o)]) -> Query Text ->
    OnceT (T m) [Option t name i o]
filterResults order res query
    | "" == query ^. qSearchTerm = groups (gForType <> gLocals <> gSyntax) <&> (^. traverse)
    | "." `Text.isPrefixOf` (query ^. qSearchTerm) =
        groups (gForType <> gSyntax <> gDefs <> gFromNoms <> gGetFields) <&> (^. traverse)
    | "'" `Text.isPrefixOf` (query ^. qSearchTerm) = groups (gForType <> gToNoms <> gInjects) <&> (^. traverse)
    | "{" `Text.isPrefixOf` (query ^. qSearchTerm) =
        groups (gForType <> gSyntax <> gWrapInRecs) <&> (^. traverse)
    | otherwise =
        -- Within certain search-term matching level (exact/prefix/infix),
        -- prefer locals over globals even for type mismatches
        groups (gForType <> gLocals) <> groups (gSyntax <> gDefs <> gToNoms) <&> (^. traverse)
    where
        groups f =
            f res <&> fmap ((^.. traverse . _2) . sortOn s) . foldMap (matchResult query)
        s (i, opt) = order (if opt ^. optionTypeMatch then TypeMatches else TypeMismatch) i

unicodeAlts :: Text -> [Text]
unicodeAlts haystack =
    traverse alts (Text.unpack haystack)
    <&> concat
    <&> Text.pack
    where
        alts x = [x] : extras x
        extras '≥' = [">="]
        extras '≤' = ["<="]
        extras '≠' = ["/=", "!=", "<>"]
        extras '⋲' = ["<{"]
        extras _ = []

matchResult :: Query Text -> Result a -> Matches [a]
matchResult query result
    | s `elem` texts = mempty & mExact .~ e
    | any (Text.isPrefixOf s) texts = mempty & mPrefix .~ e
    | any (Text.isInfixOf s) texts = mempty & mInfix .~ e
    | otherwise = mempty
    where
        e = [result ^. rExpr]
        texts = (result ^. rTexts) (q ^. qLangInfo) <&> Text.toLower >>= unicodeAlts
        q = query <&> Text.toLower
        s = q ^. qSearchTerm

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
    Load.nominal tid <&> (^.. Lens._Just) <&> Lens.mapped %~
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
    Text ->
    (T.Tag -> a) ->
    ConvertM m [Result a]
makeTagRes prefix f =
    getListing Anchors.tags >>= traverse mk
    where
        mk tag =
            ExprIRef.readTagData tag & transaction <&> tagTexts <&> Lens.mapped . traverse %~ (prefix <>)
            <&> Result mempty (f tag)

symTexts :: (Monad m, ToUUID a) => Text -> a -> T m (QueryLangInfo Text -> [Text])
symTexts prefix tid =
    getP (Anchors.assocTag tid) >>= ExprIRef.readTagData <&> tagTexts
    <&> Lens.mapped . traverse %~ (prefix <>)

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
                Load.nominal tid >>= Lens._Just %%~
                \d ->
                do
                    texts <- symTexts prefix tid
                    f (d ^. _Pure . nScheme . sTyp) tid
                        <&> traverse %~ (rDeps . depsNominals . Lens.at tid ?~ d) . (rTexts <>~ texts)

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
    where
        addRecRef r = Lens.at (ExprIRef.globalId  (r ^. ConvertM.rrDefI)) ?~ r ^. ConvertM.rrDefType
        existingGlobal (x, s) = f x (s ^. _Pure . sTyp) >>= Lens._Just (\r -> symTexts "" x <&> Result mempty r)
        newGlobal x =
            do
                s <- Transaction.readIRef x <&> (^. Def.defType)
                f v (s ^. _Pure . sTyp)
                    >>= Lens._Just (\r -> symTexts "" x <&> Result (mempty & depsGlobalTypes . Lens.at v ?~ s) r)
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
    >>= traverse (\(deps, v) -> mkTexts v <&> Result deps v)
    where
        mkTexts v =
            case v ^. _Pure of
            V.BRecExtend{} -> pure recTexts
            V.BLeaf V.LRecEmpty -> pure recTexts
            V.BCase{} -> pure caseTexts
            V.BLeaf V.LAbsurd -> pure caseTexts
            V.BLeaf (V.LFromNom nomId) -> symTexts "." nomId
            V.BLeaf (V.LGetField tag) -> symTexts "." tag
            V.BLeaf (V.LInject tag) -> symTexts "'" tag
            V.BApp (V.App (Pure (V.BLeaf (V.LInject tag))) _) -> symTexts "'" tag
            _ -> pure (const [])

tagTexts :: Tag.Tag -> QueryLangInfo Text -> [Text]
tagTexts t l
    | null names = l ^.. qNameTexts . Texts.unnamed
    | otherwise = names
    where
        names =
            t ^..
            ( Tag.tagTexts . Lens.ix (l ^. qLangId) . (Tag.name <> Tag.abbreviation . Lens._Just)
                <> Tag.tagSymbol . (Tag._UniversalSymbol <> Tag._DirectionalSymbol . dir)
            )
        dir =
            case l ^. qLangDir of
            LeftToRight -> Tag.opLeftToRight
            RightToLeft -> Tag.opRightToLeft

recTexts :: QueryLangInfo Text -> [Text]
recTexts = (^.. qCodeTexts . Texts.recordOpener) <> (^.. qCodeTexts . Texts.recordCloser)

caseTexts :: QueryLangInfo Text -> [Text]
caseTexts = (<&> ("." <>)) . (^.. qCodeTexts . Texts.case_)

lamTexts :: QueryLangInfo Text -> [Text]
lamTexts = (^.. qUITexts . Texts.lambda) <> const ["\\"]

ifTexts :: QueryLangInfo Text -> [Text]
ifTexts = (^.. qCodeTexts . Texts.if_)

makeOption ::
    Monad m =>
    Input.Payload m b # V.Term ->
    Result [(a, Ann (Write m) # V.Term)] ->
    ConvertM m (Result (a, Option Binder InternalName (OnceT (T m)) (T m)))
makeOption dstPl res =
    do
        curCtx <- Lens.view ConvertM.scInferContext
        let (scope, ctx0) =
                Infer.runPureInfer (dstPl ^. Input.inferScope) curCtx
                (Infer.loadDeps (res ^. rDeps) ?? dstPl ^. Input.inferScope)
                ^?! Lens._Right
        let errInfo = res ^.. rExpr . traverse . _2 <&> (hPlain #) . unwrap (const (^. hVal)) & show
        let ((ctx1, i), (inferred, _)) =
                ((res ^. rExpr <&> _2 %~ Infer.runPureInfer scope ctx0 . infer)
                    ^@.. traverse . Lens.filteredBy _1 <. _2 . Lens._Right <&>
                    \(idx, (e, ctx)) ->
                    Infer.runPureInfer () ctx (inferUVarsApplyBindings e) <&> (,) (ctx, idx)
                ) ^? traverse . Lens._Right
                & fromMaybe (error ("inference of all options failed: " <> errInfo))
        (written, changes) <-
            inferred & hflipped %~ hmap (const markToPrune)
            & hAnn . _2 . _1 .~ Const False
            -- The forked transaction serves two purposes:
            -- No actual data is written to the db for generating an option
            -- The results cache is not invalidated due to writing to the database
            & writeRecursively & Transaction.fork & transaction
            <&> _1 %~
                (hflipped %~ hmap (const mkPayload)) .
                ExprIRef.toHRefs (dstPl ^. Input.stored . ExprIRef.setIref) .
                prune
        s <-
            Input.preprocess (dstPl ^. Input.inferScope) (dstPl ^. Input.localsInScope) written
            & convertBinder
            & local (ConvertM.scInferContext .~ ctx1)
            & -- Updated deps are required to sugar labeled apply
                Lens.locally (ConvertM.scFrozenDeps . pVal) (<> res ^. rDeps)
            <&> hflipped %~ hmap (const (Lens._Wrapped %~ convertPayload . (pInput . Input.userData .~ (ParenInfo 0 False, []))))
            <&> SugarLens.hAnnotations @EvalPrep @(Annotation () InternalName) .~ AnnotationNone
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
                , _optionTypeMatch =
                    Lens.has Lens._Right
                    (Infer.runPureInfer () ctx1
                        (unify (dstPl ^. Input.inferredTypeUVar) (inferred ^. hAnn . _2 . inferResult . _2)))
                }
            ) & pure
    where
        mkPayload (stored :*: inferRes) =
            Input.Payload
            { Input._entityId = stored ^. ExprIRef.iref . _F & IRef.uuid & EntityId
            , Input._userData = ()
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
            Lens.view (ConvertM.scScopeInfo . ConvertM.siTagParamInfos)
            <&> (^@.. Lens.itraversed . ConvertM._TagFieldParam . Lens.to ConvertM.tpiFromParameters)
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
            Result mempty
            <$> transaction (f typ (_Pure . V._BLeaf . V._LVar # var))
            <*> localName typ var
        mkGetField ctx (tag, var) =
            Result mempty
            <$> f typ (V.BLeafP (V.LGetField tag) `V.BAppP` V.BLeafP (V.LVar var) ^. hPlain)
            <*> (ExprIRef.readTagData tag <&> tagTexts)
            where
                typ =
                    Infer.runPureInfer scope ctx
                    (instantiate (scope ^?! V.scopeVarTypes . Lens.ix var . _HFlip) >>= applyBindings)
                    ^?! Lens._Right . _1 . _Pure . T._TRecord . T.flatRow . freExtends . Lens.ix tag

 -- Duplicate name-gen behaviour for locals
localName :: MonadTransaction n m => Pure # T.Type -> V.Var -> m (QueryLangInfo Text -> [Text])
localName typ var =
    do
        tag <- Anchors.assocTag var & getP & transaction
        if tag == Anchors.anonTag
            then mkVarInfo typ <&> autoName
            else pure tag
    >>= transaction . ExprIRef.readTagData
    <&> tagTexts
