-- | Convert applied holes to Fragments

module Lamdu.Sugar.Convert.Fragment
    ( convertAppliedHole
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Char as Char
import           Data.Property (getP)
import qualified Data.Map as Map
import           Data.Typeable (Typeable)
import           Generic.Data (gconIndex)
import           Hyper
import           Hyper.Class.Context
import           Hyper.Recurse
import           Hyper.Type.AST.FuncType (FuncType(..), funcIn)
import           Hyper.Type.AST.Nominal (nId, nScheme)
import           Hyper.Type.AST.Row (FlatRowExtends, freExtends)
import           Hyper.Type.AST.Scheme (sTyp)
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify (UVar, applyBindings, unify)
import           Lamdu.Calc.Definition (depsNominals)
import qualified Lamdu.Calc.Infer as Infer
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Data.Tag (tagOrder)
import           Lamdu.Expr.IRef (iref)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Sugar.Config as Config
import qualified Lamdu.Sugar.Convert.Expression.Actions as Actions
import           Lamdu.Sugar.Convert.Fragment.Heal (healMismatch)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Option
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Hyper (Write(..))
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction.Transaction

checkTypeMatch :: Monad m => UVar # T.Type -> UVar # T.Type -> ConvertM m Bool
checkTypeMatch x y =
    Lens.view ConvertM.scInferContext <&> check <&> Lens.has Lens._Right
    where
        check ctx = unify x y >> applyBindings x & Infer.runPureInfer () ctx

convertAppliedHole ::
    (Monad m, Typeable m, Monoid a) =>
    V.App V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ExpressionU EvalPrep m a ->
    MaybeT (ConvertM m) (ExpressionU EvalPrep m a)
convertAppliedHole app@(V.App funcI argI) exprPl argS =
    do
        Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.fragment) >>= guard
        guard (Lens.has ExprLens.valHole funcI)
        do
            isTypeMatch <-
                checkTypeMatch (argI ^. hAnn . Input.inferredTypeUVar)
                (exprPl ^. Input.inferredTypeUVar)
            postProcess <- ConvertM.postProcessAssert
            healMis <- healMismatch
            typeMismatch <-
                if isTypeMatch
                then pure Nothing
                else
                    Actions.makeTypeAnnotation
                        (EntityId.ofFragmentArg (argPl ^. Input.entityId))
                        (argPl ^. Input.inferredType) <&> Just
            opts <-
                do
                    forType <- transformArg exprPl argI & transaction
                    ResultGroups
                        { gDefs =
                            makeGlobals (makeGetDef topRef argI)
                            <&> traverse . rTexts . Lens.mapped . traverse %~ toOpName
                        , gLocals =
                            makeLocals (makeLocal topRef argI) (exprPl ^. Input.inferScope)
                            <&> traverse %~ sequenceA <&> (^.. traverse . Lens._Just)
                        , gInjects = makeTagRes "'" (Pure . V.BLeaf . V.LInject) <&> funcOpt
                        , gToNoms = makeNoms [] "" (makeToNom argI <&> Lens.mapped . Lens.mapped %~ (:[]) . (Result mempty ?? mempty))
                        , gFromNoms =
                            makeNoms (forType ^.. Lens._Just . _1 . Lens._Just)
                            "." (makeFromNom exprPl argI <&> Lens.mapped . Lens.mapped %~ (:[]))
                        , gForType = forType ^.. Lens._Just . _2 & pure
                        , gGetFields = makeTagRes "." (Pure . V.BLeaf . V.LGetField) <&> funcOpt
                        , gSyntax = makeResultsSyntax argI & transaction
                        , gWrapInRecs =
                            makeTagRes "{"
                            (\t ->
                                [ (TypeMatches
                                    , V.RowExtend t
                                        (argI & hflipped %~ hmap (const (ExistingRef . (^. Input.stored . ExprIRef.iref))))
                                        (Ann WriteNew (V.BLeaf V.LRecEmpty))
                                        & V.BRecExtend & Ann WriteNew
                                    )
                                ]
                            )
                        } <&> (>>= traverse (makeOption exprPl))
                        <&> Lens.mapped . traverse . rExpr . _2 . optionExpr %~ toFragOpt
                        & traverse ConvertM.convertOnce
                <&> filterResults (\outerMatch innerMatch -> (innerMatch, outerMatch))
                & ConvertM.convertOnce
            BodyFragment Fragment
                { _fExpr =
                    argS
                    & annotation . pActions . detach .~ FragmentedAlready storedEntityId
                    & annotation . pActions . delete .~
                        SetToHole
                        (DataOps.setToHole stored <* postProcess <&> EntityId.ofValI)
                , _fHeal =
                    ( if isTypeMatch
                        then DataOps.replace stored argIRef <* postProcess
                        else argIRef <$ healMis (stored ^. iref)
                    )
                    <&> EntityId.ofValI
                , _fTypeMismatch = typeMismatch
                , _fOptions = opts
                } & pure
            >>= Actions.addActions app exprPl
            & lift
        <&> annotation . pActions . detach .~ FragmentedAlready storedEntityId
    where
        topRef = exprPl ^. Input.stored . ExprIRef.iref
        funcOpt = traverse . rExpr %~ makeFuncOpts
        argPl = argS ^. annotation . pInput
        argIRef = argI ^. hAnn . Input.stored . iref
        stored = exprPl ^. Input.stored
        storedEntityId = stored ^. iref & EntityId.ofValI
        makeFuncOpts opt = emplaceArg argI <&> _2 %~ Ann (ExistingRef topRef) . V.BApp . App (writeNew opt)

makeResultsSyntax ::
    Monad m =>
    Ann (Input.Payload m a) # V.Term -> T m [Result [(TypeMatch, Ann (Write m) # V.Term)]]
makeResultsSyntax arg =
    genLamVar <&>
    \v ->
    [ Result mempty
        (emplaceArg arg <&> _2 %~ Ann WriteNew . V.BLam . V.TypedLam v (Ann WriteNew (HCompose Pruned)))
        lamTexts
    ]

writeNew :: Pure # V.Term -> Ann (Write m) # V.Term
writeNew = wrap (const (Ann WriteNew))

transformArg ::
    Monad m =>
    Input.Payload m a # V.Term -> Ann (Input.Payload m a) # V.Term ->
    T m (Maybe (Maybe T.NominalId, Result [(TypeMatch, Ann (Write m) # V.Term)]))
transformArg topPl arg =
    case arg ^? hAnn . Input.inferredType . _Pure . T._TInst . nId of
    Just tid ->
        Load.nominal tid
        >>=
        \case
        Nothing -> replaceFunc
        Just s ->
            s ^? _Pure . nScheme . sTyp . _Pure . T._TVariant
            & Lens._Just %%~
            \r ->
            Result (mempty & depsNominals . Lens.at tid ?~ s)
            <$> ( suggestCase r topType <&>
                    \c ->
                    emplaceArg arg
                    <&> _2 %~
                        Ann WriteNew . V.BApp . App (writeNew c) .
                        Ann WriteNew . V.BApp . App (Ann WriteNew (V.BLeaf (V.LFromNom tid)))
                )
            <*> (symTexts "." tid <&> (<> caseTexts))
        <&> Lens.mapped %~ (,) (Just tid)
    Nothing -> replaceFunc <&> Lens.mapped %~ (,) Nothing
    where
        topType = topPl ^. Input.inferredType
        replaceFunc =
            makeForType (_Pure . T._TFun # FuncType (arg ^. hAnn . Input.inferredType) topType)
            <&> Lens._Just . rExpr %~
            \f ->
            emplaceArg arg
            <&> _2 %~ Ann (ExistingRef (topPl ^. Input.stored . ExprIRef.iref)) . V.BApp . App (writeNew f)

makeLocal ::
    Monad m =>
    ExprIRef.ValI m -> Ann (Input.Payload m a) # V.Term ->
    Pure # T.Type -> Pure # V.Term -> T m (Maybe [(TypeMatch, Ann (Write m) # V.Term)])
makeLocal top arg typ val =
    case typ ^. _Pure of
    T.TFun f -> emplaceExtendArg (f ^. funcIn) arg <&> traverse . _2 %~ r <&> Just
    T.TVar{} -> emplaceArg arg <&> _2 %~ r & Just & pure
    _ -> pure Nothing
    where
        r = Ann (ExistingRef top) . V.BApp . App (writeNew val)

toFragOpt :: Annotated a # Binder v name i o -> Annotated a # FragOpt v name i o
toFragOpt o =
    case o ^. hVal of
    BinderTerm (BodyPostfixApply (PostfixApply a f)) ->
        reverse (f : match a) & FragPostfix & Ann (Const (f ^. annotation))
        where
            match (Ann _ (BodyPostfixApply (PostfixApply a' f'))) = f' : match a'
            match _ = []
    BinderTerm (BodySimpleApply a) ->
        a ^. appFunc & annValue %~
        \case
        BodyLeaf (LeafInject x) -> FragInject x
        BodyLeaf (LeafGetVar x) -> FragGetVar x
        _ -> error "unexpected result in fragment apply"
    BinderTerm (BodyLabeledApply x) ->
        FragOp FragOperator
        { _oFunc = x ^. aFunc
        , _oRightArg = x ^?! Lens.failing (aMOpArgs . Lens._Just . oaRhs) (aAnnotatedArgs . Lens.ix 1 . aaExpr)
        } & Ann (Const (o ^. annotation))
    BinderTerm (BodyToNom n) -> n ^. nTId & FragToNom & Ann (Const (o ^. annotation))
    BinderTerm (BodyIfElse i) -> i ^. iThen & FragIf & Ann (Const (o ^. annotation))
    BinderTerm BodyLam{} -> o & annValue .~ FragLam
    BinderTerm (BodyRecord r) ->
        r ^?! cItems . traverse . ciTag & FragWrapInRec & Ann (Const (o ^. annotation))
    BinderTerm x -> error ("unexpected result in fragment result: " <> show (gconIndex x))
    BinderLet{} -> error "let in fragment result"

emplaceTag :: Monad m => FlatRowExtends T.Tag a b # h -> V.Var -> T m T.Tag
emplaceTag r v =
    Anchors.assocPresentationMode v & getP >>=
    \case
    Operator x _ -> pure x
    Verbose ->
        r ^.. freExtends . Lens.itraversed . Lens.asIndex
        & traverse (\x -> ExprIRef.readTagData x <&> \t -> (t ^. tagOrder, x))
        <&> snd . minimum

emplaceArg :: Ann (Input.Payload m a) # V.Term -> [(TypeMatch, Ann (Write m) # V.Term)]
emplaceArg arg =
    [ (TypeMatches , a)
    , (TypeMismatch , App (Ann WriteNew (V.BLeaf V.LHole)) a & V.BApp & Ann WriteNew)
    ]
    where
        a = arg & hflipped %~ hmap (const (ExistingRef . (^. Input.stored . ExprIRef.iref)))

emplaceExtendArg ::
    Monad m =>
    Pure # T.Type -> Ann (Input.Payload m a) # V.Term ->
    T m [(TypeMatch, Ann (Write m) # V.Term)]
emplaceExtendArg typ arg =
    suggestVal typ <&>
    \s ->
    let h =
            case s ^. _Pure of
            V.BLeaf{} -> [] -- Redundant with plain emplace
            _ ->
                annContexts (writeNew s)
                ^.. ExprLens.valLeafs . V._LHole . Lens.asIndex
                <&> \(HFunc f :*: _) -> (TypeMatches, f a ^. Lens._Wrapped)
    in
    (TypeMatches, a) :
    (if length h == 1 then h else []) <>
    [(TypeMismatch, App (Ann WriteNew (V.BLeaf V.LHole)) a & V.BApp & Ann WriteNew)]
    where
        a = arg & hflipped %~ hmap (const (ExistingRef . (^. Input.stored . ExprIRef.iref)))

makeFromNom ::
    Monad m =>
    Input.Payload m a # V.Term -> Ann (Input.Payload m a) # V.Term ->
    Pure # T.Type -> NominalId ->
    T m (Result [(TypeMatch, Ann (Write m) # V.Term)])
makeFromNom topPl arg t tid =
    case t ^. _Pure of
    T.TVariant r ->
        suggestCase r (topPl ^. Input.inferredType) <&>
        \c ->
        Result mempty
        (fromNom WriteNew <&> _2 %~ Ann WriteNew . V.BApp . App (writeNew c))
        (if tid == Builtins.boolTid then ifTexts else caseTexts)
    _ -> Result mempty (fromNom (ExistingRef (topPl ^. Input.stored . ExprIRef.iref))) mempty & pure
    where
        fromNom w =
            emplaceArg arg
            <&> _2 %~ asHyper . Ann w . V.BApp . V.App (V._BLeaf . V._LFromNom # tid & Ann WriteNew)

makeToNom ::
    Monad m =>
    Ann (Input.Payload m a) # V.Term -> Pure # T.Type -> NominalId ->
    T m [(TypeMatch, Ann (Write m) # V.Term)]
makeToNom arg t tid =
    emplaceExtendArg t arg <&> Lens.mapped . _2 %~ Ann WriteNew . V.BToNom . V.ToNom tid

makeGetDef ::
    Monad m =>
    ExprIRef.ValI m -> Ann (Input.Payload m a) # V.Term -> V.Var -> Pure # T.Type ->
    T m (Maybe [(TypeMatch, Ann (Write m) # V.Term)])
makeGetDef top arg v t =
    t ^? _Pure . T._TFun & Lens._Just %%~
    \f ->
    case f ^. funcIn of
    Pure (T.TRecord r) ->
        do
            e <- emplaceTag flat v
            let ext rest x =
                    V.RowExtend
                    { V._eKey = e
                    , V._eVal = x
                    , V._eRest = rest
                    }
            rest <-
                T.flatRow # (flat & freExtends %~ Map.filterWithKey (\k _ -> k /= e)) & suggestRec
                <&> writeNew
            emplaceExtendArg (flat ^?! freExtends . Lens.ix e) arg
                <&> traverse . _2 %~
                    Ann WriteNew . V.BApp . App (Ann WriteNew (V.BLeaf (V.LVar v))) .
                    Ann WriteNew . V.BRecExtend . ext rest
        where
            flat = r ^. T.flatRow
    typ ->
        emplaceExtendArg typ arg
        <&> traverse . _2 %~ Ann (ExistingRef top) . V.BApp . App (Ann WriteNew (V.BLeaf (V.LVar v)))

toOpName :: Text -> Text
toOpName t = "." <> t <$ t ^? Lens._head . Lens.filtered Char.isAlpha & fromMaybe t
