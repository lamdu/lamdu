module Lamdu.Sugar.Convert.Hole
    ( convert, results
    ) where

import qualified Control.Lens.Extended as Lens
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Typeable (Typeable)
import           Hyper
import           Hyper.Recurse (wrap)
import           Hyper.Syntax (FuncType(..))
import           Hyper.Syntax.Row (freExtends, freRest)
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify (UVar)
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Calc.Definition (depsNominals)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.I18N.Code as Texts
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Option
import           Lamdu.Sugar.Convert.Suggest
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction.Transaction

results ::
    Monad m =>
    ConvertM.PositionInfo ->
    Pure # T.Type -> V.Scope # UVar ->
    ConvertM m (ResultGroups (ConvertM m [Result (Pure # V.Term)]))
results posInfo typ scope =
    do
        forType <- makeForType typ & transaction
        let filtForType = filter (\x -> x ^. rExpr `notElem` (forType <&> (^. rExpr)))
        newTag <- DataOps.genNewTag & transaction
        pure ResultGroups
            { gSyntax = makeResultsSyntax typ posInfo & transaction <&> filtForType
            , gDefs = makeGlobals makeGetDef
            , gLocals = makeLocals (const pure) scope
            , gInjects =
                makeTagRes newTag "'" ((^. hPlain) . (`V.BAppP` V.BLeafP V.LRecEmpty) . V.BLeafP . V.LInject)
                <&> filtForType
            , gToNoms = makeNoms [] "" makeToNoms
            , gFromNoms =
                makeNoms [] "." (\_ x -> pure [simpleResult (_Pure . V._BLeaf . V._LFromNom # x) mempty])
                <&> filtForType
            , gForType = pure forType
            , gGetFields = makeTagRes newTag "." (Pure . V.BLeaf . V.LGetField)
            , gWrapInRecs = pure [] -- Only used in fragments
            }

convert ::
    (Monad m, Typeable m) =>
    ConvertM.PositionInfo ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convert posInfo holePl =
    do
        tagsProp <- Lens.view Anchors.codeAnchors <&> Anchors.tags
        results posInfo (holePl ^. Input.inferredType) (holePl ^. Input.inferScope)
            <&> Lens.mapped %~ (>>= traverse (makeOption holePl . fmap (\x -> [((), wrap (const (Ann ExprIRef.WriteNew)) x)])))
            >>= traverse ConvertM.convertOnce
            <&> filterResults tagsProp const
    -- The call to convertOnce makes the result expressions consistent.
    -- If we remove all calls to convertOnce (replacing with "fmap pure"),
    -- they would flicker when editing the search term.
    & ConvertM.convertOnce
    <&> BodyLeaf . LeafHole . (`Hole` mempty)
    >>= addActions (Ann holePl (V.BLeaf V.LHole))
    <&> annotation . pActions . delete .~ CannotDelete
    <&> annotation . pActions . mApply .~ Nothing

makeToNoms :: Monad m => Pure # T.Type -> NominalId -> T m [Result (Pure # V.Term)]
makeToNoms t tid =
    case t ^. _Pure of
    -- Many nominals (like Maybe, List) wrap a sum type, suggest their various injections
    T.TVariant r | Lens.has (freRest . _Pure . T._REmpty) f ->
        f ^@.. freExtends . Lens.itraversed & traverse mkVariant
        where
            f = r ^. T.flatRow
            mkVariant (tag, typ) =
                simpleResult
                <$> (suggestVal typ <&> (_Pure . V._BApp #) . V.App (_Pure . V._BLeaf . V._LInject # tag))
                <*> (taggedVar tid tag <&> Lens.mapped . Lens.mapped %~ (>>= injTexts))
    _ -> suggestVal t <&> (:[]) . (simpleResult ?? mempty)
    <&> traverse . rExpr %~ Pure . V.BToNom . V.ToNom tid
    where
        -- "t" will be prefix for "Bool 'true" too,
        -- so that one doesn't have to type the "'" prefix
        injTexts x = [x, "'" <> x]

makeResultsSyntax :: Monad m => Pure # T.Type -> ConvertM.PositionInfo -> T m [Result (Pure # V.Term)]
makeResultsSyntax typ posInfo =
    sequenceA
    [ r recTexts (V.BLeafP V.LRecEmpty) & pure
    , genLamVar <&> \v -> r (lamTexts typ) (V.BLamP v Pruned (V.BLeafP V.LHole))
    , r caseTexts (V.BLeafP V.LAbsurd) & pure
    ] <>
    sequenceA
    [ genLamVar <&>
        \v ->
        r (^.. qCodeTexts . Texts.let_)
        (V.BLamP v Pruned (V.BLeafP V.LHole) `V.BAppP` V.BLeafP V.LHole)
    | posInfo == ConvertM.BinderPos
    ] <>
    do
        -- Suggest if-else only if bool is in the stdlib (otherwise tests fail)
        deps <-
            Load.nominal Builtins.boolTid
            <&> \(Right x) -> mempty & depsNominals . Lens.at Builtins.boolTid ?~ x
        if Lens.has (depsNominals . Lens.ix Builtins.boolTid) deps then
            do
                t <- genLamVar
                f <- genLamVar
                pure [Result
                    { _rTexts = QueryTexts (const ifTexts)
                    , _rAllowEmptyQuery = False
                    , _rExpr =
                        ( V.BLeafP V.LAbsurd
                        & V.BCaseP Builtins.falseTag (V.BLamP f Pruned (V.BLeafP V.LHole))
                        & V.BCaseP Builtins.trueTag (V.BLamP t Pruned (V.BLeafP V.LHole))
                        ) `V.BAppP`
                        (V.BLeafP (V.LFromNom Builtins.boolTid) `V.BAppP` V.BLeafP V.LHole)
                        ^. hPlain
                    , _rWithTypeAnnotations = False
                    , _rDeps = deps
                    }]
            else pure []
    where
        r f t = simpleResult (t ^. hPlain) (const f)

makeGetDef :: Monad m => V.Var -> Pure # T.Type -> T m (Maybe (Pure # V.Term))
makeGetDef v t =
    case t of
    Pure (T.TFun (FuncType a@(Pure (T.TRecord r)) _))
        -- Avoid filling in params for open records
        | Lens.nullOf (T.flatRow . freRest . _Pure . T._RVar) r ->
            suggestVal a <&> Pure . V.BApp . V.App base
    _ -> pure base
    <&> Just
    where
        base = _Pure . V._BLeaf . V._LVar # v
