{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Convert.Nominal
    ( pane
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Reader (ReaderT(..))
import           Control.Monad.Reader.Instances ()
import           Data.Property (Property(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Hyper
import qualified Hyper.Syntax.Nominal as Nominal
import qualified Hyper.Syntax.Scheme as HyperScheme
import           Hyper.Unify.QuantifiedVar (QVar)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (HasCodeAnchors)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import qualified Lamdu.Sugar.Convert.TaggedList as ConvertTaggedList
import qualified Lamdu.Sugar.Convert.Type as ConvertType
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

class NominalParamKind h where
    qvarIdentifier :: Proxy h -> Lens.AnIso' (QVar h) Identifier
    kindOf :: Proxy h -> ParamKind

instance NominalParamKind T.Row where
    qvarIdentifier _ = T._Var
    kindOf _ = RowParam
instance NominalParamKind T.Type where
    qvarIdentifier _ = T._Var
    kindOf _ = TypeParam

convertNominalParams ::
    forall m env p.
    (Monad m, HasCodeAnchors env m, NominalParamKind p) =>
    (T.Tag -> T.Tag -> T m ()) -> (T.Tag -> T m ()) ->
    (T.Tag -> ParamKind -> T m()) ->
    OnceT (T m) (TagChoice InternalName (T m)) ->
    EntityId -> Set T.Tag -> HyperScheme.QVars # p ->
    ReaderT env (OnceT (T m)) [TaggedItem InternalName (OnceT (T m)) (T m) (Property (T m) ParamKind)]
convertNominalParams replaceTag delParam setKind addParam entityId tagList =
    traverse convParam . (^.. qvars)
    where
        paramKind = kindOf (Proxy @p)
        convParam t =
            ConvertTag.ref t Nothing (tagList & Lens.contains t .~ False) (pure ()) resultInfo >>= lift <&>
            \tag ->
            let p = tag ^. tagRefTag . tagVal
            in
            TaggedItem
            { _tiTag = tag
            , _tiValue = Property paramKind (setKind p)
            , _tiDelete = delParam p
            , _tiAddAfter = addParam
            }
            where
                resultInfo () = ConvertTag.TagResultInfo <$> EntityId.ofTag entityId <*> replaceTag t

qvars ::
    forall t.
    NominalParamKind t =>
    Lens.Fold (HyperScheme.QVars # t) T.Tag
qvars = HyperScheme._QVars . Lens.ifolded . Lens.asIndex . Lens.cloneIso (qvarIdentifier (Proxy @t)) . Lens.to T.Tag

editParamInScheme :: _ -> Identifier -> T.Scheme # Pure -> T.Scheme # Pure
editParamInScheme edit p old@(HyperScheme.Scheme forAlls typ)
    | isShadowed = old
    | otherwise = edit p typ & HyperScheme.Scheme forAlls
    where
        isShadowed = p `elem` hfoldMap (Proxy @ExprLens.HasQVar #> (^.. schemeVars)) forAlls

replaceQVars :: (RTraversable k, ExprLens.HasQVarIds expr) => Identifier -> Identifier -> (k # expr) -> k # expr
replaceQVars newId oldId = ExprLens.qVarIds . Lens.filteredBy (Lens.only oldId) .~ newId

schemeVars :: forall n. ExprLens.HasQVar n => Lens.Fold (HyperScheme.QVars # n) Identifier
schemeVars = HyperScheme._QVars . Lens.ifolded . Lens.asIndex . ExprLens.qvarId (Proxy @n)

qvarssIdentifiers :: Lens.Traversal' (T.Types # HyperScheme.QVars) Identifier
qvarssIdentifiers f = htraverse (Proxy @ExprLens.HasQVar #> ExprLens.qvarsQVarIds f)

editNom :: Lens.Bifunctor p =>
    (Nominal.NomVarTypes t # HyperScheme.QVars -> Nominal.NomVarTypes t # HyperScheme.QVars) ->
    (HyperScheme.Scheme (Nominal.NomVarTypes t) t # Pure ->
        HyperScheme.Scheme (Nominal.NomVarTypes t) t # Pure) ->
    p (Nominal.NomVarTypes t # HyperScheme.QVars) (Pure # Nominal.NominalDecl t) ->
    p (Nominal.NomVarTypes t # HyperScheme.QVars) (Pure # Nominal.NominalDecl t)
editNom params scheme =
    Lens.bimap params (_Pure %~ (Nominal.nParams %~ params) . (Nominal.nScheme %~ scheme))

filterParams :: forall t. ExprLens.HasQVar t => Identifier -> HyperScheme.QVars # t -> HyperScheme.QVars # t
filterParams p =
    HyperScheme._QVars %~ Map.filterWithKey (\k _ -> k ^. ExprLens.qvarId (Proxy @t) /= p)

class DelTypeVar t where
    delTypeVar :: Identifier -> Pure # t -> Pure # t

instance DelTypeVar T.Type where
    delTypeVar v =
        _Pure %~
        \case
        T.TVar (T.Var x) | x == v -> Pure T.REmpty & T.TRecord
        b -> hmap (Proxy @DelTypeVar #> delTypeVar v) b

instance DelTypeVar T.Row where
    delTypeVar v =
        _Pure %~
        \case
        T.RVar (T.Var x) | x == v -> T.REmpty
        b -> hmap (Proxy @DelTypeVar #> delTypeVar v) b

pane ::
    (Monad m, HasCodeAnchors env m) =>
    env -> NominalId -> OnceT (T m) (PaneBody v InternalName (OnceT (T m)) (T m) a)
pane env nomId =
    do
        nom <- ExprLoad.nominal nomId & lift
        tag <- ConvertTag.taggedEntityWith (env ^. Anchors.codeAnchors) Nothing nomId & join
        let entityId = EntityId.ofNominalPane nomId
        (params, body) <-
            case nom of
            Left params -> pure (params, Nothing)
            Right (Pure (Nominal.NominalDecl params scheme)) ->
                ConvertType.convertScheme (EntityId.currentTypeOf entityId) (Pure scheme)
                <&> Just
                <&> (,) params
                & (`runReaderT` env)
        let tagList = hfoldMap (Proxy @NominalParamKind #> (^.. qvars)) params & Set.fromList
        let writeReplacedTypeVarById (T.Tag oldId) (T.Tag newId) =
                editNom (qvarssIdentifiers . Lens.filteredBy (Lens.only oldId) .~ newId)
                (editParamInScheme (replaceQVars newId) oldId) nom
                & ExprLoad.writeNominal nomId
        let withDeletedParam (T.Tag t) =
                editNom (hmap (Proxy @ExprLens.HasQVar #> filterParams t))
                (editParamInScheme delTypeVar t) nom
        let delParam = ExprLoad.writeNominal nomId . withDeletedParam
        let setKind (T.Tag t) k =
                withDeletedParam (T.Tag t)
                & editNom add id
                & ExprLoad.writeNominal nomId
                where
                    add =
                        case k of
                        TypeParam -> T.tType . HyperScheme._QVars . Lens.at (T.Var t) ?~ mempty
                        RowParam -> T.tRow . HyperScheme._QVars . Lens.at (T.Var t) ?~ mempty
        let addParamInfo () t@(T.Tag i) =
                editNom (T.tType . HyperScheme._QVars . Lens.at (T.Var i) ?~ mempty) id nom
                & ExprLoad.writeNominal nomId
                & ConvertTag.TagResultInfo (EntityId.ofTag entityId t)
        let usedParams = params ^.. qvarssIdentifiers <&> T.Tag
        addParam <-
            runReaderT
            (ConvertTag.replace (nameWithContext Nothing nomId) (Set.fromList usedParams) (pure ()) addParamInfo) env
            & join
        paramsS <-
            hfoldMap
            (Proxy @NominalParamKind #>
                convertNominalParams writeReplacedTypeVarById delParam setKind addParam entityId tagList)
            params
            & (`runReaderT` env)
        PaneNominal NominalPane
            { _npName = tag
            , _npParams = ConvertTaggedList.convert addParam paramsS
            , _npEntityId = entityId
            , _npBody = body
            , _npNominalId = nomId
            } & pure
