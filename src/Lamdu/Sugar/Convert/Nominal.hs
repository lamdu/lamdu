{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Convert.Nominal
    ( pane
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Reader (ReaderT(..))
import           Control.Monad.Reader.Instances ()
import           Data.Property (Property(..), pVal, pureModify)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Hyper
import qualified Hyper.Syntax.Nominal as Nominal
import qualified Hyper.Syntax.Scheme as HyperScheme
import           Hyper.Unify (TypeConstraintsOf)
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

addQVar :: (Monoid (TypeConstraintsOf t), Ord (QVar t)) => QVar t -> HyperScheme.QVars # t -> HyperScheme.QVars # t
addQVar t = HyperScheme._QVars . Lens.at t ?~ mempty

makeAddParam ::
    (Monad n, HasCodeAnchors env n) =>
    Property (T n) (T.Types # HyperScheme.QVars) -> EntityId -> env ->
    OnceT (T n) (OnceT (T n) (TagChoice InternalName (T n)))
makeAddParam prop entityId =
    ConvertTag.replace (nameWithContext Nothing entityId) (Set.fromList usedParams) (pure ()) addParamInfo
    where
        usedParams = prop ^.. pVal . qvarssIdentifiers <&> T.Tag
        addParamInfo () t@(T.Tag i) =
            pureModify prop (T.tType %~ addQVar (T.Var i))
            & ConvertTag.TagResultInfo (EntityId.ofTag entityId t)

setParamKind :: T.Tag -> ParamKind -> T.Types # HyperScheme.QVars -> T.Types # HyperScheme.QVars
setParamKind (T.Tag t) k =
    add . hmap (Proxy @ExprLens.HasQVar #> filterParams t)
    where
        add =
            case k of
            TypeParam -> T.tType %~ addQVar (T.Var t)
            RowParam -> T.tRow %~ addQVar (T.Var t)

nominalParams ::
    (Monad m, HasCodeAnchors env m) =>
    env ->
    T.Types # HyperScheme.QVars ->
    (T.Types # HyperScheme.QVars ->
        (HyperScheme.Scheme T.Types T.Type # Pure -> HyperScheme.Scheme T.Types T.Type # Pure) ->
        T m ()) ->
    EntityId ->
    OnceT (T m) (TaggedList InternalName (OnceT (T m)) (T m) (Property (T m) ParamKind))
nominalParams env params edit entityId =
    do
        addParam <- makeAddParam (Property params (`edit` id)) entityId env
        hfoldMap
            (Proxy @NominalParamKind #> convertNominalParams writeReplacedTypeVarById delParam setKind addParam entityId tagList)
            params
            <&> ConvertTaggedList.convert addParam
            & (`runReaderT` env)
    where
        tagList = hfoldMap (Proxy @NominalParamKind #> (^.. qvars)) params & Set.fromList
        writeReplacedTypeVarById (T.Tag oldId) (T.Tag newId) =
            edit (params & qvarssIdentifiers . Lens.filteredBy (Lens.only oldId) .~ newId)
            (editParamInScheme (replaceQVars newId) oldId)
        filterParamList t = hmap (Proxy @ExprLens.HasQVar #> filterParams t) params
        delParamInScheme = editParamInScheme delTypeVar
        delParam (T.Tag t) = edit (filterParamList t) (delParamInScheme t)
        setKind (T.Tag t) k = edit (setParamKind (T.Tag t) k params) (delParamInScheme t)

pane ::
    (Monad m, HasCodeAnchors env m) =>
    env -> NominalId -> OnceT (T m) (PaneBody v InternalName (OnceT (T m)) (T m) a)
pane env nomId =
    do
        nom <- ExprLoad.nominal nomId & lift
        let (params, mScheme) =
                case nom of
                Left p -> (p, Nothing)
                Right (Pure (Nominal.NominalDecl p scheme)) -> (p, Just scheme)
        body <- Lens._Just (ConvertType.convertScheme (EntityId.currentTypeOf entityId) . Pure) mScheme & (`runReaderT` env)
        let editNom p e =
                Lens.bimap (const p) (_Pure %~ (Nominal.nParams .~ p) . (Nominal.nScheme %~ e)) nom
                & ExprLoad.writeNominal nomId
        paramsS <- nominalParams env params editNom entityId
        tag <- ConvertTag.taggedEntityWith (env ^. Anchors.codeAnchors) Nothing nomId
        PaneNominal NominalPane
            { _npName = tag
            , _npParams = paramsS
            , _npEntityId = entityId
            , _npBody = body
            , _npNominalId = nomId
            } & pure
    where
        entityId = EntityId.ofNominalPane nomId
