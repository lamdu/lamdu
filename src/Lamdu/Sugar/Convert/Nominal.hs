{-# LANGUAGE GADTs, TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Convert.Nominal
    ( convertToNom, convertFromNom, pane
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Reader (ReaderT(..))
import           Control.Monad.Reader.Instances ()
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import qualified Data.Set as Set
import           Hyper
import           Hyper.Syntax.Nominal (ToNom(..))
import qualified Hyper.Syntax.Nominal as Nominal
import qualified Hyper.Syntax.Scheme as HyperScheme
import           Hyper.Unify.QuantifiedVar (QVar)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (HasCodeAnchors)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import qualified Lamdu.Sugar.Convert.Text as ConvertText
import qualified Lamdu.Sugar.Convert.Type as ConvertType
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convertToNom ::
    (Monad m, Monoid a) =>
    ToNom NominalId V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convertToNom t@(ToNom tid x) pl =
    do
        ConvertText.text t pl & justToLeft
        Nominal
            <$> ConvertTId.convert tid
            <*> ConvertBinder.convertBinder x
            <&> BodyToNom
            >>= addActions (_ANode # x) pl
            & lift
    & runMatcherT
    <&> annotation . pActions . mApply .~ Nothing

convertFromNom ::
    (Monad m, Monoid a) =>
    NominalId -> Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU v m a)
convertFromNom tid pl =
    ConvertTId.convert tid <&> PfFromNom <&> BodyPostfixFunc >>= addActions (Const ()) pl

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
    (Identifier -> Identifier -> T m ()) -> EntityId -> Set T.Tag -> HyperScheme.QVars # p ->
    ReaderT env (OnceT (T m)) [NominalParam InternalName (OnceT (T m)) (T m)]
convertNominalParams replaceTypeVars entityId tagList =
    traverse (convParam (kindOf (Proxy @p))) . (^.. qvars)
    where
        replaceNominalParamAsTag (T.Tag oldIdentifier) (T.Tag newIdentifier) =
            replaceTypeVars oldIdentifier newIdentifier
        convParam kind t =
            ConvertTag.ref t Nothing (tagList & Lens.contains t .~ False)
            (EntityId.ofTag entityId)
            (replaceNominalParamAsTag t)
            >>= lift
            <&> \tagRef -> NominalParam
            { _pName = tagRef
            , _pKind = kind
            }

qvars ::
    forall t.
    NominalParamKind t =>
    Lens.Fold (HyperScheme.QVars # t) T.Tag
qvars = HyperScheme._QVars . Lens.ifolded . Lens.asIndex . Lens.cloneIso (qvarIdentifier (Proxy @t)) . Lens.to T.Tag

replaceInScheme :: Identifier -> Identifier -> T.Scheme # Pure -> T.Scheme # Pure
replaceInScheme oldId newId old@(HyperScheme.Scheme forAlls typ)
    | isShadowed = old
    | otherwise = HyperScheme.Scheme forAlls (typ & ExprLens.qVarIds . Lens.filteredBy (Lens.only oldId) .~ newId)
    where
        isShadowed = oldId `elem` hfoldMap (Proxy @ExprLens.HasQVar #> (^.. schemeVars)) forAlls

schemeVars :: forall n. ExprLens.HasQVar n => Lens.Fold (HyperScheme.QVars # n) Identifier
schemeVars = HyperScheme._QVars . Lens.ifolded . Lens.asIndex . ExprLens.qvarId (Proxy @n)

qvarssIdentifiers :: Lens.Traversal' (T.Types # HyperScheme.QVars) Identifier
qvarssIdentifiers f = htraverse (Proxy @ExprLens.HasQVar #> ExprLens.qvarsQVarIds f)

pane ::
    (Monad m, HasCodeAnchors env m) =>
    env -> NominalId -> OnceT (T m) (PaneBody v InternalName (OnceT (T m)) (T m) a)
pane env nomId =
    do
        nom <- ExprLoad.nominal nomId & lift
        tag <- ConvertTag.taggedEntityWith (env ^. Anchors.codeAnchors) Nothing nomId & join
        let entityId = EntityId.ofNominalPane nomId
        let replaceInParams oldId newId = qvarssIdentifiers . Lens.filteredBy (Lens.only oldId) .~ newId
        (params, body) <-
            case nom of
            Left params -> pure (params, Nothing)
            Right (Pure (Nominal.NominalDecl params scheme)) ->
                ConvertType.convertScheme (EntityId.currentTypeOf entityId) (Pure scheme)
                <&> Just
                <&> (,) params
                & (`runReaderT` env)
        let tagList = hfoldMap (Proxy @NominalParamKind #> (^.. qvars)) params & Set.fromList
        let writeReplacedTypeVarById oldId newId =
                Lens.bimap (replaceInParams oldId newId)
                ( _Pure %~
                    (Nominal.nParams %~ replaceInParams oldId newId) . (Nominal.nScheme %~ replaceInScheme oldId newId)
                ) nom & ExprLoad.writeNominal nomId
        paramsS <-
            hfoldMap (Proxy @NominalParamKind #> convertNominalParams writeReplacedTypeVarById entityId tagList)
            params
            & (`runReaderT` env)
        PaneNominal NominalPane
            { _npName = tag
            , _npParams = paramsS
            , _npEntityId = entityId
            , _npBody = body
            , _npNominalId = nomId
            } & pure
