module Lamdu.Sugar.Convert.Nominal
    ( convertToNom, convertFromNom, pane
    ) where

import           Control.Monad.Once (OnceT)
import           Control.Monad.Reader (ReaderT(..))
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Hyper (_ANode)
import           Hyper.Syntax.Nominal (ToNom(..))
import qualified Hyper.Syntax.Nominal as Nominal
import qualified Hyper.Syntax.Scheme as HyperScheme
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

convertNominalTypeBody ::
    (Monad m, HasCodeAnchors env m) =>
    env -> EntityId -> T.Types # HyperScheme.QVars -> HyperScheme.Scheme _ _ # Pure ->
    T m (NominalTypeBody InternalName (T m))
convertNominalTypeBody env entityId _params scheme =
    ConvertType.convertScheme (EntityId.currentTypeOf entityId) (Pure scheme)
    & (`runReaderT` env)
    <&> \schemeS ->
    NominalTypeBody
    { _nominalType = schemeS
    , _nominalParams = () -- TODO
    }

pane ::
    (Monad m, HasCodeAnchors env m) =>
    env -> NominalId -> OnceT (T m) (PaneBody v InternalName (OnceT (T m)) (T m) a)
pane env nomId =
    do
        nom <- ExprLoad.nominal nomId & lift
        tag <- ConvertTag.taggedEntityWith (env ^. Anchors.codeAnchors) Nothing nomId & join
        let entityId = EntityId.ofNominalPane nomId
        body <- case nom of
            Nothing -> pure NominalPaneOpaque
            Just (Pure (Nominal.NominalDecl params scheme)) ->
                convertNominalTypeBody env entityId params scheme <&> NominalPaneType & lift
        PaneNominal NominalPane
            { _npName = tag
            , _npEntityId = entityId
            , _npBody = body
            , _npNominalId = nomId
            } & pure
