{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Lamdu.Sugar.Convert.Monad
    ( OuterScopeInfo(..), osiPos, osiScope
    , RecursiveRef(..), rrDefI, rrDefType
    , ScopeInfo(..), siRecordParams, siNullParams, siLetItems, siExtractPos, siFloatPos

    , Context(..)
    , scInferContext, scTopLevelExpr, scPostProcessRoot, siRecursiveRef, scConfig
    , scScopeInfo, scDebugMonitors, scCacheFunctions
    , scOutdatedDefinitions, scFrozenDeps

    , cachedFunc

    , ConvertM(..), run, convertOnce
    , PositionInfo(..)
    , convertSubexpression
    , typeProtectedSetToVal, typeProtect, postProcessAssert, postProcessWith
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, MonadOnce(..), Typeable)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Property (Property)
import           Hyper.Unify (UVar)
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Definition (Deps)
import           Lamdu.Calc.Infer (InferState)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Config (Config)
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

data OuterScopeInfo m = OuterScopeInfo
    { _osiPos :: ExprIRef.HRef m # V.Term
    , _osiScope :: V.Scope # UVar
    }
Lens.makeLenses ''OuterScopeInfo

data RecursiveRef m = RecursiveRef
    { _rrDefI :: ExprIRef.DefI m
    , _rrDefType :: Pure # T.Scheme
    }
Lens.makeLenses ''RecursiveRef

data ScopeInfo m = ScopeInfo
    { _siRecordParams :: Map V.Var (Set [T.Tag])
    , _siNullParams :: Set V.Var
    , -- Each let item potentially has an inline action
      _siLetItems :: Map V.Var (Sugar.EntityId -> Sugar.VarInline (T m))
      -- TODO: siTagParamInfos needs a reverse-lookup map too
    , _siExtractPos :: Maybe (OuterScopeInfo m)
    , _siFloatPos :: Maybe (OuterScopeInfo m)
    , -- The globals we artificially inject into the scope in order to
      -- infer their type supporting mutual recursions
      _siRecursiveRef :: Maybe (RecursiveRef m)
    }
Lens.makeLenses ''ScopeInfo

newtype ConvertM m a = ConvertM (ReaderT (Context m) (OnceT (T m)) a)
    deriving newtype (Functor, Applicative, Monad, MonadReader (Context m))

instance Monad m => MonadTransaction m (ConvertM m) where
    transaction = ConvertM . lift . lift

data PositionInfo = BinderPos | ExpressionPos deriving Eq

data Context m = Context
    { _scInferContext :: InferState
    , _scCodeAnchors :: Anchors.CodeAnchors m
    , _scScopeInfo :: ScopeInfo m
    , _scTopLevelExpr :: Ann (Input.Payload m) # V.Term
    , -- Check whether the definition is valid after an edit,
      -- so that can detach bad edits.
      _scPostProcessRoot :: T m PostProcess.Result
    , _scOutdatedDefinitions ::
        Map V.Var (Sugar.DefinitionOutdatedType InternalName (T m) ())
    , _scFrozenDeps :: Property (T m) Deps
    , _scDebugMonitors :: Debug.Monitors
    , _scCacheFunctions :: Cache.Functions
    , _scConfig :: Config
    , scConvertSubexpression ::
        PositionInfo -> Ann (Input.Payload m) # V.Term ->
        ConvertM m (ExpressionU EvalPrep m)
    }
Lens.makeLenses ''Context

instance Anchors.HasCodeAnchors (Context m) m where codeAnchors = scCodeAnchors

cachedFunc :: Monad m => (Cache.Functions -> a) -> ConvertM m a
cachedFunc f = Lens.view scCacheFunctions <&> f

typeProtect :: Monad m => T m PostProcess.Result -> T m a -> T m (Maybe a)
typeProtect checkOk act =
    do
        ((result, isOk), changes) <- (,) <$> act <*> checkOk & Transaction.fork
        case isOk of
            PostProcess.GoodExpr -> Just result <$ Transaction.merge changes
            PostProcess.BadExpr _ -> pure Nothing

typeProtectedSetToVal ::
    Monad m =>
    ConvertM m
    (ExprIRef.HRef m # V.Term -> ExprIRef.ValI m -> T m (ExprIRef.ValI m))
typeProtectedSetToVal =
    Lens.view scPostProcessRoot
    <&> \checkOk dest valI ->
    do
        mResult <- DataOps.replace dest valI & typeProtect checkOk
        case mResult of
            Just result -> pure result
            Nothing ->
                do
                    res <- DataOps.setToAppliedHole valI dest
                    _ <- checkOk
                    pure res

postProcessWith :: Monad m => ConvertM m ((Pure # T.TypeError -> T m ()) -> T m ())
postProcessWith =
    Lens.view scPostProcessRoot
    <&> \postProcess onError ->
    postProcess
    >>=
    \case
    PostProcess.GoodExpr -> pure ()
    PostProcess.BadExpr err ->
        do
            onError err
            postProcess >>=
                \case
                PostProcess.GoodExpr -> pure ()
                PostProcess.BadExpr e -> error ("postProcessWith onError failed: " <> prettyShow e)

postProcessAssert :: Monad m => ConvertM m (T m ())
postProcessAssert = postProcessWith ?? error . prettyShow

run :: (HasCallStack, Monad m) => Context m -> ConvertM m a -> OnceT (T m) a
run ctx (ConvertM action) =
    runReaderT action ctx & report
    where
        Debug.EvaluatorM report = ctx ^. scDebugMonitors . Debug.sugaring . Debug.mAction

convertOnce :: (Monad m, Typeable a) => ConvertM m a -> ConvertM m (OnceT (T m) a)
convertOnce action =
    Lens.view id
    <&> (`run` action)
    >>= ConvertM . lift . once

convertSubexpression :: Monad m => Ann (Input.Payload m) # V.Term -> ConvertM m (ExpressionU EvalPrep m)
convertSubexpression exprI =
    do
        convertSub <- Lens.view id <&> scConvertSubexpression
        convertSub ExpressionPos exprI
