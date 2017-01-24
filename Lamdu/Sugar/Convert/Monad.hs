{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, TemplateHaskell, PolymorphicComponents, ConstraintKinds, RecordWildCards #-}
module Lamdu.Sugar.Convert.Monad
    ( TagParamInfo(..)
    , TagFieldParam(..), _TagFieldParam, _CollidingFieldParam
    , OuterScopeInfo(..), osiPos, osiVarsUnderPos
    , ScopeInfo(..), siTagParamInfos, siNullParams, siLetItems, siOuter

    , Context(..)
    , scInferContext, scReinferCheckRoot, scGlobalsInScope
    , scCodeAnchors, scScopeInfo, scNominalsMap, scOutdatedDefinitions

    , ConvertM(..), run
    , readContext, liftTransaction, local
    , convertSubexpression
    , typeProtectTransaction, typeProtectedSetToVal
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Type.Nominal (Nominal(..))
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data TagParamInfo = TagParamInfo
    { tpiFromParameters :: V.Var -- TODO: Rename "From" to something else
    , tpiJumpTo :: Sugar.EntityId
    }

data TagFieldParam
    = -- Sugared field param:
      TagFieldParam TagParamInfo
    | -- Colliding (and thus non-sugared) field param
      CollidingFieldParam TagParamInfo

data OuterScopeInfo m = OuterScopeInfo
    { _osiPos :: Maybe (ExprIRef.ValIProperty m)
    , -- The vars that disappear from scope when moving up to pos
      _osiVarsUnderPos :: [V.Var]
    }
Lens.makeLenses ''OuterScopeInfo

data ScopeInfo m = ScopeInfo
    { _siTagParamInfos :: Map T.Tag TagFieldParam -- tag uuids
    , _siNullParams :: Set V.Var
    , -- Each let item potentially has an inline action
      _siLetItems :: Map V.Var (Sugar.BinderVarInline m)
      -- TODO: siTagParamInfos needs a reverse-lookup map too
    , -- Where "extract to let" goes:
      _siOuter :: OuterScopeInfo m
    }
Lens.makeLenses ''ScopeInfo

type T = Transaction

newtype ConvertM m a = ConvertM (ReaderT (Context m) (T m) a)
    deriving (Functor, Applicative, Monad)

data Context m = Context
    { _scInferContext :: Infer.Context
    , -- The globals we artificially inject into the scope in order to
      -- infer their type supporting mutual recursions
      _scGlobalsInScope :: Set (ExprIRef.DefI m)
    , _scCodeAnchors :: Anchors.CodeProps m
    , _scScopeInfo :: ScopeInfo m
    , -- Check whether the definition is valid after an edit,
      -- so that can hole-wrap bad edits.
      _scReinferCheckRoot :: T m Bool
    , -- The nominal types appearing in the converted expr and its subexpression
      _scNominalsMap :: Map T.NominalId Nominal
    , _scOutdatedDefinitions :: Map V.Var (Sugar.DefinitionOutdatedType m)
    , scConvertSubexpression ::
        forall a. Monoid a => Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
    }
Lens.makeLenses ''Context
Lens.makePrisms ''TagFieldParam

typeProtectTransaction :: Monad m => ConvertM m (T m a -> T m (Maybe a))
typeProtectTransaction =
    do
        checkOk <- (^. scReinferCheckRoot) <$> readContext
        let protect act =
                do
                    (resume, changes) <-
                        Transaction.fork $
                        do
                            result <- act
                            isOk <- checkOk
                            return $
                                if isOk
                                then (>> return (Just result)) . Transaction.merge
                                else const $ return Nothing
                    resume changes
        return protect

typeProtectedSetToVal ::
    Monad m =>
    ConvertM m
    (ExprIRef.ValIProperty m -> ExprIRef.ValI m -> T m (ExprIRef.ValI m))
typeProtectedSetToVal =
    do
        typeProtect <- typeProtectTransaction
        let setToVal dest valI =
                do
                    mResult <- typeProtect $ DataOps.replace dest valI
                    case mResult of
                        Just result -> return result
                        Nothing -> DataOps.setToWrapper valI dest
        return setToVal

run :: Context m -> ConvertM m a -> T m a
run ctx (ConvertM action) = runReaderT action ctx

readContext :: Monad m => ConvertM m (Context m)
readContext = ConvertM Reader.ask

local :: (Context m -> Context m) -> ConvertM m a -> ConvertM m a
local f (ConvertM act) = ConvertM $ Reader.local f act

liftTransaction :: Monad m => T m a -> ConvertM m a
liftTransaction = ConvertM . lift

convertSubexpression :: (Monad m, Monoid a) => Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
convertSubexpression exprI =
    do
        convertSub <- scConvertSubexpression <$> readContext
        convertSub exprI
