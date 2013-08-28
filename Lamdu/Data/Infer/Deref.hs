{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Deref
  ( M, expr, entireExpr, deref
  , toInferError
  , DerefedSTV(..), dValue, dType, dScope
  , Error(..)
  , RefData.Restriction(..), ExprRef
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Function.Decycle (decycle)
import Data.Store.Guid (Guid)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.RefTags (ExprRef, ParamRef)
import Lamdu.Data.Infer.TypedValue (tvVal, tvType, ScopedTypedValue, stvTV, stvScope)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData

data Error def = InfiniteExpression (ExprRef def)
  deriving (Show, Eq, Ord)

type Expr def = Expr.Expression (LoadedDef def) (ExprRef def)

data DerefedSTV def = DerefedSTV
  { _dValue :: Expr def
  , _dType :: Expr def
  , _dScope :: RefData.Scope def
  }
Lens.makeLenses ''DerefedSTV

type M def = StateT (Context def) (Either (Error def))
mError :: Error def -> M def a
mError = lift . Left
mGuidAliases :: StateT (GuidAliases def) (Either (Error def)) a -> M def a
mGuidAliases = Lens.zoom Context.guidAliases

-- | The stored guid names we know for paremeter refs (different
-- mapping in different subexprs)
type StoredGuids def = [(ParamRef def, Guid)]

canonizeGuid ::
  MonadA m =>
  StoredGuids def -> Guid -> StateT (GuidAliases def) m Guid
canonizeGuid storedGuidsOfRefs guid = do
  guidRep <- GuidAliases.getRep guid
  storedGuidsOfReps <-
    storedGuidsOfRefs
    & Lens.traverse . Lens._1 %%~ GuidAliases.find
  case lookup guidRep storedGuidsOfReps of
    Nothing -> State.gets (GuidAliases.guidOfRep guidRep)
    Just storedGuid -> return storedGuid

deref :: StoredGuids def -> ExprRef def -> M def (Expr def)
deref storedGuids =
  decycle go
  where
    go Nothing ref = mError $ InfiniteExpression ref
    go (Just recurse) ref = do
      refData <- Lens.zoom Context.ufExprs (UFData.read ref)
      refData ^. RefData.rdBody
        & Lens.traverse %%~ recurse
        >>= ExprLens.bodyParamIds %%~ mGuidAliases . canonizeGuid storedGuids
        <&> (`Expr.Expression` ref)

expr ::
  Expr.Expression ldef (ScopedTypedValue def, a) ->
  M def (Expr.Expression ldef (M def (DerefedSTV def), a))
expr =
  go []
  where
    go storedGuids (Expr.Expression storedBody (stv, pl)) = do
      newStoredGuids <-
        case storedBody ^? Expr._BodyLam . Expr.lamParamId of
        Nothing -> return storedGuids
        Just storedParamId -> do
          storedParamIdRep <- mGuidAliases $ GuidAliases.getRep storedParamId
          return $ (storedParamIdRep, storedParamId) : storedGuids
      let
        derefTV =
          DerefedSTV
          <$> deref newStoredGuids (stv ^. stvTV . tvVal)
          <*> deref newStoredGuids (stv ^. stvTV . tvType)
          <*> pure (stv ^. stvScope)
      storedBody
        & Lens.traverse %%~ go newStoredGuids
        <&> (`Expr.Expression` (derefTV, pl))

entireExpr ::
  Expr.Expression ldef (ScopedTypedValue def, a) ->
  M def (Expr.Expression ldef (DerefedSTV def, a))
entireExpr = (>>= Lens.sequenceOf (Lens.traverse . Lens._1)) . expr
------- Lifted errors:

toInferError :: Error def -> InferM.Error def
toInferError (InfiniteExpression ref) = InferM.InfiniteExpression ref
