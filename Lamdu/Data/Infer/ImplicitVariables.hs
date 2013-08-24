{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Data.Infer.ImplicitVariables
  ( add, Payload(..)
  ) where

import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT, mapStateT, state)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (traverse_)
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.TypedValue (ScopedTypedValue(..))
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Store.Guid as Guid
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.Deref as Deref
import qualified Lamdu.Data.Infer.LamWrap as LamWrap
import qualified Lamdu.Data.Infer.Load as Load
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified Lamdu.Data.Infer.TypedValue as TypedValue

data Payload a = Stored a | AutoGen Guid
  deriving (Eq, Ord, Show, Functor, Typeable)
derive makeBinary ''Payload

deref ::
  Deref.ExprRef def ->
  StateT (Context def)
  (Either (InferM.Error def))
  (Expr.Expression def (Deref.ExprRef def, [RefData.Restriction def]))
deref =
  Deref.deref [] {-TODO: <-- [] is icky API-}
  & Lens.mapped . Lens.sets mapStateT . Lens._Left %~ Deref.toInferError

add ::
  (Show def, Ord def, RandomGen gen) =>
  gen -> def ->
  Expr.Expression (Load.LoadedDef def) (ScopedTypedValue def, a) ->
  StateT (Context def) (Either (InferM.Error def))
  (Expr.Expression (Load.LoadedDef def) (ScopedTypedValue def, Payload a))
add gen def expr =
  expr ^.. Lens.traverse . Lens._1 . TypedValue.stvTV . TypedValue.tvType
  & traverse_ (onEachType def)
  & (`execStateT` gen)
  & (`execStateT` (expr <&> Lens._2 %~ Stored))

onEachType ::
  (Ord def, RandomGen gen) =>
  def -> Deref.ExprRef def ->
  StateT gen
  (StateT (Expr.Expression (Load.LoadedDef def) (ScopedTypedValue def, Payload a))
   (StateT (Context def)
    (Either (InferM.Error def)))) ()
onEachType def typeRef = do
  -- TODO: can use a cached deref here
  typeExpr <- lift . lift $ deref typeRef
  traverse_ onEachTypeHole $ typeExpr ^.. ExprLens.holePayloads
  where
    onEachTypeHole (_, (_:_)) = return ()
    onEachTypeHole (holeRef, []) = do -- no restrictions:
      -- Found unrestricted hole, make type variable here
      paramTypeRef <-
        lift . lift $ Context.freshHole (RefData.emptyScope def)
      paramId <- state random
      lift State.get
        >>= lift . lift . LamWrap.lambdaWrap paramId paramTypeRef
        <&> Lens.mapped . Lens._2 %~ joinPayload . toPayload paramId
        >>= lift . State.put
      lift . lift $ setImplicitVar paramId holeRef
    toPayload paramId Nothing = AutoGen $ Guid.augment "implicitLam" paramId
    toPayload _ (Just x) = Stored x

joinPayload :: Payload (Payload a) -> Payload a
joinPayload (AutoGen guid) = AutoGen guid
joinPayload (Stored x) = x

setImplicitVar ::
  Ord def =>
  Guid -> Deref.ExprRef def ->
  StateT (Context def) (Either (InferM.Error def)) ()
setImplicitVar paramId holeRef = do
  holeScope <-
    Lens.zoom Context.ufExprs (UFData.read holeRef)
    <&> (^. RefData.rdScope)
  getVarValRef <-
    Load.exprIntoContext holeScope $
    ExprLens.pureExpr . ExprLens.bodyParameterRef # paramId
  void $ Infer.unifyRefs getVarValRef holeRef
