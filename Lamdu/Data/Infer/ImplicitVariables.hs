{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Data.Infer.ImplicitVariables
  ( add, Payload(..)
  ) where

import Control.Lens.Operators
import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT, state)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.RefData (RefData, LoadedExpr)
import Lamdu.Data.Infer.TypedValue (TypedValue(..))
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.OpaqueRef as OR
import qualified Data.Store.Guid as Guid
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtils
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

add ::
  (Show def, Ord def, RandomGen gen) =>
  gen -> def ->
  LoadedExpr def (TypedValue def, a) ->
  StateT (Context def) (Either (InferM.Error def))
  (LoadedExpr def (TypedValue def, Payload a))
add gen def expr = do
  derefedType <-
    State.mapStateT (Lens._Left .~ error "ImplicitVariable.add: deref should succeed!") $
    Deref.deref [] outerTypeRef
  let
    relevantHoles =
      OR.refSetToList . OR.refSetFromList $
      map snd . filter (Lens.has (Lens._1 . ExprUtils._Negative)) $
      ExprUtils.annotateTypePositions
      derefedType ^.. ExprLens.holePayloads
    holesWithMaybeTypes = map withType relevantHoles
    withType x = (x, typeOfValRef ^. Lens.at x)
    holesWithTypes =
      holesWithMaybeTypes
      >>= Lens._2 %%~ (^.. Lens._Just)
      <&> toTv
    toTv (v, t) = TypedValue v t
  mapM_ (onEachHole def) holesWithTypes
    & (`execStateT` gen)
    & (`execStateT` (expr <&> Lens._2 %~ Stored))
  where
    outerTypeRef = expr ^. Expr.ePayload . Lens._1 . TypedValue.tvType
    storedTvs = expr ^.. Lens.traverse . Lens._1
    typeOfValRef = OR.refMapFromList $ map tvToTup storedTvs
    tvToTup tv = (tv ^. TypedValue.tvVal, tv ^. TypedValue.tvType)

isUnrestrictedHole :: RefData ref -> Bool
isUnrestrictedHole refData =
  null (refData ^. RefData.rdRestrictions)
  && Lens.has (RefData.rdBody . ExprLens.bodyHole) refData

onEachHole ::
  (Ord def, RandomGen gen) =>
  def ->
  TypedValue def ->
  StateT gen
  (StateT (LoadedExpr def (TypedValue def, Payload a))
   (StateT (Context def)
    (Either (InferM.Error def)))) ()
onEachHole def tv@(TypedValue valRef _) = do
  iValData <- lift . lift . Lens.zoom Context.ufExprs $ UFData.read valRef
  when (isUnrestrictedHole iValData) $ do
    paramId <- state random
    -- Make a new type ref for the implicit (we can't just re-use the
    -- given tv type ref because we need to intersect/restrict its
    -- scope)
    -- TODO: If this uses a scope-with-def, it will get itself in
    -- scope. If it doesn't, it won't get next variables in scope...
    implicitTypeRef <-
      lift . lift $ Context.freshHole (RefData.emptyScope def)
    -- Wrap with (paramId:implicitTypeRef) lambda
    lift State.get
      >>= lift . lift . LamWrap.lambdaWrap paramId implicitTypeRef
      -- TODO: Is it OK that we use same AutoGen/guid for new Lam and
      -- new ParamType in here?
      <&> Lens.mapped . Lens._2 %~ joinPayload . toPayload paramId
      >>= lift . State.put
    varScope <-
      lift . lift . Lens.zoom Context.ufExprs $
      UFData.read valRef <&> (^. RefData.rdScope)
    -- implicitValRef <= getVar paramId
    implicitValRef <-
      lift . lift . Load.exprIntoContext varScope $
      ExprLens.pureExpr . ExprLens.bodyParameterRef # paramId
    -- tv <= TV implicitValRef implicitTypeRef
    let
      implicit = TypedValue implicitValRef implicitTypeRef
    void . lift . lift $ Infer.unify tv implicit
  where
    toPayload paramId Nothing = AutoGen $ Guid.augment "implicitLam" paramId
    toPayload _ (Just x) = Stored x

joinPayload :: Payload (Payload a) -> Payload a
joinPayload (AutoGen guid) = AutoGen guid
joinPayload (Stored x) = x
