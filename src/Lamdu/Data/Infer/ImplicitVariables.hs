{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Data.Infer.ImplicitVariables
  ( addVariables, Payload(..)
  ) where

import Control.Applicative (liftA2)
import Control.Lens (SimpleLens, (^.))
import Control.Monad.Trans.State (State, runState)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Functor.Identity (Identity(..))
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.Infer as Infer

inferredLens :: SimpleLens (Data.Expression def (Infer.Inferred def, b)) (Infer.Inferred def)
inferredLens = Data.ePayload . Lens._1

data Payload a = Stored a | AutoGen Guid
  deriving (Eq, Ord, Show, Functor, Typeable)
derive makeBinary ''Payload

isUnrestrictedHole :: Data.Expression def Infer.IsRestrictedPoly -> Bool
isUnrestrictedHole
  (Data.Expression
    (Data.ExpressionLeaf Data.Hole)
    Infer.UnrestrictedPoly) = True
isUnrestrictedHole _ = False

addVariablesGen ::
  (Ord def, RandomGen g) => g ->
  Data.Expression def (Infer.Inferred def, b) ->
  Data.Expression def (Infer.Inferred def, Payload a) ->
  State (Infer.Context def) (Data.Expression def (Infer.Inferred def, Payload a))
addVariablesGen gen rootTypeExpr expr =
  case unrestrictedHoles of
  [] -> return expr
  (hole : _) -> do
    paramTypeTypeRef <- Infer.createRefExpr
    let
      holePoint = Infer.iPoint . fst $ hole ^. Data.ePayload
      paramTypeRef = Infer.tvType $ Infer.nRefs holePoint
      paramTypeNode =
        Infer.InferNode (Infer.TypedValue paramTypeRef paramTypeTypeRef) mempty
      loaded = runIdentity $ Infer.load loader Nothing getVar
    _ <- Infer.inferLoaded actions loaded holePoint
    newRootNode <- Infer.newNodeWithScope mempty
    let
      paramTypeExpr = Data.Expression Data.hole (paramTypeNode, AutoGen (Guid.augment "paramType" paramGuid))
      newRootLam = Data.makeLambda paramGuid paramTypeExpr $ Lens.over (Lens.mapped . Lens._1) Infer.iPoint expr
      newRootExpr = Data.Expression newRootLam (newRootNode, AutoGen (Guid.augment "root" paramGuid))
    Infer.addRules actions [fmap fst newRootExpr]
    inferredRootTypeExpr <-
      State.gets . Infer.derefExpr $
      Lens.over (Lens.mapped . Lens._1) Infer.iPoint rootTypeExpr
    inferredNewRootExpr <- State.gets $ Infer.derefExpr newRootExpr
    addVariablesGen newGen inferredRootTypeExpr inferredNewRootExpr
  where
    loader =
      Infer.Loader . const . Identity $ error "Should not be loading defs when loading a mere getVar"
    unrestrictedHoles =
      filter (isUnrestrictedHole . Infer.iValue . fst . Lens.view Data.ePayload) .
      reverse $ Data.subExpressions rootTypeExpr
    getVar = Data.pureExpression $ Data.makeParameterRef paramGuid
    (paramGuid, newGen) = random gen

-- TODO: Infer.Utils
actions :: Infer.InferActions def Identity
actions = Infer.InferActions . const . Identity $ error "Infer error when adding implicit vars!"

addVariables ::
  (MonadA m, Ord def, RandomGen g) =>
  g -> Infer.Loader def m ->
  Infer.Context def -> Data.Expression def (Infer.Inferred def, a) ->
  m (Infer.Context def, Data.Expression def (Infer.Inferred def, Payload a))
addVariables gen loader initialInferContext expr =
  fmap swap $
  liftA2 onLoaded
  (load Data.pureSet)
  (load (Infer.iType rootNode))
  where
    load = Infer.load loader Nothing -- <-- TODO: Nothing?
    onLoaded loadedSet loadedRootType =
      (`runState` initialInferContext) $ do
        inferredSet <-
          (fmap . fmap) fst $
          Infer.inferLoaded actions loadedSet =<< Infer.newNodeWithScope mempty
        let
          rootTypeTypeRef = Infer.tvVal . Infer.nRefs . Infer.iPoint $ inferredSet ^. Data.ePayload
          rootTypeNode = Infer.InferNode (Infer.TypedValue rootTypeRef rootTypeTypeRef) mempty
        inferredRootType <- Infer.inferLoaded actions loadedRootType rootTypeNode
        addVariablesGen gen inferredRootType $
          Lens.over (Lens.mapped . Lens._2) Stored expr
    rootNode = expr ^. inferredLens
    rootTypeRef = (Infer.tvType . Infer.nRefs . Infer.iPoint) rootNode
