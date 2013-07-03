{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Lamdu.Data.Expression.Infer.Conflicts
  ( InferredWithConflicts(..)
  , inferWithConflicts
  , iwcInferredTypes
  , iwcInferredValues
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((%~))
import Control.Monad (void)
import Control.Monad.Trans.State (State, mapStateT)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.Binary (Binary(..))
import Data.Foldable (sequenceA_)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer

data InferredWithConflicts def = InferredWithConflicts
  { iwcInferred :: Infer.Inferred def
  , iwcTypeConflicts :: [Infer.Error def]
  , iwcValueConflicts :: [Infer.Error def]
  } deriving (Typeable)
-- Requires Ord instance for def, cannot derive
instance (Ord def, Binary def) => Binary (InferredWithConflicts def) where
  get = InferredWithConflicts <$> get <*> get <*> get
  put (InferredWithConflicts a b c) = sequenceA_ [put a, put b, put c]

newtype ConflictMap def =
  ConflictMap { unConflictMap :: Map Infer.ExprRef (Set (Infer.Error def)) }

instance Ord def => Monoid (ConflictMap def) where
  mempty = ConflictMap mempty
  mappend (ConflictMap x) (ConflictMap y) =
    ConflictMap $ Map.unionWith mappend x y

getConflicts :: Infer.ExprRef -> ConflictMap def -> [Infer.Error def]
getConflicts ref = maybe [] Set.toList . Map.lookup ref . unConflictMap

reportConflict :: Ord def => Infer.Error def -> Writer (ConflictMap def) ()
reportConflict err =
  Writer.tell . ConflictMap .
  Map.singleton (Infer.errRef err) $
  Set.singleton err

inferWithConflicts ::
  Ord def => Infer.Loaded def a -> Infer.Node def ->
  State (Infer.Context def)
  ( Bool
  , Expr.Expression def (InferredWithConflicts def, a)
  )
inferWithConflicts loaded node = do
  (exprInferred, conflictsMap) <-
    mapStateT (toRes . runWriter) $
    Infer.inferLoaded (Infer.InferActions reportConflict) loaded node
  let
    conflicts getRef x =
      getConflicts ((getRef . Infer.nRefs . Infer.iNode) x)
      conflictsMap
    toIWC x =
      InferredWithConflicts
      { iwcInferred = x
      , iwcValueConflicts = conflicts Infer.tvVal x
      , iwcTypeConflicts = conflicts Infer.tvType x
      }
  return
    ( Map.null $ unConflictMap conflictsMap
    , Lens.mapped . Lens._1 %~ toIWC $ exprInferred
    )
  where
    toRes ((a, s), w) = Identity ((a, w), s)

iwcInferredExprs ::
  Ord def =>
  (Infer.Inferred def -> Expr.Expression def a) ->
  (InferredWithConflicts def -> [Infer.Error def]) ->
  InferredWithConflicts def -> [Expr.Expression def ()]
iwcInferredExprs getVal eeConflicts ee =
  (void . getVal . iwcInferred) ee :
  (Set.toList . Set.fromList . map (snd . Infer.errMismatch) . eeConflicts) ee

iwcInferredTypes :: Ord def => InferredWithConflicts def -> [Expr.Expression def ()]
iwcInferredTypes = iwcInferredExprs Infer.iType iwcTypeConflicts

iwcInferredValues :: Ord def => InferredWithConflicts def -> [Expr.Expression def ()]
iwcInferredValues = iwcInferredExprs Infer.iValue iwcValueConflicts
