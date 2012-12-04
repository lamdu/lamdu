{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Editor.Data.Infer.Conflicts
  ( InferredWithConflicts(..)
  , inferWithConflicts
  , iwcInferredTypes
  , iwcInferredValues
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (void)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.Binary (Binary(..))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Editor.Data as Data
import qualified Editor.Data.Infer as Infer

data InferredWithConflicts def = InferredWithConflicts
  { iwcInferred :: Infer.Inferred def
  , iwcTypeConflicts :: [Infer.Error def]
  , iwcValueConflicts :: [Infer.Error def]
  }

instance (Ord def, Binary def) => Binary (InferredWithConflicts def) where
  get = InferredWithConflicts <$> get <*> get <*> get
  put (InferredWithConflicts a b c) = sequence_ [put a, put b, put c]

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
  Ord def => Infer.Loaded def a ->
  Infer.Context def -> Infer.InferNode def ->
  ( Bool
  , Infer.Context def
  , Data.Expression def (InferredWithConflicts def, a)
  )
inferWithConflicts loaded initialInferContext node =
  ( Map.null $ unConflictMap conflictsMap
  , resultInferContext
  , (fmap . first) toIWC exprInferred
  )
  where
    ((exprInferred, resultInferContext), conflictsMap) =
      runWriter . (`runStateT` initialInferContext) $
      Infer.inferLoaded (Infer.InferActions reportConflict)
      loaded node
    toIWC x =
      InferredWithConflicts
      { iwcInferred = x
      , iwcValueConflicts = conflicts Infer.tvVal x
      , iwcTypeConflicts = conflicts Infer.tvType x
      }
    conflicts getRef x =
      getConflicts ((getRef . Infer.nRefs . Infer.iPoint) x)
      conflictsMap

iwcInferredExprs ::
  Ord def =>
  (Infer.Inferred def -> Data.Expression def a) ->
  (InferredWithConflicts def -> [Infer.Error def]) ->
  InferredWithConflicts def -> [Data.Expression def ()]
iwcInferredExprs getVal eeConflicts ee =
  (void . getVal . iwcInferred) ee :
  (Set.toList . Set.fromList . map (snd . Infer.errMismatch) . eeConflicts) ee

iwcInferredTypes :: Ord def => InferredWithConflicts def -> [Data.Expression def ()]
iwcInferredTypes = iwcInferredExprs Infer.iType iwcTypeConflicts

iwcInferredValues :: Ord def => InferredWithConflicts def -> [Data.Expression def ()]
iwcInferredValues = iwcInferredExprs Infer.iValue iwcValueConflicts
