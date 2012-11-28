{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Editor.Data.Infer.Conflicts
  ( InferredWithConflicts(..)
  , inferWithConflicts
  , iwcInferredTypes
  , iwcInferredValues
  ) where

import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Traversable (Traversable)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer

data InferredWithConflicts a = InferredWithConflicts
  { iwcInferred :: Infer.Inferred a
  , iwcTypeConflicts :: [Infer.Error]
  , iwcValueConflicts :: [Infer.Error]
  } deriving (Functor, Foldable, Traversable)
derive makeBinary ''InferredWithConflicts

newtype ConflictMap =
  ConflictMap { unConflictMap :: Map Infer.ExprRef (Set Infer.Error) }

instance Monoid ConflictMap where
  mempty = ConflictMap mempty
  mappend (ConflictMap x) (ConflictMap y) =
    ConflictMap $ Map.unionWith mappend x y

getConflicts :: Infer.ExprRef -> ConflictMap -> [Infer.Error]
getConflicts ref = maybe [] Set.toList . Map.lookup ref . unConflictMap

reportConflict :: Infer.Error -> Writer ConflictMap ()
reportConflict err =
  Writer.tell . ConflictMap .
  Map.singleton (Infer.errRef err) $
  Set.singleton err

inferWithConflicts ::
  Infer.Loaded a -> Infer.IsNewRoot ->
  Infer.Context -> Infer.InferNode ->
  ( Bool
  , Infer.Context
  , Data.Expression DataIRef.DefinitionIRef (InferredWithConflicts a)
  )
inferWithConflicts loaded isNewRoot initialInferContext node =
  ( Map.null $ unConflictMap conflictsMap
  , resultInferContext
  , fmap toIWC exprInferred
  )
  where
    ((exprInferred, resultInferContext), conflictsMap) =
      runWriter $ Infer.inferLoaded (Infer.InferActions reportConflict) isNewRoot
      loaded initialInferContext node
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
  (Infer.Inferred a -> Data.Expression DataIRef.DefinitionIRef ()) ->
  (InferredWithConflicts a -> [Infer.Error]) ->
  InferredWithConflicts a -> [Data.Expression DataIRef.DefinitionIRef ()]
iwcInferredExprs getVal eeConflicts ee =
  (getVal . iwcInferred) ee :
  (Set.toList . Set.fromList . map (snd . Infer.errMismatch) . eeConflicts) ee

iwcInferredTypes :: InferredWithConflicts a -> [Data.Expression DataIRef.DefinitionIRef ()]
iwcInferredTypes = iwcInferredExprs Infer.iType iwcTypeConflicts

iwcInferredValues :: InferredWithConflicts a -> [Data.Expression DataIRef.DefinitionIRef ()]
iwcInferredValues = iwcInferredExprs Infer.iValue iwcValueConflicts
