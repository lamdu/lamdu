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
import qualified Editor.Data.Infer as Infer

data InferredWithConflicts a = InferredWithConflicts
  { iwcInferred :: Infer.Inferred a
  , iwcTypeConflicts :: [Data.PureExpression Data.DefinitionIRef]
  , iwcValueConflicts :: [Data.PureExpression Data.DefinitionIRef]
  } deriving (Functor, Foldable, Traversable)
derive makeBinary ''InferredWithConflicts

newtype ConflictMap =
  ConflictMap { unConflictMap :: Map Infer.Ref (Set (Data.PureExpression Data.DefinitionIRef)) }

instance Monoid ConflictMap where
  mempty = ConflictMap mempty
  mappend (ConflictMap x) (ConflictMap y) =
    ConflictMap $ Map.unionWith mappend x y

getConflicts :: Infer.Ref -> ConflictMap -> [Data.PureExpression Data.DefinitionIRef]
getConflicts ref = maybe [] Set.toList . Map.lookup ref . unConflictMap

reportConflict :: Infer.Error -> Writer ConflictMap ()
reportConflict err =
  Writer.tell . ConflictMap .
  Map.singleton (Infer.errRef err) .
  Set.singleton .
  snd $ Infer.errMismatch err

inferWithConflicts ::
  Infer.Loaded a ->
  Infer.RefMap -> Infer.InferNode ->
  ( Bool
  , Infer.RefMap
  , Data.Expression Data.DefinitionIRef (InferredWithConflicts a)
  )
inferWithConflicts loaded refMap node =
  ( Map.null $ unConflictMap conflictsMap
  , inferContext
  , fmap toIWC exprInferred
  )
  where
    ((exprInferred, inferContext), conflictsMap) =
      runWriter $ Infer.infer (Infer.InferActions reportConflict)
      loaded refMap node
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
  (Infer.Inferred a -> b) ->
  (InferredWithConflicts a -> [b]) ->
  InferredWithConflicts a -> [b]
iwcInferredExprs getVal eeConflicts ee =
  getVal (iwcInferred ee) : eeConflicts ee

iwcInferredTypes :: InferredWithConflicts a -> [Data.PureExpression Data.DefinitionIRef]
iwcInferredTypes = iwcInferredExprs Infer.iType iwcTypeConflicts

iwcInferredValues :: InferredWithConflicts a -> [Data.PureExpression Data.DefinitionIRef]
iwcInferredValues = iwcInferredExprs Infer.iValue iwcValueConflicts
