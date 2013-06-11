{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Expression.Load
  ( LoadedClosure, Loaded
  , loadDefinition, loadDefinitionClosure
  , loadExpressionProperty

  , PropertyClosure, propertyOfClosure, irefOfClosure
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import Lamdu.Data.Definition (Definition(..))
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef

type T = Transaction

type ExprI = ExprIRef.ExpressionI

-- | SubexpressionIndex is a Foldable-index into Expr.Body (i.e:
-- 0 or 1 for BodyApply func/arg)
type SubexpressionIndex = Int

data PropertyClosure t
  = DefinitionTypeProperty (DefI t) (Definition (ExprI t))
  | DefinitionBodyExpressionProperty (DefI t) (ExprI t) (ExprI t)
  | SubexpressionProperty (ExprI t) (Expr.Body (DefI t) (ExprI t)) SubexpressionIndex
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''PropertyClosure

propertyOfClosure :: MonadA m => PropertyClosure (Tag m) -> ExprIRef.ExpressionProperty m
propertyOfClosure (DefinitionTypeProperty defI (Definition defBody defType)) =
  Property defType (Transaction.writeIRef defI . Definition defBody)
propertyOfClosure (DefinitionBodyExpressionProperty defI bodyExpr defType) =
  Property bodyExpr
  (Transaction.writeIRef defI . (`Definition` defType) . Definition.BodyExpression)
propertyOfClosure (SubexpressionProperty exprI body index) =
  Property (body ^?! lens)
  (ExprIRef.writeExprBody exprI . flip (lens .~) body)
  where
    lens :: Traversable t => Lens.IndexedTraversal' Int (t a) a
    lens = Lens.element index

irefOfClosure :: MonadA m => PropertyClosure (Tag m) -> ExprI (Tag m)
irefOfClosure = Property.value . propertyOfClosure

type LoadedClosure t = ExprIRef.Expression t (PropertyClosure t)
type Loaded m = ExprIRef.ExpressionM m (ExprIRef.ExpressionProperty m)

loadExpressionProperty ::
  MonadA m => ExprIRef.ExpressionProperty m -> T m (Loaded m)
loadExpressionProperty prop =
  fmap ((`Expr.Expression` prop) . (fmap . fmap) propertyOfClosure) .
  loadExpressionBody Set.empty $ Property.value prop

loadExpressionClosure ::
  MonadA m => Set Guid ->
  PropertyClosure (Tag m) -> T m (LoadedClosure (Tag m))
loadExpressionClosure visited closure =
  fmap (`Expr.Expression` closure) . loadExpressionBody visited $
  irefOfClosure closure

loadExpressionBody ::
  MonadA m => Set Guid -> ExprI (Tag m) ->
  T m (Expr.Body (DefI (Tag m)) (LoadedClosure (Tag m)))
loadExpressionBody visited iref
  | ourGuid `Set.member` visited = error "Recursive IRef structure"
  | otherwise = onBody =<< ExprIRef.readExprBody iref
  where
    ourGuid = ExprIRef.exprGuid iref
    newVisited = Set.insert ourGuid visited
    onBody body =
      Lens.itraverseOf (Lens.indexing Lens.traverse) (loadElement body) body
    loadElement body i _ = loadExpressionClosure newVisited $ SubexpressionProperty iref body i

loadDefinition :: MonadA m => DefI (Tag m) -> T m (Definition (Loaded m))
loadDefinition x = (fmap . fmap . fmap) propertyOfClosure . loadDefinitionClosure $ x

loadDefinitionClosure :: MonadA m => DefI (Tag m) -> T m (Definition (LoadedClosure (Tag m)))
loadDefinitionClosure defI = do
  def <- Transaction.readIRef defI
  defType <- loadExpressionClosure Set.empty $ DefinitionTypeProperty defI def
  fmap (`Definition` defType) $
    case def ^. Definition.defBody of
    Definition.BodyExpression bodyI ->
      fmap Definition.BodyExpression . loadExpressionClosure Set.empty $
      DefinitionBodyExpressionProperty defI bodyI $ def ^. Definition.defType
    Definition.BodyBuiltin (Definition.Builtin name) ->
      return . Definition.BodyBuiltin $ Definition.Builtin name
