{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Expression.Load
  ( loadDefinitionClosure
  , ExprPropertyClosure, exprPropertyOfClosure
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
import Lamdu.Data.Expression.IRef (DefI, DefIM)
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

data ExprPropertyClosure t
  = DefinitionTypeProperty (DefI t) (Definition (ExprI t))
  | DefinitionBodyExpressionProperty (DefI t) (ExprI t) (ExprI t)
  | SubexpressionProperty (ExprI t) (Expr.Body (DefI t) (ExprI t)) SubexpressionIndex
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''ExprPropertyClosure

exprPropertyOfClosure :: MonadA m => ExprPropertyClosure (Tag m) -> ExprIRef.ExpressionProperty m
exprPropertyOfClosure (DefinitionTypeProperty defI (Definition defBody defType)) =
  Property defType (Transaction.writeIRef defI . Definition defBody)
exprPropertyOfClosure (DefinitionBodyExpressionProperty defI bodyExpr defType) =
  Property bodyExpr
  (Transaction.writeIRef defI . (`Definition` defType) . Definition.BodyExpression)
exprPropertyOfClosure (SubexpressionProperty exprI body index) =
  Property (body ^?! lens)
  (ExprIRef.writeExprBody exprI . flip (lens .~) body)
  where
    lens :: Traversable t => Lens.IndexedTraversal' SubexpressionIndex (t a) a
    lens = Lens.element index

irefOfClosure :: MonadA m => ExprPropertyClosure (Tag m) -> ExprI (Tag m)
irefOfClosure = Property.value . exprPropertyOfClosure

loadExpressionClosure ::
  MonadA m => Set Guid ->
  ExprPropertyClosure (Tag m) ->
  T m (ExprIRef.ExpressionM m (ExprPropertyClosure (Tag m)))
loadExpressionClosure visited closure =
  fmap (`Expr.Expression` closure) . loadExpressionBody visited $
  irefOfClosure closure

loadExpressionBody ::
  MonadA m => Set Guid -> ExprI (Tag m) ->
  T m
  (Expr.BodyExpr (DefIM m) (ExprPropertyClosure (Tag m)))
loadExpressionBody visited iref
  | ourGuid `Set.member` visited = error "Recursive IRef structure"
  | otherwise = onBody =<< ExprIRef.readExprBody iref
  where
    ourGuid = ExprIRef.exprGuid iref
    newVisited = Set.insert ourGuid visited
    onBody body = body & Lens.traversed %%@~ loadElement body
    loadElement body i _ = loadExpressionClosure newVisited $ SubexpressionProperty iref body i

-- TODO: Return DefinitionClosure
loadDefinitionClosure ::
  MonadA m => DefIM m ->
  T m (Definition (ExprIRef.ExpressionM m (ExprPropertyClosure (Tag m))))
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
