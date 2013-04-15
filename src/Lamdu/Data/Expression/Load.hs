{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Expression.Load
  ( LoadedClosure, Loaded
  , loadDefinition, loadDefinitionClosure
  , loadExpressionClosure, loadExpressionProperty

  , PropertyClosure, propertyOfClosure, irefOfClosure
  ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Lens (LensLike')
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.IRef (Tag)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import Lamdu.Data.Definition (Definition(..))
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type T = Transaction

data ApplyRole = Func | Arg
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''ApplyRole
applyChildByRole :: Functor f => ApplyRole -> LensLike' f (Expression.Apply a) a
applyChildByRole Func = Expression.applyFunc
applyChildByRole Arg = Expression.applyArg

data LambdaRole = ParamType | Result
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''LambdaRole
lambdaChildByRole :: Functor f => LambdaRole -> LensLike' f (Expression.Lambda a) a
lambdaChildByRole ParamType = Expression.lambdaParamType
lambdaChildByRole Result = Expression.lambdaResult

data PropertyClosure t
  = DefinitionTypeProperty
      (DefI t) (Definition (DataIRef.ExpressionI t))
  | DefinitionBodyExpressionProperty
      (DefI t) (DataIRef.ExpressionI t) (DataIRef.ExpressionI t)
  | ApplyProperty
      (DataIRef.ExpressionI t) (Expression.Apply (DataIRef.ExpressionI t)) ApplyRole
  | LambdaProperty
      (DataIRef.ExpressionI t) (Expression.Lambda (DataIRef.ExpressionI t)) LambdaRole
  | RecordProperty
      (DataIRef.ExpressionI t) (Expression.Record (DataIRef.ExpressionI t)) Expression.Field
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''PropertyClosure

setter :: Lens.ALens s t dummy b -> s -> b -> t
setter = flip . Lens.set . Lens.cloneLens

assocListAt :: (Applicative f, Eq k) => k -> LensLike' f [(k, a)] a
assocListAt key = Lens.traverse . Lens.filtered ((== key) . fst) . Lens._2

propertyOfClosure :: MonadA m => PropertyClosure (Tag m) -> DataIRef.ExpressionProperty m
propertyOfClosure (DefinitionTypeProperty defI (Definition defBody defType)) =
  Property defType (Transaction.writeIRef defI . Definition defBody)
propertyOfClosure (DefinitionBodyExpressionProperty defI bodyExpr defType) =
  Property bodyExpr
  (Transaction.writeIRef defI . (`Definition` defType) . Definition.BodyExpression)
propertyOfClosure (ApplyProperty exprI apply role) =
  Property (apply ^. Lens.cloneLens lens)
  (DataIRef.writeExprBody exprI . Expression.BodyApply . setter lens apply)
  where
    lens = applyChildByRole role
propertyOfClosure (LambdaProperty exprI lambda role) =
  Property (lambda ^. Lens.cloneLens lens)
  (DataIRef.writeExprBody exprI . Expression.BodyLam . setter lens lambda)
  where
    lens = lambdaChildByRole role
propertyOfClosure (RecordProperty exprI record field) =
  Property (record ^?! lens0)
  (DataIRef.writeExprBody exprI . Expression.BodyRecord . (flip . Lens.set) lens1 record)
  where
    -- TODO: Why does Lens.cloneLens not work?
    lens0 = Expression.recordFields . assocListAt field
    lens1 = Expression.recordFields . assocListAt field

irefOfClosure :: MonadA m => PropertyClosure (Tag m) -> DataIRef.ExpressionI (Tag m)
irefOfClosure = Property.value . propertyOfClosure

type LoadedClosure t = DataIRef.Expression t (PropertyClosure t)
type Loaded m = DataIRef.ExpressionM m (DataIRef.ExpressionProperty m)

loadExpressionProperty ::
  MonadA m => DataIRef.ExpressionProperty m -> T m (Loaded m)
loadExpressionProperty prop =
  fmap ((`Expression.Expression` prop) . (fmap . fmap) propertyOfClosure) .
  loadExpressionBody $ Property.value prop

loadExpressionClosure :: MonadA m => PropertyClosure (Tag m) -> T m (LoadedClosure (Tag m))
loadExpressionClosure closure =
  fmap (`Expression.Expression` closure) . loadExpressionBody $
  irefOfClosure closure

loadExpressionBody ::
  MonadA m =>
  DataIRef.ExpressionI (Tag m) ->
  T m (Expression.Body (DefI (Tag m)) (LoadedClosure (Tag m)))
loadExpressionBody iref =
  onBody =<< DataIRef.readExprBody iref
  where
    onBody (Expression.BodyLeaf x) = return $ Expression.BodyLeaf x
    onBody (Expression.BodyApply apply) =
      ExprUtil.makeApply <$> loadRole Func <*> loadRole Arg
      where
        loadRole = loadExpressionClosure . ApplyProperty iref apply
    onBody (Expression.BodyLam lambda@(Expression.Lambda k param _ _)) =
      ExprUtil.makeLam k param <$> loadRole ParamType <*> loadRole Result
      where
        loadRole = loadExpressionClosure . LambdaProperty iref lambda
    onBody (Expression.BodyRecord record@(Expression.Record k fields)) =
      Expression.BodyRecord . Expression.Record k <$>
      Lens.traverse loadField fields
      where
        loadField (field, _) =
          (,) field <$> loadExpressionClosure (RecordProperty iref record field)

loadDefinition :: MonadA m => DefI (Tag m) -> T m (Definition (Loaded m))
loadDefinition x = (fmap . fmap . fmap) propertyOfClosure . loadDefinitionClosure $ x

loadDefinitionClosure :: MonadA m => DefI (Tag m) -> T m (Definition (LoadedClosure (Tag m)))
loadDefinitionClosure defI = do
  def <- Transaction.readIRef defI
  defType <- loadExpressionClosure $ DefinitionTypeProperty defI def
  fmap (`Definition` defType) $
    case def ^. Definition.defBody of
    Definition.BodyExpression bodyI ->
      fmap Definition.BodyExpression . loadExpressionClosure $
      DefinitionBodyExpressionProperty defI bodyI $ def ^. Definition.defType
    Definition.BodyBuiltin (Definition.Builtin name) ->
      return . Definition.BodyBuiltin $ Definition.Builtin name
