{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Expression.Load
  ( LoadedClosure, Loaded
  , loadDefinition, loadDefinitionClosure
  , loadExpressionClosure, loadExpressionProperty

  , PropertyClosure, propertyOfClosure, irefOfClosure
  ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Lens (LensLike', Traversal', Lens')
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
      (DataIRef.ExpressionI t) (Expression.Record (DataIRef.ExpressionI t)) Int
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''PropertyClosure

propertyOfClosure :: MonadA m => PropertyClosure (Tag m) -> DataIRef.ExpressionProperty m
propertyOfClosure (DefinitionTypeProperty defI (Definition defBody defType)) =
  Property defType (Transaction.writeIRef defI . Definition defBody)
propertyOfClosure (DefinitionBodyExpressionProperty defI bodyExpr defType) =
  Property bodyExpr
  (Transaction.writeIRef defI . (`Definition` defType) . Definition.BodyExpression)
propertyOfClosure (ApplyProperty exprI apply role) =
  Property (apply ^. lens)
  (DataIRef.writeExprBody exprI . Expression.BodyApply . flip (Lens.set lens) apply)
  where
    lens :: Lens' (Expression.Apply expr) expr
    lens = applyChildByRole role
propertyOfClosure (LambdaProperty exprI lambda role) =
  Property (lambda ^. lens)
  (DataIRef.writeExprBody exprI . Expression.BodyLam . flip (Lens.set lens) lambda)
  where
    lens :: Lens' (Expression.Lambda expr) expr
    lens = lambdaChildByRole role
propertyOfClosure (RecordProperty exprI record idx) =
  Property (record ^?! lens)
  (DataIRef.writeExprBody exprI . Expression.BodyRecord . (flip . Lens.set) lens record)
  where
    lens :: Traversal' (Expression.Record expr) expr
    lens = Expression.recordFields . Lens.ix idx . Lens._2

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
      Lens.itraverse loadField fields
      where
        loadField idx (field, _) =
          (,) field <$> loadExpressionClosure (RecordProperty iref record idx)

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
