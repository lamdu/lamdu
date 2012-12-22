{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Expression.Load
  ( LoadedClosure, Loaded
  , loadDefinition, loadDefinitionClosure
  , loadExpressionClosure, loadExpressionProperty

  , PropertyClosure, propertyOfClosure, irefOfClosure
  ) where

import Control.Applicative (liftA2)
import Control.Lens ((^.), SimpleLensLike)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Function (on)
import Data.Store.IRef (Tag)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import Lamdu.Data.Definition (Definition(..))
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens as Lens
import qualified Control.Lens.Internal as LensInternal
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef

type T = Transaction

data ApplyRole = Func | Arg
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''ApplyRole
applyChildByRole :: Functor f => ApplyRole -> SimpleLensLike f (Expression.Apply a) a
applyChildByRole Func = Expression.applyFunc
applyChildByRole Arg = Expression.applyArg

data LambdaRole = ParamType | Result
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''LambdaRole
lambdaChildByRole :: Functor f => LambdaRole -> SimpleLensLike f (Expression.Lambda a) a
lambdaChildByRole ParamType = Expression.lambdaParamType
lambdaChildByRole Result = Expression.lambdaBody

data PropertyClosure t
  = DefinitionTypeProperty
      (DefI t) (Definition (DataIRef.ExpressionI t))
  | DefinitionBodyExpressionProperty
      (DefI t) (DataIRef.ExpressionI t) (DataIRef.ExpressionI t)
  | ApplyProperty
      (DataIRef.ExpressionI t) (Expression.Apply (DataIRef.ExpressionI t)) ApplyRole
  | LambdaProperty Expression.LambdaWrapper
      (DataIRef.ExpressionI t) (Expression.Lambda (DataIRef.ExpressionI t)) LambdaRole
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''PropertyClosure

setter :: Lens.LensLike (LensInternal.Context a b) s t a b -> s -> b -> t
setter = flip . Lens.set . Lens.cloneLens

propertyOfClosure :: MonadA m => PropertyClosure (Tag m) -> DataIRef.ExpressionProperty m
propertyOfClosure (DefinitionTypeProperty defI (Definition defBody defType)) =
  Property defType $
  Transaction.writeIRef defI . Definition defBody
propertyOfClosure (DefinitionBodyExpressionProperty defI bodyExpr defType) =
  Property bodyExpr $
  Transaction.writeIRef defI . (`Definition` defType) .
  Definition.BodyExpression
propertyOfClosure (ApplyProperty exprI apply role) =
  Property (apply ^. Lens.cloneLens lens) $
  DataIRef.writeExprBody exprI . Expression.BodyApply .
  setter lens apply
  where
    lens = applyChildByRole role
propertyOfClosure (LambdaProperty cons exprI lambda role) =
  Property (lambda ^. Lens.cloneLens lens) $
  DataIRef.writeExprBody exprI . Lens.review (Expression.lambdaWrapperPrism cons) .
  setter lens lambda
  where
    lens = lambdaChildByRole role

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
  MonadA m => DataIRef.ExpressionI (Tag m) -> T m (Expression.Body (DefI (Tag m)) (LoadedClosure (Tag m)))
loadExpressionBody iref = onBody =<< DataIRef.readExprBody iref
  where
    onBody (Expression.BodyLeaf x) =
      return $ Expression.BodyLeaf x
    onBody (Expression.BodyApply apply) =
      on (liftA2 Expression.makeApply) loadExpressionClosure (prop Func) (prop Arg)
      where
        prop = ApplyProperty iref apply
    onBody (Expression.BodyLambda lambda) = onLambda Expression.LambdaWrapperLambda lambda
    onBody (Expression.BodyPi lambda) = onLambda Expression.LambdaWrapperPi lambda
    onLambda cons lambda@(Expression.Lambda param _ _) =
      fmap (Lens.review (Expression.lambdaWrapperPrism cons)) $
      on (liftA2 (Expression.Lambda param)) loadExpressionClosure
      (prop ParamType) (prop Result)
      where
        prop = LambdaProperty cons iref lambda

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
