{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Load
  ( LoadedClosure, Loaded
  , loadDefinition, loadDefinitionClosure
  , loadExpressionClosure, loadExpressionProperty

  , PropertyClosure, propertyOfClosure, irefOfClosure
  ) where

import Control.Applicative (liftA2, (<$>), (<*>))
import Control.Lens ((^.), SimpleLensLike)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (sequenceA_)
import Data.Function (on)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import Lamdu.Data.IRef (DefI)
import qualified Control.Lens as Lens
import qualified Control.Lens.Internal as LensInternal
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef

type T = Transaction

data ApplyRole = Func | Arg
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''ApplyRole
applyChildByRole :: Functor f => ApplyRole -> SimpleLensLike f (Data.Apply a) a
applyChildByRole Func = Data.applyFunc
applyChildByRole Arg = Data.applyArg

data LambdaRole = ParamType | Result
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''LambdaRole
lambdaChildByRole :: Functor f => LambdaRole -> SimpleLensLike f (Data.Lambda a) a
lambdaChildByRole ParamType = Data.lambdaParamType
lambdaChildByRole Result = Data.lambdaBody

data PropertyClosure t
  = DefinitionTypeProperty
      (DefI t) (Data.Definition (DataIRef.Expression t))
  | DefinitionBodyExpressionProperty
      (DefI t) (DataIRef.Expression t) (DataIRef.Expression t)
  | ApplyProperty
      (DataIRef.Expression t) (Data.Apply (DataIRef.Expression t)) ApplyRole
  | LambdaProperty Data.ExprLambdaWrapper
      (DataIRef.Expression t) (Data.Lambda (DataIRef.Expression t)) LambdaRole
  deriving (Eq, Ord, Show, Typeable)
instance Binary (PropertyClosure t) where
  get = do
    tag <- get
    case tag of
      'T' -> DefinitionTypeProperty <$> get <*> get
      'B' -> DefinitionBodyExpressionProperty <$> get <*> get <*> get
      'A' -> ApplyProperty <$> get <*> get <*> get
      'L' -> LambdaProperty <$> get <*> get <*> get <*> get
      _ -> error $ "Bad Binary encoding: " ++ show tag
  put (DefinitionTypeProperty a b) = sequenceA_ [put 'T', put a, put b]
  put (DefinitionBodyExpressionProperty a b c) = sequenceA_ [put 'B', put a, put b, put c]
  put (ApplyProperty a b c) = sequenceA_ [put 'A', put a, put b, put c]
  put (LambdaProperty a b c d) = sequenceA_ [put 'L', put a, put b, put c, put d]

setter :: Lens.LensLike (LensInternal.Context a b) s t a b -> s -> b -> t
setter = flip . Lens.set . Lens.cloneLens

propertyOfClosure :: MonadA m => PropertyClosure (m ()) -> DataIRef.ExpressionProperty m
propertyOfClosure (DefinitionTypeProperty defI (Data.Definition defBody defType)) =
  Property defType $
  Transaction.writeIRef defI . Data.Definition defBody
propertyOfClosure (DefinitionBodyExpressionProperty defI bodyExpr defType) =
  Property bodyExpr $
  Transaction.writeIRef defI . (`Data.Definition` defType) .
  Data.DefinitionExpression
propertyOfClosure (ApplyProperty exprI apply role) =
  Property (apply ^. Lens.cloneLens lens) $
  DataIRef.writeExprBody exprI . Data.ExpressionApply .
  setter lens apply
  where
    lens = applyChildByRole role
propertyOfClosure (LambdaProperty cons exprI lambda role) =
  Property (lambda ^. Lens.cloneLens lens) $
  DataIRef.writeExprBody exprI . Data.exprLambdaCons cons .
  setter lens lambda
  where
    lens = lambdaChildByRole role

irefOfClosure :: MonadA m => PropertyClosure (m ()) -> DataIRef.Expression (m ())
irefOfClosure = Property.value . propertyOfClosure

type LoadedClosure t = Data.Expression (DefI t) (PropertyClosure t)
type Loaded m = Data.Expression (DefI (m ())) (DataIRef.ExpressionProperty m)

loadExpressionProperty ::
  MonadA m => DataIRef.ExpressionProperty m -> T m (Loaded m)
loadExpressionProperty prop =
  fmap ((`Data.Expression` prop) . (fmap . fmap) propertyOfClosure) .
  loadExpressionBody $ Property.value prop

loadExpressionClosure :: MonadA m => PropertyClosure (m ()) -> T m (LoadedClosure (m ()))
loadExpressionClosure closure =
  fmap (`Data.Expression` closure) . loadExpressionBody $
  irefOfClosure closure

loadExpressionBody ::
  MonadA m => DataIRef.Expression (m ()) -> T m (Data.ExpressionBody (DefI (m ())) (LoadedClosure (m ())))
loadExpressionBody iref = onBody =<< DataIRef.readExprBody iref
  where
    onBody (Data.ExpressionLeaf x) =
      return $ Data.ExpressionLeaf x
    onBody (Data.ExpressionApply apply) =
      on (liftA2 Data.makeApply) loadExpressionClosure (prop Func) (prop Arg)
      where
        prop = ApplyProperty iref apply
    onBody (Data.ExpressionLambda lambda) = onLambda Data.ExprLambda lambda
    onBody (Data.ExpressionPi lambda) = onLambda Data.ExprPi lambda
    onLambda cons lambda@(Data.Lambda param _ _) =
      fmap (Data.exprLambdaCons cons) $
      on (liftA2 (Data.Lambda param)) loadExpressionClosure
      (prop ParamType) (prop Result)
      where
        prop = LambdaProperty cons iref lambda

loadDefinition :: MonadA m => DefI (m ()) -> T m (Data.Definition (Loaded m))
loadDefinition x = (fmap . fmap . fmap) propertyOfClosure . loadDefinitionClosure $ x

loadDefinitionClosure :: MonadA m => DefI (m ()) -> T m (Data.Definition (LoadedClosure (m ())))
loadDefinitionClosure defI = do
  def <- Transaction.readIRef defI
  defType <- loadExpressionClosure $ DefinitionTypeProperty defI def
  fmap (`Data.Definition` defType) $
    case def ^. Data.defBody of
    Data.DefinitionExpression bodyI ->
      fmap Data.DefinitionExpression . loadExpressionClosure $
      DefinitionBodyExpressionProperty defI bodyI $ def ^. Data.defType
    Data.DefinitionBuiltin (Data.Builtin name) ->
      return . Data.DefinitionBuiltin $ Data.Builtin name
