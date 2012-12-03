{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Editor.Data.Load
  ( LoadedClosure, Loaded
  , loadDefinition, loadDefinitionClosure
  , loadExpressionClosure, loadExpressionProperty

  , PropertyClosure, propertyOfClosure, irefOfClosure
  ) where

import Control.Lens ((^.), SimpleLensLike)
import Control.Monad (liftM, liftM2)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Control.Lens.Internal as LensInternal
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef

type T = Transaction
type DefI = DataIRef.DefinitionIRef

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

data PropertyClosure
  = DefinitionTypeProperty
      DefI (Data.Definition DataIRef.Expression)
  | DefinitionBodyExpressionProperty
      DefI DataIRef.Expression DataIRef.Expression
  | ApplyProperty
      DataIRef.Expression (Data.Apply DataIRef.Expression) ApplyRole
  | LambdaProperty Data.ExprLambdaWrapper
      DataIRef.Expression (Data.Lambda DataIRef.Expression) LambdaRole
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''PropertyClosure

setter :: Lens.LensLike (LensInternal.Context a b) s t a b -> s -> b -> t
setter = flip . Lens.set . Lens.cloneLens

propertyOfClosure :: Monad m => PropertyClosure -> DataIRef.ExpressionProperty (T m)
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

irefOfClosure :: PropertyClosure -> DataIRef.Expression
irefOfClosure closure =
  Property.value
  -- Yuck: We don't care about the setter, but we must provide a Monad
  -- instance for it, so we just hard-code it to Identity and ignore
  -- it.
  (propertyOfClosure closure :: DataIRef.ExpressionProperty (T Identity))

type LoadedClosure = Data.Expression DefI PropertyClosure
type Loaded m = Data.Expression DefI (DataIRef.ExpressionProperty m)

loadExpressionProperty ::
  Monad m => DataIRef.ExpressionProperty (T m) -> T m (Loaded (T m))
loadExpressionProperty prop =
  liftM ((`Data.Expression` prop) . (fmap . fmap) propertyOfClosure) .
  loadExpressionBody $ Property.value prop

loadExpressionClosure :: Monad m => PropertyClosure -> T m LoadedClosure
loadExpressionClosure closure =
  liftM (`Data.Expression` closure) . loadExpressionBody $
  irefOfClosure closure

loadExpressionBody ::
  Monad m => DataIRef.Expression -> T m (Data.ExpressionBody DefI LoadedClosure)
loadExpressionBody iref = onBody =<< DataIRef.readExprBody iref
  where
    onBody (Data.ExpressionLeaf x) =
      return $ Data.ExpressionLeaf x
    onBody (Data.ExpressionApply apply) =
      on (liftM2 Data.makeApply) loadExpressionClosure (prop Func) (prop Arg)
      where
        prop = ApplyProperty iref apply
    onBody (Data.ExpressionLambda lambda) = onLambda Data.ExprLambda lambda
    onBody (Data.ExpressionPi lambda) = onLambda Data.ExprPi lambda
    onLambda cons lambda@(Data.Lambda param _ _) =
      liftM (Data.exprLambdaCons cons) $
      on (liftM2 (Data.Lambda param)) loadExpressionClosure
      (prop ParamType) (prop Result)
      where
        prop = LambdaProperty cons iref lambda

loadDefinition ::
  (Monad m, Monad n) => DefI -> T m (Data.Definition (Loaded (T n)))
loadDefinition = (liftM . fmap . fmap) propertyOfClosure . loadDefinitionClosure

loadDefinitionClosure :: Monad m => DefI -> T m (Data.Definition LoadedClosure)
loadDefinitionClosure defI = do
  def <- Transaction.readIRef defI
  defType <- loadExpressionClosure $ DefinitionTypeProperty defI def
  liftM (`Data.Definition` defType) $
    case def ^. Data.defBody of
    Data.DefinitionExpression bodyI ->
      liftM Data.DefinitionExpression . loadExpressionClosure $
      DefinitionBodyExpressionProperty defI bodyI $ def ^. Data.defType
    Data.DefinitionBuiltin (Data.Builtin name) ->
      return . Data.DefinitionBuiltin $ Data.Builtin name
