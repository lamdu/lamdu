{-# LANGUAGE DeriveFunctor #-}
module Editor.DataLoad
  ( Entity(..), loadExpression
  )
where

import Control.Monad (liftM, liftM2, (<=<))
import Data.Binary (Binary)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.MonadF (MonadToFunctor(..))
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data

-- Pure alternative for IRef
data Entity a = Entity
  { entityGuid :: Guid
  , entityValue :: a
  }
  deriving (Functor)

sequenceEntity :: Functor f => Entity (f a) -> f (Entity a)
sequenceEntity (Entity guid value) = fmap (Entity guid) value

loadWrapped
  :: (Monad m, Binary (e IRef))
  => (e IRef -> Transaction t m (e Entity))
  -> IRef (e IRef) -> Transaction t m (Entity (e Entity))
loadWrapped f =
  unMonadToFunctor . sequenceEntity . fmap (MonadToFunctor . f) <=< loadItem

loadItem :: (Monad m, Binary a) => IRef a -> Transaction t m (Entity a)
loadItem i = do
  value <- Transaction.readIRef i
  return Entity { entityGuid = IRef.guid i, entityValue = value }

loadLambda :: Monad m => Data.Lambda IRef -> Transaction t m (Data.Lambda Entity)
loadLambda (Data.Lambda paramType body) =
  liftM2 Data.Lambda (loadWrapped loadExpression paramType) (loadWrapped loadExpression body)

loadApply :: Monad m => Data.Apply IRef -> Transaction t m (Data.Apply Entity)
loadApply (Data.Apply funcI argI) =
  liftM2 Data.Apply (loadWrapped loadExpression funcI) (loadWrapped loadExpression argI)

loadDefinition :: Monad m => Data.Definition IRef -> Transaction t m (Data.Definition Entity)
loadDefinition (Data.Definition defBody) =
  liftM Data.Definition $ loadWrapped loadExpression defBody

loadVariable :: Monad m => Data.VariableRef IRef -> Transaction t m (Data.VariableRef Entity)
loadVariable (Data.ParameterRef x) = liftM Data.ParameterRef $ loadWrapped loadExpression x
loadVariable (Data.DefinitionRef x) = liftM Data.DefinitionRef $ loadWrapped loadDefinition x
loadVariable (Data.BuiltinRef x) = liftM Data.BuiltinRef $ loadItem x

loadExpression :: Monad m => Data.Expression IRef -> Transaction t m (Data.Expression Entity)
loadExpression (Data.ExpressionLambda x) = liftM Data.ExpressionLambda $ loadLambda x
loadExpression (Data.ExpressionApply x) = liftM Data.ExpressionApply $ loadApply x
loadExpression (Data.ExpressionGetVariable x) = liftM Data.ExpressionGetVariable $ loadVariable x
loadExpression Data.ExpressionHole = return Data.ExpressionHole
loadExpression (Data.ExpressionLiteralInteger x) = return $ Data.ExpressionLiteralInteger x
