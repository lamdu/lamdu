{-# LANGUAGE TypeFamilies #-}
module Editor.DataLoad
  ( Entity(..), loadExpression
  )
where

import Control.Monad (liftM, liftM2)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Data (Expression(..), Apply(..), Lambda(..))
import qualified Data.Store.Transaction as Transaction

-- Have to use type families to avoid infinite kinds.
type family EntityToIRef a
type instance EntityToIRef (a Entity) = a IRef

-- Pure alternative for IRef
data Entity a = Entity
  { entityIRef :: IRef (EntityToIRef a)
  , entityValue :: a
  }

loadExpression :: Monad m => IRef (Expression IRef) -> Transaction t m (Entity (Expression Entity))
loadExpression ref = do
  expr <- Transaction.readIRef ref
  liftM (Entity ref) $ case expr of
    ExpressionLambda lambda -> loadLambda ExpressionLambda lambda
    ExpressionPi lambda -> loadLambda ExpressionPi lambda
    ExpressionApply (Apply x y) -> liftM ExpressionApply $ liftM2 Apply (loadExpression x) (loadExpression y)
    ExpressionGetVariable x -> return $ ExpressionGetVariable x
    ExpressionHole -> return ExpressionHole
    ExpressionLiteralInteger x -> return $ ExpressionLiteralInteger x
  where
    loadLambda f (Lambda x y) = liftM f $ liftM2 Lambda (loadExpression x) (loadExpression y)
