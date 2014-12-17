{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Expr.IRef
  ( ValI(..)
  , ValBody
  , ValIProperty
  , Lam, Apply
  , newValBody, readValBody, writeValBody
  , newLambda
  , newVal, writeVal, readVal
  , writeValWithStoredSubexpressions
  , DefI
  , addProperties

  , globalId, defI

  , ValTree(..), ValTreeM, writeValTree
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Function.Decycle (decycle)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Expr.Identifier (Identifier(..))
import Lamdu.Expr.Val (Val(..))
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.Val as V

type T = Transaction

type DefI m = IRef m (Definition.Body (ValI m))

-- NOTE: Nobody else should generate Lamdu-visible Global Id's
globalId :: DefI m -> V.GlobalId
globalId = V.GlobalId . Identifier . Guid.bs . IRef.guid

defI :: V.GlobalId -> DefI m
defI (V.GlobalId (Identifier bs)) = IRef.unsafeFromGuid $ Guid.make bs

newtype ValI m = ValI
  { unValI :: IRef m (V.Body (ValI m))
  } deriving (Eq, Ord, Show, Binary)

type ValIProperty m = Property (T m) (ValI m)
type ValBody m = V.Body (ValI m)
type Lam m = V.Lam (ValI m)
type Apply m = V.Apply (ValI m)

newValBody :: MonadA m => ValBody m -> T m (ValI m)
newValBody = fmap ValI . Transaction.newIRef

-- TODO: Remove this
newLambda :: MonadA m => ValI m -> T m (V.Var, ValI m)
newLambda body = do
  paramId <- V.Var . Identifier . Guid.bs <$> Transaction.newKey
  expr <- newValBody $ V.BAbs $ V.Lam paramId body
  return (paramId, expr)

readValBody :: MonadA m => ValI m -> T m (ValBody m)
readValBody = Transaction.readIRef . unValI

writeValBody ::
  MonadA m => ValI m -> ValBody m -> T m ()
writeValBody = Transaction.writeIRef . unValI

newVal :: MonadA m => Val () -> T m (ValI m)
newVal = fmap (^. V.payload . Lens._1) . newValFromH . ((,) Nothing <$>)

-- Returns expression with new Guids
writeVal ::
  MonadA m =>
  ValI m -> Val a ->
  T m (Val (ValI m, a))
writeVal iref =
  writeValWithStoredSubexpressions iref .
  fmap ((,) Nothing)

writeValWithStoredSubexpressions ::
  MonadA m => ValI m -> Val (Maybe (ValI m), a) -> T m (Val (ValI m, a))
writeValWithStoredSubexpressions iref expr = do
  exprBodyP <- expressionBodyFrom expr
  exprBodyP
    <&> (^. V.payload . Lens._1)
    & writeValBody iref
  return $ Val (iref, expr ^. V.payload . Lens._2) exprBodyP

readVal ::
  MonadA m => ValI m -> T m (Val (ValI m))
readVal =
  decycle loop
  where
    loop Nothing valI = error $ "Recursive reference: " ++ show valI
    loop (Just go) valI =
      fmap (Val valI) .
      traverse go =<< readValBody valI

expressionBodyFrom ::
  MonadA m =>
  Val (Maybe (ValI m), a) ->
  T m (V.Body (Val (ValI m, a)))
expressionBodyFrom = traverse newValFromH . (^. V.body)

newValFromH ::
  MonadA m =>
  Val (Maybe (ValI m), a) ->
  T m (Val (ValI m, a))
newValFromH expr =
  case mIRef of
  Just iref -> writeValWithStoredSubexpressions iref expr
  Nothing -> do
    body <- expressionBodyFrom expr
    exprI <-
      body
      <&> (^. V.payload . Lens._1)
      & Transaction.newIRef
    return $ Val (ValI exprI, pl) body
  where
    (mIRef, pl) = expr ^. V.payload

addProperties ::
  MonadA m =>
  (ValI m -> T m ()) ->
  Val (ValI m, a) ->
  Val (ValIProperty m, a)
addProperties setIRef (Val (iref, a) body) =
  Val (Property iref setIRef, a) (body & Lens.traversed %@~ f)
  where
    f index =
      addProperties $ \newIRef ->
      body
      <&> (^. V.payload . Lens._1) -- convert to body of IRefs
      & Lens.element index .~ newIRef
      & writeValBody iref

data ValTree m
  = ValTreeLeaf (ValI m)
  | ValTreeNode (V.Body (ValTree m))
  deriving (Show)
type ValTreeM m = ValTree m

writeValTree :: MonadA m => ValTreeM m -> T m (ValI m)
writeValTree (ValTreeLeaf valI) = return valI
writeValTree (ValTreeNode body) = newValBody =<< traverse writeValTree body
