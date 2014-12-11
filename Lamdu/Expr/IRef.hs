{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Expr.IRef
  ( ValI(..), ValIM
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

type ValIM m = ValI m

type ValIProperty m = Property (T m) (ValIM m)
type ValBody m = V.Body (ValI m)
type Lam m = V.Lam (ValI m)
type Apply m = V.Apply (ValI m)

newValBody :: MonadA m => ValBody m -> T m (ValIM m)
newValBody = fmap ValI . Transaction.newIRef

-- TODO: Remove this
newLambda :: MonadA m => ValIM m -> T m (V.Var, ValIM m)
newLambda body = do
  paramId <- V.Var . Identifier . Guid.bs <$> Transaction.newKey
  expr <- newValBody $ V.BAbs $ V.Lam paramId body
  return (paramId, expr)

readValBody :: MonadA m => ValIM m -> T m (ValBody m)
readValBody = Transaction.readIRef . unValI

writeValBody ::
  MonadA m => ValIM m -> ValBody m -> T m ()
writeValBody = Transaction.writeIRef . unValI

newVal :: MonadA m => Val () -> T m (ValIM m)
newVal = fmap (^. V.payload . Lens._1) . newValFromH . ((,) Nothing <$>)

-- Returns expression with new Guids
writeVal ::
  MonadA m =>
  ValIM m -> Val a ->
  T m (Val (ValIM m, a))
writeVal iref =
  writeValWithStoredSubexpressions iref .
  fmap ((,) Nothing)

writeValWithStoredSubexpressions ::
  MonadA m => ValIM m -> Val (Maybe (ValIM m), a) -> T m (Val (ValIM m, a))
writeValWithStoredSubexpressions iref expr = do
  exprBodyP <- expressionBodyFrom expr
  exprBodyP
    <&> (^. V.payload . Lens._1)
    & writeValBody iref
  return $ Val (iref, expr ^. V.payload . Lens._2) exprBodyP

readVal ::
  MonadA m => ValIM m -> T m (Val (ValIM m))
readVal exprI =
  fmap (Val exprI) .
  traverse readVal =<< readValBody exprI

expressionBodyFrom ::
  MonadA m =>
  Val (Maybe (ValIM m), a) ->
  T m (V.Body (Val (ValIM m, a)))
expressionBodyFrom = traverse newValFromH . (^. V.body)

newValFromH ::
  MonadA m =>
  Val (Maybe (ValIM m), a) ->
  T m (Val (ValIM m, a))
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
  (ValIM m -> T m ()) ->
  Val (ValIM m, a) ->
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

writeValTree :: MonadA m => ValTreeM m -> T m (ValIM m)
writeValTree (ValTreeLeaf valI) = return valI
writeValTree (ValTreeNode body) = newValBody =<< traverse writeValTree body
