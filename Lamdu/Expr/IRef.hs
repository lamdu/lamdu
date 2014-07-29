{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.Expr.IRef
  ( ValI(..), ValIM
  , ValBody
  , ValIProperty, epGuid
  , Lam, Apply
  , newValBody, readValBody, writeValBody, valIGuid
  , newLambda
  , newVal, writeVal, readVal
  , writeValWithStoredSubexpressions
  , DefI, DefIM
  , addProperties

  , globalId, defI

  , ValTree(..), ValTreeM, writeValTree
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr as E

type T = Transaction

type DefI t = IRef t (Definition.Body (ValI t))
type DefIM m = DefI (Tag m)

globalId :: DefI t -> E.GlobalId
globalId = E.GlobalId . E.Identifier . Guid.bs . IRef.guid

defI :: E.GlobalId -> DefI t
defI (E.GlobalId (E.Identifier bs)) = IRef.unsafeFromGuid $ Guid.make bs

newtype ValI t = ValI {
  unValI :: IRef t (E.ValBody (ValI t))
  } deriving (Eq, Ord, Show, Typeable, Binary)

type ValIM m = ValI (Tag m)

type ValIProperty m = Property (T m) (ValIM m)
type ValBody t = E.ValBody (ValI t)
type Lam t = E.Lam (ValI t)
type Apply t = E.Apply (ValI t)

epGuid :: ValIProperty m -> Guid
epGuid = IRef.guid . unValI . Property.value

valIGuid :: ValI t -> Guid
valIGuid = IRef.guid . unValI

newValBody :: MonadA m => ValBody (Tag m) -> T m (ValIM m)
newValBody = fmap ValI . Transaction.newIRef

-- TODO: Remove this
newLambda :: MonadA m => ValIM m -> T m (E.ValVar, ValIM m)
newLambda body = do
  paramId <- E.ValVar . E.Identifier . Guid.bs <$> Transaction.newKey
  expr <- newValBody $ E.VAbs $ E.Lam paramId body
  return (paramId, expr)

readValBody :: MonadA m => ValIM m -> T m (ValBody (Tag m))
readValBody = Transaction.readIRef . unValI

writeValBody ::
  MonadA m => ValIM m -> ValBody (Tag m) -> T m ()
writeValBody = Transaction.writeIRef . unValI

newVal :: MonadA m => E.Val () -> T m (ValIM m)
newVal = fmap (^. E.valPayload . Lens._1) . newValFromH . ((,) Nothing <$>)

-- Returns expression with new Guids
writeVal ::
  MonadA m =>
  ValIM m -> E.Val a ->
  T m (E.Val (ValIM m, a))
writeVal iref =
  writeValWithStoredSubexpressions iref .
  fmap ((,) Nothing)

writeValWithStoredSubexpressions ::
  MonadA m => ValIM m -> E.Val (Maybe (ValIM m), a) -> T m (E.Val (ValIM m, a))
writeValWithStoredSubexpressions iref expr = do
  exprBodyP <- expressionBodyFrom expr
  exprBodyP
    <&> (^. E.valPayload . Lens._1)
    & writeValBody iref
  return $ E.Val (iref, expr ^. E.valPayload . Lens._2) exprBodyP

readVal ::
  MonadA m => ValIM m -> T m (E.Val (ValIM m))
readVal exprI =
  fmap (E.Val exprI) .
  traverse readVal =<< readValBody exprI

expressionBodyFrom ::
  MonadA m =>
  E.Val (Maybe (ValIM m), a) ->
  T m (E.ValBody (E.Val (ValIM m, a)))
expressionBodyFrom = traverse newValFromH . (^. E.valBody)

newValFromH ::
  MonadA m =>
  E.Val (Maybe (ValIM m), a) ->
  T m (E.Val (ValIM m, a))
newValFromH expr =
  case mIRef of
  Just iref -> writeValWithStoredSubexpressions iref expr
  Nothing -> do
    body <- expressionBodyFrom expr
    exprI <-
      body
      <&> (^. E.valPayload . Lens._1)
      & Transaction.newIRef
    return $ E.Val (ValI exprI, pl) body
  where
    (mIRef, pl) = expr ^. E.valPayload

addProperties ::
  MonadA m =>
  (ValIM m -> T m ()) ->
  E.Val (ValIM m, a) ->
  E.Val (ValIProperty m, a)
addProperties setIRef (E.Val (iref, a) body) =
  E.Val (Property iref setIRef, a) (body & Lens.traversed %@~ f)
  where
    f index =
      addProperties $ \newIRef ->
      body
      <&> (^. E.valPayload . Lens._1) -- convert to body of IRefs
      & Lens.element index .~ newIRef
      & writeValBody iref

data ValTree t
  = ValTreeLeaf (ValI t)
  | ValTreeNode (E.ValBody (ValTree t))
  deriving (Show, Typeable)
type ValTreeM m = ValTree (Tag m)

writeValTree :: MonadA m => ValTreeM m -> T m (ValIM m)
writeValTree (ValTreeLeaf valI) = return valI
writeValTree (ValTreeNode body) = newValBody =<< traverse writeValTree body
