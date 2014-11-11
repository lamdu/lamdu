{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Lamdu.Expr.Identifier (Identifier(..))
import Lamdu.Expr.Val (Val(..))
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.Scheme as S
import qualified Lamdu.Expr.Val as V

type T = Transaction

type DefI t = IRef t (Definition.Body (ValI t))
type DefIM m = DefI (Tag m)

-- NOTE: Nobody else should generate Lamdu-visible Global Id's
globalId :: DefI t -> V.GlobalId
globalId = V.GlobalId . Identifier . Guid.bs . IRef.guid

defI :: V.GlobalId -> DefI t
defI (V.GlobalId (Identifier bs)) = IRef.unsafeFromGuid $ Guid.make bs

newtype ValI t = ValI {
  unValI :: IRef t (V.Body (ValI t))
  } deriving (Eq, Ord, Show, Binary)

type ValIM m = ValI (Tag m)

type ValIProperty m = Property (T m) (ValIM m)
type ValBody t = V.Body (ValI t)
type Lam t = V.Lam (ValI t)
type Apply t = V.Apply (ValI t)

epGuid :: ValIProperty m -> Guid
epGuid = IRef.guid . unValI . Property.value

valIGuid :: ValI t -> Guid
valIGuid = IRef.guid . unValI

newValBody :: MonadA m => ValBody (Tag m) -> T m (ValIM m)
newValBody = fmap ValI . Transaction.newIRef

-- TODO: Remove this
newLambda :: MonadA m => ValIM m -> T m (V.Var, ValIM m)
newLambda body = do
  paramId <- V.Var . Identifier . Guid.bs <$> Transaction.newKey
  expr <- newValBody $ V.BAbs $ V.Lam paramId S.any body
  return (paramId, expr)

readValBody :: MonadA m => ValIM m -> T m (ValBody (Tag m))
readValBody = Transaction.readIRef . unValI

writeValBody ::
  MonadA m => ValIM m -> ValBody (Tag m) -> T m ()
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

data ValTree t
  = ValTreeLeaf (ValI t)
  | ValTreeNode (V.Body (ValTree t))
  deriving (Show)
type ValTreeM m = ValTree (Tag m)

writeValTree :: MonadA m => ValTreeM m -> T m (ValIM m)
writeValTree (ValTreeLeaf valI) = return valI
writeValTree (ValTreeNode body) = newValBody =<< traverse writeValTree body
