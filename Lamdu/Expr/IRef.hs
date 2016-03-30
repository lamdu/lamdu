{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, GeneralizedNewtypeDeriving, LambdaCase #-}
module Lamdu.Expr.IRef
    ( ValI(..)
    , ValBody
    , ValIProperty
    , Lam, Apply
    , newValBody, readValBody, writeValBody
    , newVar
    , newVal, writeVal, readVal
    , writeValWithStoredSubexpressions
    , DefI
    , addProperties

    , globalId, defI, nominalI

    , ValTree(..), ValTreeM, writeValTree
    ) where

import           Prelude.Compat

import           Control.DeepSeq (NFData)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Binary (Binary(..))
import           Data.Function.Decycle (decycle)
import qualified Data.Store.Guid as Guid
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.Identifier (Identifier(..))
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V

type T = Transaction

type DefI m = IRef m (Definition.Body (ValI m))

-- NOTE: Nobody else should generate Lamdu-visible Global Id's
globalId :: DefI m -> V.Var
globalId = V.Var . Identifier . Guid.bs . IRef.guid

defI :: V.Var -> DefI m
defI (V.Var (Identifier bs)) = IRef.unsafeFromGuid $ Guid.make bs

nominalI :: T.NominalId -> IRef m Nominal
nominalI (T.NominalId (Identifier bs)) = IRef.unsafeFromGuid $ Guid.make bs

newtype ValI m = ValI
    { unValI :: IRef m (V.Body (ValI m))
    } deriving (Eq, Ord, Show, Binary, NFData)

type ValIProperty m = Property (T m) (ValI m)
type ValBody m = V.Body (ValI m)
type Lam m = V.Lam (ValI m)
type Apply m = V.Apply (ValI m)

newValBody :: Monad m => ValBody m -> T m (ValI m)
newValBody = fmap ValI . Transaction.newIRef

newVar :: Monad m => T m V.Var
newVar = V.Var . Identifier . Guid.bs <$> Transaction.newKey

readValBody :: Monad m => ValI m -> T m (ValBody m)
readValBody = Transaction.readIRef . unValI

writeValBody ::
    Monad m => ValI m -> ValBody m -> T m ()
writeValBody = Transaction.writeIRef . unValI

newVal :: Monad m => Val () -> T m (ValI m)
newVal = fmap (^. V.payload . _1) . writeValWithStoredSubexpressions . ((,) Nothing <$>)

-- Returns expression with new Guids
writeVal ::
    Monad m =>
    ValI m -> Val a ->
    T m (Val (ValI m, a))
writeVal iref =
    writeValWithStoredSubexpressions .
    (V.payload . _1 .~ Just iref) .
    fmap ((,) Nothing)

readVal :: Monad m => ValI m -> T m (Val (ValI m))
readVal =
    decycle loop
    where
        loop valI =
            \case
            Nothing -> error $ "Recursive reference: " ++ show valI
            Just go -> readValBody valI >>= traverse go <&> Val valI

expressionBodyFrom ::
    Monad m =>
    Val (Maybe (ValI m), a) ->
    T m (V.Body (Val (ValI m, a)))
expressionBodyFrom = traverse writeValWithStoredSubexpressions . (^. V.body)

writeValWithStoredSubexpressions :: Monad m => Val (Maybe (ValI m), a) -> T m (Val (ValI m, a))
writeValWithStoredSubexpressions expr =
    do
        body <- expressionBodyFrom expr
        let bodyWithRefs = body <&> (^. V.payload . _1)
        case mIRef of
            Just iref ->
                Val (iref, pl) body <$
                writeValBody iref bodyWithRefs
            Nothing ->
                do
                    exprI <- Transaction.newIRef bodyWithRefs
                    return $ Val (ValI exprI, pl) body
    where
        (mIRef, pl) = expr ^. V.payload

addProperties ::
    Monad m =>
    (ValI m -> T m ()) ->
    Val (ValI m, a) ->
    Val (ValIProperty m, a)
addProperties setIRef (Val (iref, a) body) =
    Val (Property iref setIRef, a) (body & Lens.traversed %@~ f)
    where
        f index =
            addProperties $ \newIRef ->
            body
            <&> (^. V.payload . _1) -- convert to body of IRefs
            & Lens.element index .~ newIRef
            & writeValBody iref

data ValTree m
    = ValTreeLeaf (ValI m)
    | ValTreeNode (V.Body (ValTree m))
    deriving (Show)
type ValTreeM m = ValTree m

writeValTree :: Monad m => ValTreeM m -> T m (ValI m)
writeValTree (ValTreeLeaf valI) = return valI
writeValTree (ValTreeNode body) = newValBody =<< traverse writeValTree body
