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

import           Control.DeepSeq (NFData)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Binary (Binary(..))
import           Data.Function.Decycle (decycle)
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Data.UUID.Utils as UUIDUtils
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val

import           Prelude.Compat

type T = Transaction

type DefI m = IRef m (Definition.Body (ValI m))

-- NOTE: Nobody else should generate Lamdu-visible Global Id's
globalId :: DefI m -> V.Var
globalId = V.Var . Identifier . UUIDUtils.toSBS16 . IRef.uuid

defI :: V.Var -> DefI m
defI (V.Var (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

nominalI :: T.NominalId -> IRef m Nominal
nominalI (T.NominalId (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

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
newVar = V.Var . Identifier . UUIDUtils.toSBS16 <$> Transaction.newKey

readValBody :: Monad m => ValI m -> T m (ValBody m)
readValBody = Transaction.readIRef . unValI

writeValBody ::
    Monad m => ValI m -> ValBody m -> T m ()
writeValBody = Transaction.writeIRef . unValI

newVal :: Monad m => Val () -> T m (ValI m)
newVal = fmap (^. Val.payload . _1) . writeValWithStoredSubexpressions . ((,) Nothing <$>)

-- Returns expression with new UUIDs
writeVal ::
    Monad m =>
    ValI m -> Val a ->
    T m (Val (ValI m, a))
writeVal iref =
    writeValWithStoredSubexpressions .
    (Val.payload . _1 .~ Just iref) .
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
expressionBodyFrom = traverse writeValWithStoredSubexpressions . (^. Val.body)

writeValWithStoredSubexpressions :: Monad m => Val (Maybe (ValI m), a) -> T m (Val (ValI m, a))
writeValWithStoredSubexpressions expr =
    do
        body <- expressionBodyFrom expr
        let bodyWithRefs = body <&> (^. Val.payload . _1)
        case mIRef of
            Just iref ->
                Val (iref, pl) body <$
                writeValBody iref bodyWithRefs
            Nothing ->
                do
                    exprI <- Transaction.newIRef bodyWithRefs
                    return $ Val (ValI exprI, pl) body
    where
        (mIRef, pl) = expr ^. Val.payload

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
            <&> (^. Val.payload . _1) -- convert to body of IRefs
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
