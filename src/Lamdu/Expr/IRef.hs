module Lamdu.Expr.IRef
    ( ValI
    , ValBody
    , ValP
    , Lam, Apply
    , newValBody, readValBody, writeValBody
    , newVar, readVal
    , writeValWithStoredSubexpressions
    , DefI
    , addProperties

    , globalId, defI, nominalI

    ) where

import qualified Control.Lens as Lens
import           Data.Function.Decycle (decycle)
import           Data.Property (Property(..))
import qualified Data.UUID.Utils as UUIDUtils
import           Data.Tree.Diverse (Node(..), Ann(..), _Node, ann, val)
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import           Lamdu.Data.Definition (Definition)
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

type DefI m = IRef m (Definition (ValI m) ())

-- NOTE: Nobody else should generate Lamdu-visible Global Id's
globalId :: DefI m -> V.Var
globalId = V.Var . Identifier . UUIDUtils.toSBS16 . IRef.uuid

defI :: V.Var -> DefI m
defI (V.Var (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

nominalI :: T.NominalId -> IRef m Nominal
nominalI (T.NominalId (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

type ValI m = Node (IRef m) V.Term

type ValP m = Property (T m) (ValI m)
type ValBody m = V.Term (IRef m)
type Lam m = V.Lam (ValI m)
type Apply m = V.Apply (ValI m)

newValBody :: Monad m => ValBody m -> T m (ValI m)
newValBody = fmap Node . Transaction.newIRef

newVar :: Monad m => T m V.Var
newVar = V.Var . Identifier . UUIDUtils.toSBS16 <$> Transaction.newKey

readValBody :: Monad m => ValI m -> T m (ValBody m)
readValBody = Transaction.readIRef . (^. _Node)

writeValBody ::
    Monad m => ValI m -> ValBody m -> T m ()
writeValBody = Transaction.writeIRef . (^. _Node)

readVal :: Monad m => ValI m -> T m (Val (ValI m))
readVal =
    decycle loop
    where
        loop valI =
            \case
            Nothing -> error $ "Recursive reference: " ++ show valI
            Just go -> readValBody valI >>= V.termChildren go <&> Ann valI <&> Node

expressionBodyFrom ::
    Monad m =>
    Val (Maybe (ValI m), a) ->
    T m (V.Term (Ann (ValI m, a)))
expressionBodyFrom = V.termChildren writeValWithStoredSubexpressions . (^. _Node . val)

writeValWithStoredSubexpressions :: Monad m => Val (Maybe (ValI m), a) -> T m (Val (ValI m, a))
writeValWithStoredSubexpressions expr =
    do
        body <- expressionBodyFrom expr
        let bodyWithRefs = body & V.termChildren %~ (^. _Node . ann . _1)
        case mIRef of
            Just iref ->
                Node (Ann (iref, pl) body) <$
                writeValBody iref bodyWithRefs
            Nothing ->
                Transaction.newIRef bodyWithRefs
                <&> \exprI -> Node (Ann (Node exprI, pl) body)
    where
        (mIRef, pl) = expr ^. _Node . ann

addProperties ::
    Monad m =>
    (ValI m -> T m ()) ->
    Val (ValI m, a) ->
    Val (ValP m, a)
addProperties setIRef (Node (Ann (iref, a) body)) =
    Ann (Property iref setIRef, a) (body & Lens.indexing V.termChildren %@~ f) & Node
    where
        f index =
            addProperties $ \newIRef ->
            readValBody iref
            <&> Lens.indexing V.termChildren . Lens.index index .~ newIRef
            >>= writeValBody iref
