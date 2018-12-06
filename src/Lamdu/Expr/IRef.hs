module Lamdu.Expr.IRef
    ( ValI
    , ValBody
    , ValP
    , newVar, readVal
    , writeValWithStoredSubexpressions
    , DefI
    , addProperties

    , globalId, defI, nominalI, tagI
    , readTagInfo

    ) where

import           AST (Node, monoChildren)
import           AST.Functor.Ann (Ann(..), ann, val)
import qualified Control.Lens as Lens
import           Data.Function.Decycle (decycle)
import           Data.Property (Property(..))
import qualified Data.UUID.Utils as UUIDUtils
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import           Lamdu.Data.Definition (Definition)
import           Lamdu.Data.Tag (Tag(..))
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

tagI :: T.Tag -> IRef m Tag
tagI (T.Tag (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

nominalI :: T.NominalId -> IRef m Nominal
nominalI (T.NominalId (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

readTagInfo :: Monad m => T.Tag -> T m Tag
readTagInfo tag =
    Transaction.irefExists iref
    >>=
    \case
    False -> pure (Tag "" 0)
    True -> Transaction.readIRef iref
    where
        iref = tagI tag

type ValI m = Node (IRef m) V.Term

type ValP m = Property (T m) (ValI m)
type ValBody m = V.Term (IRef m)

newVar :: Monad m => T m V.Var
newVar = V.Var . Identifier . UUIDUtils.toSBS16 <$> Transaction.newKey

readVal :: Monad m => ValI m -> T m (Val (ValI m))
readVal =
    decycle loop
    where
        loop valI =
            \case
            Nothing -> error $ "Recursive reference: " ++ show valI
            Just go ->
                Transaction.readIRef valI >>=
                monoChildren go <&> Ann valI

expressionBodyFrom ::
    Monad m =>
    Val (Maybe (ValI m), a) ->
    T m (V.Term (Ann (ValI m, a)))
expressionBodyFrom = monoChildren writeValWithStoredSubexpressions . (^. val)

writeValWithStoredSubexpressions :: Monad m => Val (Maybe (ValI m), a) -> T m (Val (ValI m, a))
writeValWithStoredSubexpressions expr =
    do
        body <- expressionBodyFrom expr
        let bodyWithRefs = body & monoChildren %~ (^. ann . _1)
        case mIRef of
            Just iref ->
                Ann (iref, pl) body <$
                Transaction.writeIRef iref bodyWithRefs
            Nothing ->
                Transaction.newIRef bodyWithRefs
                <&> \exprI -> Ann (exprI, pl) body
    where
        (mIRef, pl) = expr ^. ann

addProperties ::
    Monad m =>
    (ValI m -> T m ()) ->
    Val (ValI m, a) ->
    Val (ValP m, a)
addProperties setIRef (Ann (iref, a) body) =
    Ann (Property iref setIRef, a) (body & Lens.indexing monoChildren %@~ f)
    where
        f index =
            addProperties $ \newIRef ->
            Transaction.readIRef iref
            <&> Lens.indexing monoChildren . Lens.index index .~ newIRef
            >>= Transaction.writeIRef iref
