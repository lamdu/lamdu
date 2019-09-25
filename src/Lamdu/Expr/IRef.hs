{-# LANGUAGE TypeFamilies #-}
module Lamdu.Expr.IRef
    ( ValI
    , ValBody
    , ValP
    , newVar, readVal
    , writeValWithStoredSubexpressions
    , DefI
    , addProperties

    , globalId, defI, nominalI, tagI
    , readTagData

    , readValI, writeValI, newValI
    ) where

import qualified Control.Lens as Lens
import           Data.Function.Decycle (decycle)
import           Data.Property (Property(..))
import qualified Data.UUID.Utils as UUIDUtils
import           Hyper (Tree, Pure, traverseK1)
import           Hyper.Type.AST.Nominal (NominalDecl)
import           Hyper.Type.Ann (Ann(..), ann, val)
import           Hyper.Type.Functor (F(..), _F)
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Definition (Definition)
import           Lamdu.Data.Tag (Tag(..), OpName(..))
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

nominalI :: T.NominalId -> IRef m (Tree Pure (NominalDecl T.Type))
nominalI (T.NominalId (Identifier bs)) =
    UUIDUtils.fromSBS16 bs & IRef.unsafeFromUUID

readTagData :: Monad m => T.Tag -> T m Tag
readTagData tag =
    Transaction.irefExists iref
    >>=
    \case
    False -> pure (Tag 0 NotAnOp mempty)
    True -> Transaction.readIRef iref
    where
        iref = tagI tag

type ValI m = Tree (F (IRef m)) V.Term

type ValP m = Property (T m) (ValI m)
type ValBody m = Tree V.Term (F (IRef m))

newVar :: Monad m => T m V.Var
newVar = V.Var . Identifier . UUIDUtils.toSBS16 <$> Transaction.newKey

readValI :: Monad m => ValI m -> T m (Tree V.Term (F (IRef m)))
readValI = Transaction.readIRef . (^. _F)

writeValI :: Monad m => ValI m -> Tree V.Term (F (IRef m)) -> T m ()
writeValI = Transaction.writeIRef . (^. _F)

newValI :: Monad m => Tree V.Term (F (IRef m)) -> T m (ValI m)
newValI = fmap (_F #) . Transaction.newIRef

readVal :: Monad m => ValI m -> T m (Val (ValI m))
readVal =
    decycle loop
    where
        loop valI =
            \case
            Nothing -> error $ "Recursive reference: " ++ show valI
            Just go -> readValI valI >>= traverseK1 go <&> Ann valI

expressionBodyFrom ::
    Monad m =>
    Val (Maybe (ValI m), a) ->
    T m (Tree V.Term (Ann (ValI m, a)))
expressionBodyFrom = traverseK1 writeValWithStoredSubexpressions . (^. val)

writeValWithStoredSubexpressions :: Monad m => Val (Maybe (ValI m), a) -> T m (Val (ValI m, a))
writeValWithStoredSubexpressions expr =
    do
        body <- expressionBodyFrom expr
        let bodyWithRefs = body & traverseK1 %~ (^. ann . _1)
        case mIRef of
            Just valI -> Ann (valI, pl) body <$ writeValI valI bodyWithRefs
            Nothing -> newValI bodyWithRefs <&> \exprI -> Ann (exprI, pl) body
    where
        (mIRef, pl) = expr ^. ann

addProperties ::
    Monad m =>
    (ValI m -> T m ()) ->
    Val (ValI m, a) ->
    Val (ValP m, a)
addProperties setValI (Ann (valI, a) body) =
    Ann (Property valI setValI, a) (body & Lens.indexing traverseK1 %@~ f)
    where
        f index =
            addProperties $ \valINew ->
            readValI valI
            <&> Lens.indexing traverseK1 . Lens.index index .~ valINew
            >>= writeValI valI
