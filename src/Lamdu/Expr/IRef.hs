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
import           Data.Binary (Binary)
import           Data.Property (Property(..))
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           Hyper
import           Hyper.Type.AST.Nominal (NominalDecl)
import           Hyper.Type.Functor (F(..), _F)
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Definition (Definition)
import           Lamdu.Data.Tag (Tag(..), Symbol(..))
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
    False -> pure (Tag 0 NoSymbol mempty)
    True -> Transaction.readIRef iref
    where
        iref = tagI tag

type ValI m = Tree (F (IRef m)) V.Term

type ValP m = Property (T m) (ValI m)
type ValBody m = Tree V.Term (F (IRef m))

newVar :: Monad m => T m V.Var
newVar = V.Var . Identifier . UUIDUtils.toSBS16 <$> Transaction.newKey

readValI ::
    (Monad m, Binary (Tree t (F (IRef m)))) =>
    Tree (F (IRef m)) t -> T m (Tree t (F (IRef m)))
readValI = Transaction.readIRef . (^. _F)

writeValI ::
    (Monad m, Binary (Tree t (F (IRef m)))) =>
    Tree (F (IRef m)) t -> Tree t (F (IRef m)) -> T m ()
writeValI = Transaction.writeIRef . (^. _F)

newValI ::
    (Monad m, Binary (Tree t (F (IRef m)))) =>
    Tree t (F (IRef m)) -> T m (Tree (F (IRef m)) t)
newValI = fmap (_F #) . Transaction.newIRef

readVal ::
    Monad m =>
    Tree (F (IRef m)) V.Term ->
    T m (Annotated (Tree (F (IRef m)) V.Term) V.Term)
readVal = readValH Set.empty

readValH ::
    ( Monad m
    , HNodesConstraint t ((~) t)
    , Binary (Tree t (F (IRef m)))
    , HTraversable t
    ) =>
    Set UUID ->
    Tree (F (IRef m)) t ->
    T m (Annotated (Tree (F (IRef m)) t) t)
readValH visited valI
    | visited ^. Lens.contains k = error $ "Recursive reference: " ++ show valI
    | otherwise =
        readValI valI
        >>= htraverse1 (readValH (visited & Lens.contains k .~ True))
        <&> Ann (Const valI)
    where
        k = IRef.uuid (valI ^. _F)

expressionBodyFrom ::
    Monad m =>
    Val (Maybe (ValI m), a) ->
    T m (Tree V.Term (Ann (Const (ValI m, a))))
expressionBodyFrom = htraverse1 writeValWithStoredSubexpressions . (^. hVal)

writeValWithStoredSubexpressions :: Monad m => Val (Maybe (ValI m), a) -> T m (Val (ValI m, a))
writeValWithStoredSubexpressions expr =
    do
        body <- expressionBodyFrom expr
        let bodyWithRefs = body & htraverse1 %~ (^. hAnn . Lens._Wrapped . _1)
        case mIRef of
            Just valI -> Ann (Const (valI, pl)) body <$ writeValI valI bodyWithRefs
            Nothing -> newValI bodyWithRefs <&> \exprI -> Ann (Const (exprI, pl)) body
    where
        (mIRef, pl) = expr ^. hAnn . Lens._Wrapped

addProperties ::
    Monad m =>
    (ValI m -> T m ()) ->
    Val (ValI m, a) ->
    Val (ValP m, a)
addProperties setValI (Ann (Const (valI, a)) body) =
    Ann (Const (Property valI setValI, a)) (body & Lens.indexing htraverse1 %@~ f)
    where
        f index =
            addProperties $ \valINew ->
            readValI valI
            <&> Lens.indexing htraverse1 . Lens.index index .~ valINew
            >>= writeValI valI
