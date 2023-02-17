{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Lamdu.Expr.IRef
    ( ValI
    , ValBody
    , newVar

    , DefI

    , globalId, defI, nominalI, tagI
    , readTagData

    , readValI, writeValI, newValI

    , module Revision.Deltum.Hyper
    ) where

import qualified Data.UUID.Utils as UUIDUtils
import           Hyper
import           Hyper.Syntax.Nominal (NominalDecl)
import           Hyper.Syntax.Scheme (QVars)
import           Hyper.Type.Functor (F(..), _F)
import           Hyper.Type.Prune (Prune)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Definition (Definition)
import           Lamdu.Data.Tag (Tag(..), Symbol(..))
import           Revision.Deltum.Hyper
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

type DefI m = IRef m (Definition (ValI m) ())

-- NOTE: Nobody else should generate Lamdu-visible Global ElemId's
globalId :: DefI m -> V.Var
globalId = V.Var . Identifier . UUIDUtils.toSBS16 . IRef.uuid

defI :: V.Var -> DefI m
defI (V.Var (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

tagI :: T.Tag -> IRef m Tag
tagI (T.Tag (Identifier bs)) = IRef.unsafeFromUUID $ UUIDUtils.fromSBS16 bs

nominalI :: T.NominalId -> IRef m (Either (T.Types # QVars) (Pure # NominalDecl T.Type))
nominalI (T.NominalId (Identifier bs)) =
    UUIDUtils.fromSBS16 bs & IRef.unsafeFromUUID

readTagData :: Monad m => T.Tag -> T m Tag
readTagData tag =
    Transaction.irefExists i
    >>=
    \case
    False -> pure (Tag 0 NoSymbol mempty)
    True -> Transaction.readIRef i
    where
        i = tagI tag

type ValI m = F (IRef m) # V.Term

type ValBody m = V.Term # F (IRef m)

newVar :: Monad m => T m V.Var
newVar = V.Var . Identifier . UUIDUtils.toSBS16 <$> Transaction.newKey

readValI ::
    (Monad m, Binary (t # F (IRef m))) =>
    F (IRef m) # t -> T m (t # F (IRef m))
readValI = Transaction.readIRef . (^. _F)

writeValI ::
    (Monad m, Binary (t # F (IRef m))) =>
    F (IRef m) # t -> t # F (IRef m) -> T m ()
writeValI = Transaction.writeIRef . (^. _F)

newValI ::
    (Monad m, Binary (t # F (IRef m))) =>
    t # F (IRef m) -> T m (F (IRef m) # t)
newValI = fmap (_F #) . Transaction.newIRef

instance Monad m => HStore m V.Term
instance Monad m => HStore m (HCompose Prune T.Type)
instance Monad m => HStore m (HCompose Prune T.Row)
