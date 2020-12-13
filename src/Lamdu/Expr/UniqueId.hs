{-# LANGUAGE UndecidableInstances, DataKinds, FlexibleInstances #-}
module Lamdu.Expr.UniqueId
    ( ToUUID(..), UniqueId(..), identifierOfUUID
    ) where

import qualified Data.ByteString as BS
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           Hyper (AHyperType(..))
import           Hyper.Type.Functor (F(..), _F)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Rev.Branch (Branch)
import qualified Revision.Deltum.Rev.Branch as Branch
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

identifierOfUUID :: UUID -> Identifier
identifierOfUUID = Identifier . UUIDUtils.toSBS16

class    ToUUID a           where toUUID :: a -> UUID
instance ToUUID Identifier  where toUUID (Identifier bs) = UUIDUtils.fromSBS16 bs
instance ToUUID V.Var       where toUUID = toUUID . V.vvName
instance ToUUID T.NominalId where toUUID = toUUID . T.nomId
instance ToUUID T.Tag       where toUUID = toUUID . T.tagName
instance ToUUID (IRef m a)  where toUUID = IRef.uuid
instance ToUUID (Branch m)  where toUUID = Branch.uuid
instance ToUUID (T.Var a) where
    toUUID (T.Var (Identifier x)) =
        x <> BS.replicate (16 - BS.length x) 0 & UUIDUtils.fromSBS16
instance ToUUID (f ('AHyperType k :# F f)) =>
    ToUUID (F f ('AHyperType k)) where
    toUUID = toUUID . (^. _F)

-- TODO: Remove this when all code uses more descritive types than UUID
instance ToUUID UUID  where toUUID = id

mkNew :: Monad m => (Identifier -> a) -> T m a
mkNew f = f . identifierOfUUID <$> Transaction.newKey

-- NOTE: No other code in Lamdu should be creating var or tag ids!
class ToUUID a => UniqueId a     where new :: Monad m => T m a
instance          UniqueId V.Var where new = mkNew V.Var
instance          UniqueId T.Tag where new = mkNew T.Tag
instance          UniqueId T.NominalId  where new = mkNew T.NominalId
