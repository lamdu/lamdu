{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Data.Store.IRef
    (IRef, guid, unsafeFromGuid, anchor)
where

import Data.Binary (Binary)
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import qualified Data.Store.Guid as Guid

newtype IRef a = IRef {
  guid :: Guid
  } deriving (Eq, Ord, Binary, Read, Show, Typeable)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef a
unsafeFromGuid = IRef

anchor :: (Binary a) => String -> IRef a
anchor = unsafeFromGuid . Guid.fromString
