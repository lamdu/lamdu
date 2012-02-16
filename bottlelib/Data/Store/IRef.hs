{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Store.IRef
    (IRef, guid, unsafeFromGuid, anchor)
where

import           Data.Binary     (Binary)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid

newtype IRef a = IRef {
  guid :: Guid
  }
  deriving (Eq, Ord, Binary, Read, Show)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef a
unsafeFromGuid = IRef

anchor :: (Binary a) => String -> IRef a
anchor = unsafeFromGuid . Guid.fromString
