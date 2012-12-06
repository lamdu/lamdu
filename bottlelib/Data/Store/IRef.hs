{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Data.Store.IRef
  (IRef, guid, unsafeFromGuid, anchor) where

import Control.Applicative ((<$>))
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import qualified Data.Store.Guid as Guid

newtype IRef t a = IRef {
  guid :: Guid
  } deriving (Eq, Ord, Read, Show, Typeable)

instance Binary (IRef t a) where
  get = IRef <$> get
  put (IRef x) = put x

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef t a
unsafeFromGuid = IRef

anchor :: Binary a => String -> IRef t a
anchor = unsafeFromGuid . Guid.fromString
