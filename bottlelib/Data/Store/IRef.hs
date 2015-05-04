{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures #-}

module Data.Store.IRef
  ( IRef, guid, unsafeFromGuid
  , anchor
  ) where

import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid

newtype IRef (m :: * -> *) a = IRef
  { guid :: Guid
  } deriving (Eq, Ord, Read, Show, NFData, Binary)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef m a
unsafeFromGuid = IRef

anchor :: Binary a => String -> IRef m a
anchor = unsafeFromGuid . Guid.fromString
