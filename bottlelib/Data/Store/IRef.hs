{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures #-}

module Data.Store.IRef
    ( IRef, uuid, unsafeFromUUID
    , anchor
    ) where

import           Control.DeepSeq (NFData(..))
import           Data.Binary (Binary(..))
import qualified Data.ByteString.Char8 as SBS8
import           Data.List.Utils (rightPad)
import           Data.UUID.Types (UUID)

import           Data.UUID.Utils (fromSBS16)

newtype IRef (m :: * -> *) a = IRef
    { uuid :: UUID
    } deriving (Eq, Ord, Read, Show, NFData, Binary)

-- Wrapper modules need to create an IRef
unsafeFromUUID :: UUID -> IRef m a
unsafeFromUUID = IRef

anchor :: Binary a => String -> IRef m a
anchor = unsafeFromUUID . fromSBS16 . SBS8.pack . rightPad 16 '\x00'
