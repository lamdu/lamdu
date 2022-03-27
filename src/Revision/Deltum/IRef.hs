{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures #-}

module Revision.Deltum.IRef
    ( IRef, uuid, unsafeFromUUID
    , anchor
    ) where

import           Control.DeepSeq (NFData(..))
import qualified Data.ByteString.Char8 as SBS8
import           Data.Kind (Type)
import           Data.List.Extended (rightPad)
import           Data.UUID.Types (UUID)
import           Data.UUID.Utils (fromSBS16)

import           Lamdu.Prelude

newtype IRef (m :: Type -> Type) a = IRef
    { uuid :: UUID
    }
    deriving stock (Read, Show)
    deriving newtype (Eq, Ord, NFData, Binary)

-- Wrapper modules need to create an IRef
unsafeFromUUID :: UUID -> IRef m a
unsafeFromUUID = IRef

anchor :: String -> IRef m a
anchor = unsafeFromUUID . fromSBS16 . SBS8.pack . rightPad 16 '\x00'
