{-# OPTIONS -O2 -Wall #-}

module Data.Store.Guid
    (Guid, make, bs, length, new, xor, fromString)
where

import           Prelude               hiding (length)
import qualified Data.ByteString       as SBS
import           Data.Monoid           (mappend)
import qualified Data.ByteString.UTF8  as UTF8
import           Data.ByteString.Utils (randomBS, xorBS)
import           Data.Binary           (Binary(..))
import           Data.Binary.Get       (getByteString)
import           Data.Binary.Put       (putByteString)

newtype Guid = Guid { bs :: SBS.ByteString }
  deriving (Eq, Ord, Read, Show)
inGuid :: (SBS.ByteString -> SBS.ByteString) -> Guid -> Guid
inGuid f = Guid . f . bs
inGuid2 :: (SBS.ByteString -> SBS.ByteString -> SBS.ByteString) ->
           Guid -> Guid -> Guid
inGuid2 f = inGuid . f . bs

length :: Int
length = 16

make :: SBS.ByteString -> Guid
make bytes
  | l > length = error ("Invalid GUID: too long: " ++ show bytes)
  | l < length = Guid $ bytes `mappend` SBS.replicate (length - l) 0
  | otherwise  = Guid bytes
  where
    l = SBS.length bytes

-- | Use only strings shorter than Guid.length
fromString :: String -> Guid
fromString = make . UTF8.fromString

instance Binary Guid where
  get = Guid `fmap` getByteString length
  put = putByteString . bs

new :: IO Guid
new = Guid `fmap` randomBS length

xor :: Guid -> Guid -> Guid
xor = inGuid2 xorBS
