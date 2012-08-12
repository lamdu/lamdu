module Data.Store.Guid
    (Guid, make, bs, length, new, combine, fromString, asHex)
where

import Control.Arrow (first)
import Data.Binary (Binary(..))
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteString.Utils (randomBS, xorBS)
import Data.Monoid (mappend)
import Data.Word (Word8)
import Numeric (showHex)
import Prelude hiding (length)
import System.Random (Random(..), split)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.UTF8 as UTF8

newtype Guid = Guid { bs :: SBS.ByteString }
  deriving (Eq, Ord, Read)

instance Show Guid where
  show = take 3 . asHex

inGuid :: (SBS.ByteString -> SBS.ByteString) -> Guid -> Guid
inGuid f = Guid . f . bs
inGuid2 :: (SBS.ByteString -> SBS.ByteString -> SBS.ByteString) ->
           Guid -> Guid -> Guid
inGuid2 f = inGuid . f . bs

word8Hex :: Word8 -> String
word8Hex n
  | n < 0x10 = '0' : showHex n ""
  | otherwise = showHex n ""

encodeHex :: SBS.ByteString -> String
encodeHex = concatMap word8Hex . SBS.unpack

asHex :: Guid -> String
asHex = encodeHex . bs

instance Random Guid where
  randomR = error "randomR: you nuts?"
  random = first (Guid . SBS.pack . take 16 . randoms) . split

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

combine :: Guid -> Guid -> Guid
combine = inGuid2 xorBS
