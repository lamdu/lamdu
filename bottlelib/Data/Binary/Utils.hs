module Data.Binary.Utils
    (decodeS, encodeS)
where

import           Data.Binary (Binary(..))
import qualified Data.Binary as Binary
import qualified Data.ByteString as SBS
import           Data.ByteString.Utils (lazifyBS, strictifyBS)

decodeS :: Binary a => SBS.ByteString -> a
decodeS = Binary.decode . lazifyBS

encodeS :: Binary a => a -> SBS.ByteString
encodeS = strictifyBS . Binary.encode
