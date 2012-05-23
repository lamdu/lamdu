module Data.Binary.Utils
    (decodeS, encodeS)
where

import qualified Data.ByteString       as SBS
import qualified Data.Binary           as Binary
import           Data.ByteString.Utils (lazifyBS, strictifyBS)
import           Data.Binary           (Binary(..))

decodeS :: Binary a => SBS.ByteString -> a
decodeS = Binary.decode . lazifyBS

encodeS :: Binary a => a -> SBS.ByteString
encodeS = strictifyBS . Binary.encode
