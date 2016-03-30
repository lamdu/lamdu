module Data.UUID.Utils
    ( fromSBS16, toSBS16
    , combine, augment
    ) where

import           Control.Lens.Operators
import           Data.ByteString (ByteString)
import qualified Data.ByteString as SBS
import           Data.ByteString.Utils (lazifyBS, strictifyBS)
import           Data.Maybe (fromMaybe)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Data.UUID.V5 (generateNamed)

fromSBS16 :: ByteString -> UUID
fromSBS16 sbs =
    lazifyBS sbs
    & UUID.fromByteString
    & fromMaybe (error ("Failed to convert to UUID: " ++ show sbs))

toSBS16 :: UUID -> ByteString
toSBS16 = strictifyBS . UUID.toByteString

combine :: UUID -> UUID -> UUID
combine = augment . toSBS16

augment :: ByteString -> UUID -> UUID
augment salt uuid = generateNamed uuid $ SBS.unpack salt
