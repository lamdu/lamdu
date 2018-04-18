module Data.UUID.Utils
    ( fromSBS16, toSBS16
    , combine, augment
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Extended as BS
import           Data.Function ((&))
import           Data.Maybe (fromMaybe)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Data.UUID.V5 (generateNamed)

import           Prelude

fromSBS16 :: ByteString -> UUID
fromSBS16 sbs =
    BS.lazify sbs
    & UUID.fromByteString
    & fromMaybe (error ("Failed to convert to UUID: " ++ show sbs))

toSBS16 :: UUID -> ByteString
toSBS16 = BS.strictify . UUID.toByteString

combine :: UUID -> UUID -> UUID
combine = augment . toSBS16

augment :: ByteString -> UUID -> UUID
augment salt uuid = generateNamed uuid $ BS.unpack salt
