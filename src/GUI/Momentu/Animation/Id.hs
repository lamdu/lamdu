module GUI.Momentu.Animation.Id
    ( AnimId
    , augmentId
    ) where

import qualified Data.ByteString.Char8 as SBS8

import           Lamdu.Prelude

type AnimId = [ByteString]

augmentId :: Show a => AnimId -> a -> AnimId
augmentId animId = (animId ++) . (:[]) . SBS8.pack . show
