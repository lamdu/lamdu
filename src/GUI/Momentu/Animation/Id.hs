module GUI.Momentu.Animation.Id
    ( AnimId
    , augmentId
    ) where

import qualified Data.ByteString.Char8 as SBS8

import           Lamdu.Prelude

type AnimId = [ByteString]

augmentId :: Show a => a -> AnimId -> AnimId
augmentId x animId = animId ++ [show x & SBS8.pack]
