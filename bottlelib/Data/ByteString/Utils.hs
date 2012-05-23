module Data.ByteString.Utils
    (lazifyBS, strictifyBS, randomBS, xorBS)
where

import           Control.Monad        (replicateM)
import qualified Data.ByteString      as SBS
import qualified Data.ByteString.Lazy as LBS
import           System.Random        (randomIO)
import           Data.Random.Instances()
import           Data.Bits            (xor)

strictifyBS :: LBS.ByteString -> SBS.ByteString
strictifyBS = SBS.concat . LBS.toChunks

lazifyBS :: SBS.ByteString -> LBS.ByteString
lazifyBS = LBS.fromChunks . return

randomBS :: Int -> IO SBS.ByteString
randomBS l = SBS.pack `fmap` replicateM l randomIO

xorBS :: SBS.ByteString -> SBS.ByteString -> SBS.ByteString
xorBS x y = SBS.pack $ SBS.zipWith xor x y
