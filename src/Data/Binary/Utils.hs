{-# OPTIONS -O2 -Wall #-}

module Data.Binary.Utils
    (decodeS, encodeS,
     get0, get1, get2, get3,
     put0, put1, put2, put3)
where

import           Control.Monad         (liftM, liftM2, liftM3)
import qualified Data.ByteString       as SBS
import qualified Data.Binary           as Binary
import           Data.ByteString.Utils (lazifyBS, strictifyBS)
import           Data.Binary           (Binary(..))
import           Data.Binary.Get       (Get)
import           Data.Binary.Put       (Put)

decodeS :: Binary a => SBS.ByteString -> a
decodeS = Binary.decode . lazifyBS

encodeS :: Binary a => a -> SBS.ByteString
encodeS = strictifyBS . Binary.encode

get0 :: a -> Get a
get0 = return

get1 :: (Binary a) => (a -> b) -> Get b
get1 f = liftM f get

get2 :: (Binary a, Binary b) => (a -> b -> c) -> Get c
get2 f = liftM2 f get get

get3 :: (Binary a, Binary b, Binary c) => (a -> b -> c -> d) -> Get d
get3 f = liftM3 f get get get

put0 :: Put
put0 = return ()

put1 :: (Binary a) => a -> Put
put1 = put

put2 :: (Binary a, Binary b) => a -> b -> Put
put2 x y = put x >> put y

put3 :: (Binary a, Binary b, Binary c) => a -> b -> c -> Put
put3 x y z = put x >> put y >> put z
