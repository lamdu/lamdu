{-# LANGUAGE DefaultSignatures, ScopedTypeVariables #-}
module GUI.Momentu.Animation.Id
    ( AnimId
    , augmentId
    , ElemIds(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Binary.Extended (encodeS)
import qualified Data.ByteString.Char8 as SBS8
import           Data.Proxy (Proxy(..))
import           Data.Typeable (Typeable, typeRep)

import           GUI.Momentu.Prelude

type AnimId = [ByteString]

augmentId :: Show a => a -> AnimId -> AnimId
augmentId x animId = animId ++ [show x & SBS8.pack]

-- | Global ids for elements in a container of kind `* -> *`.
class ElemIds t where
    elemIds :: t AnimId
    default elemIds :: (Typeable t, Traversable t, Applicative t) => t AnimId
    elemIds =
        let typeStr = SBS8.pack (show (typeRep (Proxy :: Proxy t)))
        in  pure ()
            & Lens.traversed %@~ const . (typeStr :) . (:[]) . encodeS
