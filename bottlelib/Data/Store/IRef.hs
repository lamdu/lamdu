{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Store.IRef
  ( IRef, guid, unsafeFromGuid
  , anchor
  , Tagged(..), Tag
  ) where

import Control.DeepSeq (NFData(..))
import Control.Applicative (pure)
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid

data Tagged t = Tagged
  deriving (Eq, Ord, Read, Show)

instance Binary (Tagged t) where
  get = pure Tagged
  put Tagged = pure ()

instance NFData (Tagged t) where
  rnf Tagged = ()

type Tag m = Tagged (m ())

newtype IRef t a = IRef {
  guid :: Guid
  } deriving (Eq, Ord, Read, Show, NFData, Binary)

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef t a
unsafeFromGuid = IRef

anchor :: Binary a => String -> IRef t a
anchor = unsafeFromGuid . Guid.fromString
