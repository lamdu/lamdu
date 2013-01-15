{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Data.Store.IRef
  ( IRef, guid, unsafeFromGuid
  , anchor
  , Tagged(..), Tag
  ) where

import Control.DeepSeq (NFData(..))
import Control.Applicative (pure)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import qualified Data.Store.Guid as Guid

data Tagged t = Tagged
  deriving (Eq, Ord, Read, Show, Typeable)

instance Binary (Tagged t) where
  get = pure Tagged
  put Tagged = pure ()

instance NFData (Tagged t) where
  rnf Tagged = ()

type Tag m = Tagged (m ())

newtype IRef t a = IRef {
  guid :: Guid
  } deriving (Eq, Ord, Read, Show, Typeable, NFData)

-- derive makeNFData ''IRef
derive makeBinary ''IRef

-- Wrapper modules need to create an IRef
unsafeFromGuid :: Guid -> IRef t a
unsafeFromGuid = IRef

anchor :: Binary a => String -> IRef t a
anchor = unsafeFromGuid . Guid.fromString
