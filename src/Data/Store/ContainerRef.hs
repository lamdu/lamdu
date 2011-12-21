{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Store.ContainerRef
    (ContainerRef, guidBase, unsafeFromGuid, anchor)
where

import           Data.Binary     (Binary)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid

newtype ContainerRef a = ContainerRef { guidBase :: Guid }
  deriving (Eq, Ord, Binary, Read, Show)

unsafeFromGuid :: Guid -> ContainerRef a
unsafeFromGuid = ContainerRef

anchor :: (Binary a) => String -> ContainerRef a
anchor = unsafeFromGuid . Guid.fromString
