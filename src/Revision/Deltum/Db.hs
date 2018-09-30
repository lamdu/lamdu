{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Revision.Deltum.Db
    ( withDB, defaultOptions, Haskey.FileStoreConfig(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (foldM)
import qualified Control.Monad.Haskey as Haskey
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Binary (Binary)
import           Data.UUID.Types (UUID)
import qualified Data.BTree.Impure as BTree
import qualified Data.BTree.Primitives.Key as BTree
import qualified Data.BTree.Primitives.Value as BTree
import           Data.Typeable (Typeable)
import qualified Database.Haskey.Alloc.Concurrent as Haskey hiding (transact, transactReadOnly)
import qualified Database.Haskey.Store.File as Haskey
import           Revision.Deltum.Transaction (Store(..))
import           System.Random (randomIO)

import           Lamdu.Prelude hiding (lookup)

newtype Schema = Schema
    { _schemaData :: BTree.Tree UUID ByteString
    } deriving (Generic, Show, Typeable)
Lens.makeLenses ''Schema

instance BTree.Value UUID
instance BTree.Key UUID where
    narrow x y = (x, y)

instance Binary Schema
instance BTree.Value Schema
instance Haskey.Root Schema

type DB = (Haskey.FileStoreConfig, Haskey.ConcurrentDb Schema)

openOrCreateDb :: FilePath -> Haskey.FileStoreConfig -> IO DB
openOrCreateDb path config =
    do
        Haskey.openConcurrentDb handles & MaybeT & justToLeft
        Schema BTree.empty & Haskey.createConcurrentDb handles & lift
    & runMatcherT
    & (`Haskey.runFileStoreT` config)
    <&> (,) config
    where
        handles = Haskey.concurrentHandles path

defaultOptions :: Haskey.FileStoreConfig
defaultOptions = Haskey.defFileStoreConfig

lookup :: DB -> UUID -> IO (Maybe ByteString)
lookup (config, db) key =
    Haskey.runHaskeyT (Haskey.transactReadOnly f) db config
    where
        f schema =
            BTree.lookup key (schema ^. schemaData)

transaction :: DB -> [(UUID, Maybe ByteString)] -> IO ()
transaction (config, db) ops =
    Haskey.runHaskeyT (Haskey.transact f) db config
    where
        f schema =
            schemaData (foldM op ?? ops) schema >>= Haskey.commit_
        op schema (k, Nothing) = BTree.delete k schema
        op schema (k, Just v) = BTree.insert k v schema

store :: DB -> Store IO
store db =
    Store
    { storeNewKey = randomIO
    , storeLookup = lookup db
    , storeAtomicWrite = transaction db
    }

withDB :: FilePath -> Haskey.FileStoreConfig -> (Store IO -> IO a) -> IO a
withDB path opts act =
    do
        db <- openOrCreateDb path opts
        r <- act (store db)
        Haskey.runFileStoreT (Haskey.closeConcurrentHandles (Haskey.concurrentHandles path)) opts
        pure r
