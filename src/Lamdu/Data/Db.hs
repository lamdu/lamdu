{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Data.Db
    ( withDB
    ) where

import           Control.Exception (onException)
import           Data.Store.Db (DB)
import qualified Data.Store.Db as Db
import qualified Lamdu.Data.Db.Init as DbInit
import           Lamdu.Data.Db.Migration (migration)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import           Lamdu.Prelude

withDB :: FilePath -> (DB -> IO a) -> IO a
withDB lamduDir body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        alreadyExist <- Directory.doesDirectoryExist dbPath
        let options =
                Db.defaultOptions
                { Db.createIfMissing = not alreadyExist
                , Db.errorIfExists = not alreadyExist
                }
        Db.withDB dbPath options $
            \db ->
            do
                if alreadyExist
                    then migration db
                    else DbInit.initFreshDb db `onException` Directory.removeDirectoryRecursive dbPath
                body db
    where
        dbPath = lamduDir </> "codeedit.db"
