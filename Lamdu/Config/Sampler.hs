{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Config.Sampler
    ( Sampler, new
    , Version, getConfig
    ) where

import           Prelude.Compat

import           Control.Concurrent (threadDelay, ThreadId)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (forkIOUnmasked)
import qualified Control.Exception as E
import           Control.Monad (forever)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock (UTCTime)
import           Lamdu.Config (Config)
import           Lamdu.DataFile (accessDataFile)
import           System.Directory (getModificationTime)

type ModificationTime = UTCTime
type Version = ModificationTime

data Sampler = Sampler
    { _sThreadId :: ThreadId
    , sGetConfig :: IO (ModificationTime, Config)
    }

getConfig :: Sampler -> IO (Version, Config)
getConfig = sGetConfig

withMTime :: FilePath -> IO a -> IO (ModificationTime, a)
withMTime path act =
    do
        mtimeBefore <- getModificationTime path
        res <- act
        mtimeAfter <- getModificationTime path
        if mtimeBefore == mtimeAfter
            then return (mtimeAfter, res)
            else withMTime path act

load :: FilePath -> IO (ModificationTime, Config)
load configPath =
    withMTime configPath $ do
        eConfig <- Aeson.eitherDecode' <$> LBS.readFile configPath
        either (fail . (msg ++)) return eConfig
    where
        msg = "Failed to parse config file contents at " ++ show configPath ++ ": "

maybeLoad :: (ModificationTime, Config) -> FilePath -> IO (ModificationTime, Config)
maybeLoad old@(oldMTime, _) configPath =
    do
        mtime <- getModificationTime configPath
        if mtime == oldMTime
            then return old
            else load configPath

new :: FilePath -> IO Sampler
new startDir =
    do
        ref <-
            sample load
            >>= E.evaluate
            >>= newMVar
        tid <-
            forkIOUnmasked . forever $
            do
                threadDelay 300000
                modifyMVar_ ref $ \old ->
                    sample (maybeLoad old)
                    `E.catch` \E.SomeException {} -> return old
        return $ Sampler tid $ readMVar ref
    where
        sample f = accessDataFile startDir f "config.json"
