{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Config.Sampler
    ( Sampler, new
    , Sample(..), getSample
    ) where

import           Control.Concurrent (threadDelay, ThreadId)
import           Control.Concurrent.MVar
import           Control.Concurrent.Utils (forkIOUnmasked)
import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Monad (forever)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock (UTCTime)
import           Lamdu.Config (Config)
import           Lamdu.DataFile (getDataFilePath)
import           System.Directory (getModificationTime)

import           Prelude.Compat

type ModificationTime = UTCTime

-- TODO: FRP-style sampling of (mtime, file content) of the config
-- file, then map over that to Config

data Sample a = Sample
    { sVersion :: ModificationTime
    , sFilePath :: FilePath
    , sValue :: a
    } deriving (Eq, Ord, Functor, Foldable, Traversable)

data Sampler a = Sampler
    { _sThreadId :: ThreadId
    , sGetSample :: IO (Sample a)
    } deriving Functor

getSample :: Sampler a -> IO (Sample a)
getSample = sGetSample

withMTime :: FilePath -> IO a -> IO (Sample a)
withMTime path act =
    do
        mtimeBefore <- getModificationTime path
        res <- act
        mtimeAfter <- getModificationTime path
        if mtimeBefore == mtimeAfter
            then Sample mtimeAfter path res & return
            else withMTime path act

load :: FilePath -> IO (Sample Config)
load configPath =
    do
        eConfig <- Aeson.eitherDecode' <$> LBS.readFile configPath
        either (fail . (msg ++)) return eConfig
    & withMTime configPath
    where
        msg = "Failed to parse config file contents at " ++ show configPath ++ ": "

maybeLoad :: Sample Config -> FilePath -> IO (Sample Config)
maybeLoad old configPath =
    do
        mtime <- getModificationTime configPath
        if mtime == sVersion old
            then return old
            else load configPath

new :: FilePath -> IO (Sampler Config)
new startDir =
    do
        ref <-
            getConfigPath
            >>= load
            >>= E.evaluate
            >>= newMVar
        tid <-
            forkIOUnmasked . forever $
            do
                threadDelay 300000
                modifyMVar_ ref $ \old ->
                    (getConfigPath >>= maybeLoad old)
                    `E.catch` \E.SomeException {} -> return old
        return $ Sampler tid $ readMVar ref
    where
        getConfigPath = getDataFilePath startDir "config.json"
