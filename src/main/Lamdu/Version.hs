-- | Export lamdu version for --help
{-# LANGUAGE TemplateHaskell, NamedFieldPuns #-}
{-# OPTIONS -fforce-recomp -O0 #-}
module Lamdu.Version
    ( VersionInfo(..), currentVersionInfo, currentVersionInfoStr
    ) where

import           Control.Lens.Operators
import           Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import           Language.Haskell.TH (runIO, stringE)
import qualified System.Process.Git as Git

import           Prelude.Compat

data VersionInfo = VersionInfo
    { version :: !String
    , gitCommit :: !String
    , gitStatus :: !String
    , gitDirty :: !Bool
    }

curdate :: String
curdate =
    $(do
        curTime <- getZonedTime
        formatTime defaultTimeLocale "%y-%m-%d" curTime & pure
        & runIO >>= stringE)

_rc :: String -> String
_rc ver = ver ++ "-rc-" ++ curdate

currentVersionInfo :: VersionInfo
currentVersionInfo =
    VersionInfo
    { version = "0.7"
    , gitCommit = $(Git.hash)
    , gitStatus = $(Git.status)
    , gitDirty = $(Git.dirty)
    }

currentVersionInfoStr :: String
currentVersionInfoStr =
    concat
    [ "Lamdu ", version
    , "\n  built from git revision: ", gitCommit
    , statusLine
    ]
    where
        statusLine
            | null gitStatus = ""
            | otherwise = "\n  status:" ++ gitStatus
        VersionInfo{version, gitCommit, gitStatus} = currentVersionInfo
