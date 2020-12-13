-- | Export lamdu version for --help
{-# LANGUAGE CPP, TemplateHaskell, NamedFieldPuns #-}
{-# OPTIONS -O0 #-}
#ifndef DEV_BUILD
{-# OPTIONS -fforce-recomp #-}
#endif
module Lamdu.Version
    ( VersionInfo(..), currentVersionInfo, currentVersionInfoStr
    ) where

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

_curdate :: String
_curdate =
    $(runIO (formatTime defaultTimeLocale "%y-%m-%d" <$> getZonedTime) >>= stringE)

_rc :: String -> String
_rc ver = ver ++ "-rc-" ++ _curdate

currentVersionInfo :: VersionInfo
currentVersionInfo =
    VersionInfo
    { version =
#ifdef DEV_BUILD
        "<devel>"
#else
        "0.8"
#endif
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
