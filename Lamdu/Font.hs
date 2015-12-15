module Lamdu.Font
    ( with
    ) where

import qualified Control.Exception as E
import           Data.Typeable (Typeable)
import qualified Graphics.DrawingCombinators as Draw
import qualified System.Directory as Directory

data MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

with :: E.Exception e => (e -> IO a) -> FilePath -> (Draw.Font -> IO a) -> IO a
with catchError path action =
    do
        exists <- Directory.doesFileExist path
        if exists
            then Draw.withFontCatch catchError path action
            else
                let err = MissingFont $ path ++ " does not exist!"
                in E.throwIO err `E.catch` catchError
