module Lamdu.Font
    ( with
    ) where

import           Control.Monad (unless)
import qualified Graphics.DrawingCombinators as Draw
import qualified System.Directory as Directory

with :: FilePath -> (Draw.Font -> IO a) -> IO a
with path action =
    do
        exists <- Directory.doesFileExist path
        unless exists . ioError . userError $ path ++ " does not exist!"
        Draw.withFont path action
