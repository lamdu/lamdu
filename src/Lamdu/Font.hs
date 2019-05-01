module Lamdu.Font
    ( Font.LCDSubPixelEnabled(..), new
    , Font.height
    ) where

import qualified Control.Exception as E
import           Data.Typeable (Typeable)
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import qualified System.Directory as Directory

import           Lamdu.Prelude

newtype MissingFont = MissingFont FilePath
    deriving stock (Generic, Show, Typeable)
    deriving anyclass E.Exception

openFont :: Font.LCDSubPixelEnabled -> Float -> FilePath -> IO Font
openFont subpixel size path =
    do
        unless (null path) $ -- (if not debug font)
            do
                exists <- Directory.doesFileExist path
                unless exists $ E.throwIO $
                    MissingFont $ path ++ " does not exist!"
        Font.openFont subpixel size path

new :: Traversable t => Font.LCDSubPixelEnabled -> FilePath -> t (Float, FilePath) -> IO (t Font)
new subpixel fallbackFontPath =
    traverse openEach
    where
        openEach (fontSize, fontPath) =
            open fontSize fontPath
            `E.catch` \E.SomeException{} ->
            open fontSize fallbackFontPath
        open = openFont subpixel
