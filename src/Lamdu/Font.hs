{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Font
    ( FontSize, Fonts(..)
    , Font.LCDSubPixelEnabled(..), new
    , fontDefault, fontHelp, fontLiteralText, fontAutoName, fontLiteralBytes, fontBinders, fontDebugInfo
    , Font.height
    ) where

import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)
import           Data.Typeable (Typeable)
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import qualified System.Directory as Directory

import           Lamdu.Prelude

newtype MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

data Fonts a = Fonts
    { _fontDefault :: a
    , _fontHelp :: a
    , _fontLiteralText :: a
    , _fontLiteralBytes :: a
    , _fontAutoName :: a
    , _fontBinders :: a
    , _fontDebugInfo :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^?! prefixed "_font")
    }
    ''Fonts
Lens.makeLenses ''Fonts

type FontSize = Float

openFont :: Font.LCDSubPixelEnabled -> FontSize -> FilePath -> IO Font
openFont subpixel size path =
    do
        exists <- Directory.doesFileExist path
        unless exists $ E.throwIO $ MissingFont $ path ++ " does not exist!"
        Font.openFont subpixel size path

new :: Font.LCDSubPixelEnabled -> FilePath -> Fonts (FontSize, FilePath) -> IO (Fonts Font)
new subpixel fallbackFontPath =
    traverse openEach
    where
        openEach (fontSize, fontPath) =
            open fontSize fontPath
            `E.catch` \E.SomeException{} ->
            open fontSize fallbackFontPath
        open = openFont subpixel
