{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveGeneric, DeriveTraversable #-}
module Lamdu.Font
    ( FontSize, Fonts(..)
    , LCDSubPixelEnabled(..), new
    , lfontDefault, lfontHelp, lfontLiteralText, lfontAutoName, lfontLiteralBytes, lfontBinders
    ) where

import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           Data.Typeable (Typeable)
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import qualified System.Directory as Directory

import           Lamdu.Prelude

newtype MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

data Fonts a = Fonts
    { fontDefault :: a
    , fontHelp :: a
    , fontLiteralText :: a
    , fontLiteralBytes :: a
    , fontAutoName :: a
    , fontBinders :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
deriveJSON defaultOptions ''Fonts

Lens.makeLensesFor
    [ ("fontDefault"     , "lfontDefault"     )
    , ("fontHelp"        , "lfontHelp"        )
    , ("fontAutoName"    , "lfontAutoName"    )
    , ("fontBinders"     , "lfontBinders"     )
    , ("fontLiteralText" , "lfontLiteralText" )
    , ("fontLiteralBytes", "lfontLiteralBytes")
    ]
    ''Fonts

type FontSize = Float

data LCDSubPixelEnabled = LCDSubPixelEnabled | LCDSubPixelDisabled

load :: LCDSubPixelEnabled -> Float -> FilePath -> IO Font
load LCDSubPixelEnabled = Font.openFont
load LCDSubPixelDisabled = Font.openFontNoLCD

openFont :: LCDSubPixelEnabled -> FontSize -> FilePath -> IO Font
openFont subpixel size path =
    do
        exists <- Directory.doesFileExist path
        unless exists $ E.throwIO $ MissingFont $ path ++ " does not exist!"
        load subpixel size path

new :: LCDSubPixelEnabled -> FilePath -> Fonts (FontSize, FilePath) -> IO (Fonts Font)
new subpixel fallbackFontPath =
    traverse openEach
    where
        openEach (fontSize, fontPath) =
            open fontSize fontPath
            `E.catch` \E.SomeException{} ->
            open fontSize fallbackFontPath
        open = openFont subpixel
