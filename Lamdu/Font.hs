{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Font
    ( FontSize, Fonts(..), new
    , lfontDefault, lfontHelp, lfontFancy, lfontAutoName, lfontMono
    ) where

import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.Aeson.Types as Aeson
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import qualified System.Directory as Directory

import           Lamdu.Prelude

data MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

data Fonts a = Fonts
    { fontDefault :: a
    , fontHelp :: a
    , fontFancy :: a
    , fontAutoName :: a
    , fontMono :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
instance Aeson.ToJSON a => Aeson.ToJSON (Fonts a) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON a => Aeson.FromJSON (Fonts a)

Lens.makeLensesFor
    [ ("fontDefault" , "lfontDefault"  )
    , ("fontHelp"    , "lfontHelp"     )
    , ("fontFancy"   , "lfontFancy"    )
    , ("fontAutoName", "lfontAutoName" )
    , ("fontMono"    , "lfontMono"     )
    ]
    ''Fonts

type FontSize = Float

openFont :: FontSize -> FilePath -> IO Draw.Font
openFont size path =
    do
        exists <- Directory.doesFileExist path
        unless exists $ E.throwIO $ MissingFont $ path ++ " does not exist!"
        Draw.openFont size path

new :: Fonts (FontSize, FilePath) -> IO (Fonts Draw.Font)
new = traverse (uncurry openFont)
