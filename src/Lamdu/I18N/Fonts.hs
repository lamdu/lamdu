{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Lamdu.I18N.Fonts
    ( Choice(..)
    , ProportionalAndMonospace(..), ProportionalOrMonospace(..)
    , SansAndSerif(..), SansOrSerif(..)
    , RomanAndItalic(..), RomanOrItalic(..)
    , LightAndBold(..), LightOrBold(..)
    , proportional, monospace, sans, serif, roman, italic, light, bold
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified Data.Aeson.Types as Aeson
import           Lamdu.Prelude

data ProportionalAndMonospace a = ProportionalAndMonospace
    { _proportional :: a
    , _monospace :: a
    } deriving Eq

data SansAndSerif a = SansAndSerif
    { _sans :: a
    , _serif :: a
    } deriving Eq

data RomanAndItalic a = RomanAndItalic
    { _roman :: a
    , _italic :: a
    } deriving Eq

data LightAndBold a = LightAndBold
    { _light :: a
    , _bold :: a
    } deriving Eq

traverse (\x -> (<>) <$> Lens.makeLenses x <*> JsonTH.derivePrefixed "_" x)
    [''ProportionalAndMonospace, ''SansAndSerif, ''RomanAndItalic, ''LightAndBold]
    <&> concat

class Choice s where
    type Options s :: * -> *
    choice :: Functor f => s -> Lens.LensLike' f (Options s a) a

data ProportionalOrMonospace = Proportional | Monospace deriving (Eq, Generic)
data SansOrSerif = Sans | Serif                         deriving (Eq, Generic)
data RomanOrItalic = Roman | Italic                     deriving (Eq, Generic)
data LightOrBold = Light | Bold                         deriving (Eq, Generic)

traverse (JsonTH.deriveJSON Aeson.defaultOptions)
    [''ProportionalOrMonospace, ''SansOrSerif, ''RomanOrItalic, ''LightOrBold]
    <&> concat

instance Choice ProportionalOrMonospace where
    type Options ProportionalOrMonospace = ProportionalAndMonospace
    choice Proportional = proportional
    choice Monospace = monospace

instance Choice SansOrSerif where
    type Options SansOrSerif = SansAndSerif
    choice Sans = sans
    choice Serif = serif

instance Choice RomanOrItalic where
    type Options RomanOrItalic = RomanAndItalic
    choice Roman = roman
    choice Italic = italic

instance Choice LightOrBold where
    type Options LightOrBold = LightAndBold
    choice Light = light
    choice Bold = bold
