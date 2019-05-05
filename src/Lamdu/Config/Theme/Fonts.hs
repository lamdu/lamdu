-- | The Fonts component of the Lamdu Theme
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Config.Theme.Fonts
    ( Fonts(..), FontSize
    , base, help, literalText, autoName, literalBytes
    , binders, debugInfo
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Fonts a = Fonts
    { _base :: a
    , _help :: a
    , _literalText :: a
    , _literalBytes :: a
    , _autoName :: a
    , _binders :: a
    , _debugInfo :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
JsonTH.derivePrefixed "_" ''Fonts
Lens.makeLenses ''Fonts

type FontSize = Float
