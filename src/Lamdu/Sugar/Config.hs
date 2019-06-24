{-# LANGUAGE TemplateHaskell, DerivingVia #-}

module Lamdu.Sugar.Config where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Sugars a = Sugars
    { -- `case X of ...`, rather than (`(\case ...) X`).
      -- Disabling this also implies disabling if-expressions.
      _caseWithArgument :: a
    , _fragment :: a
    , _labeledApply :: a
    , _letExpression :: a
    } deriving stock (Eq, Show, Functor, Generic, Generic1)
    deriving Applicative via Generically1 Sugars
JsonTH.derivePrefixed "_" ''Sugars
Lens.makeLenses ''Sugars

data Config = Config
    { _showAllAnnotations :: Bool
    , _sugarsEnabled :: Sugars Bool
    } deriving stock (Eq, Show)
JsonTH.derivePrefixed "_" ''Config
Lens.makeLenses ''Config
