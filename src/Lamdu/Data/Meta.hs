{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Meta
    ( DefinitionState(..)
    , SpecialArgs(..), PresentationMode
    , ParamList
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

data DefinitionState = DeletedDefinition | LiveDefinition
    deriving (Eq, Ord, Show, Generic)
instance Binary DefinitionState

data SpecialArgs a
    = Verbose
    | Object a
    | Infix a a
    deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
instance Binary a => Binary (SpecialArgs a)

type PresentationMode = SpecialArgs T.Tag

type ParamList = [T.Tag]

Lens.makePrisms ''SpecialArgs
