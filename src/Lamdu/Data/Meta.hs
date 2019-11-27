{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Meta
    ( DefinitionState(..)
    , SpecialArgs(..), _Verbose, _Object, _Infix
    , PresentationMode
    , ParamList
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

data DefinitionState = DeletedDefinition | LiveDefinition
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass Binary

data SpecialArgs a
    = Verbose
    | Object a
    | Infix a a
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass Binary

type PresentationMode = SpecialArgs T.Tag

type ParamList = [T.Tag]

Lens.makePrisms ''SpecialArgs
