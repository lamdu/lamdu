{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
module Lamdu.Data.Meta
    ( PresentationMode(..)
    , DefinitionState(..)
    , ParamList
    ) where

import           Data.Binary (Binary)
import           GHC.Generics (Generic)
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

data DefinitionState = DeletedDefinition | LiveDefinition
    deriving (Eq, Ord, Show, Generic)
instance Binary DefinitionState

data PresentationMode = OO | Verbose | Infix
    deriving (Eq, Ord, Show, Generic)
instance Binary PresentationMode

type ParamList = [T.Tag]
