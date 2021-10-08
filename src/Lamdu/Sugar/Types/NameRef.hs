{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.NameRef
    ( NameRef(..), nrName, nrGotoDefinition
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data NameRef name o = NameRef
    { _nrName :: name
    , _nrGotoDefinition :: o EntityId
    } deriving Generic

Lens.makeLenses ''NameRef
