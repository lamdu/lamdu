{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveGeneric #-}

module GUI.Momentu.State
    ( VirtualCursor(..), virtualCursor
    , Update(..), uCursor, uVirtualCursor, uAnimIdMapping
    , updateCursor
    ) where

import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           GUI.Momentu.Animation (AnimId)
import           GUI.Momentu.Rect (Rect)
import           GUI.Momentu.Widget.Id (Id)

import           Lamdu.Prelude

-- The virtual cursor is the focal area that would ideally match the
-- direction of user movements
newtype VirtualCursor = VirtualCursor { _virtualCursor :: Rect }
Lens.makeLenses ''VirtualCursor

data Update = Update
    { _uCursor :: Monoid.Last Id
    , _uVirtualCursor :: Monoid.Last VirtualCursor
    , _uAnimIdMapping :: Monoid.Endo AnimId
    } deriving (Generic)
Lens.makeLenses ''Update

instance Monoid Update where
    mempty = def_mempty
    mappend = def_mappend

updateCursor :: Id -> Update
updateCursor c =
    Update
    { _uCursor = Just c & Monoid.Last
    , _uVirtualCursor = mempty
    , _uAnimIdMapping = mempty
    }
