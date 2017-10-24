{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, TemplateHaskell, DeriveGeneric #-}

module GUI.Momentu.State
    ( Id(..)
    , VirtualCursor(..), virtualCursor
    , EventResult(..), eCursor, eVirtualCursor, eAnimIdMapping
    , eventResultFromCursor
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import           Data.List (intercalate)
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           GUI.Momentu.Animation (AnimId)
import           GUI.Momentu.Rect (Rect)
import           Numeric.Utils (encodeHex)

import           Lamdu.Prelude

-- The virtual cursor is the focal area that would ideally match the
-- direction of user movements
newtype VirtualCursor = VirtualCursor { _virtualCursor :: Rect }
Lens.makeLenses ''VirtualCursor

newtype Id = Id
    { toAnimId :: AnimId
    } deriving (Eq, Ord, Read, Binary, Monoid)

instance Show Id where
    show (Id animId) =
        "W:" ++ intercalate ":" (map each animId)
        where
            each bs = encodeHex bs ++ "(" ++ show bs ++ ")"

data EventResult = EventResult
    { _eCursor :: Monoid.Last Id
    , _eVirtualCursor :: Monoid.Last VirtualCursor
    , _eAnimIdMapping :: Monoid.Endo AnimId
    } deriving (Generic)
Lens.makeLenses ''EventResult

instance Monoid EventResult where
    mempty = def_mempty
    mappend = def_mappend

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor c =
    EventResult
    { _eCursor = Just c & Monoid.Last
    , _eVirtualCursor = mempty
    , _eAnimIdMapping = mempty
    }
