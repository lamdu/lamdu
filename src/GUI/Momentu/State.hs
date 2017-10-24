{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveGeneric #-}

module GUI.Momentu.State
    ( VirtualCursor(..), virtualCursor
    , Update(..), uCursor, uVirtualCursor, uAnimIdMapping
    , updateCursor
    , HasCursor(..), subId, isSubCursor, assignCursor, assignCursorPrefix
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           GUI.Momentu.Animation (AnimId)
import           GUI.Momentu.Rect (Rect)
import           GUI.Momentu.Widget.Id (Id)
import qualified GUI.Momentu.Widget.Id as Id

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

class HasCursor env where cursor :: Lens' env Id

subId :: (MonadReader env m, HasCursor env) => m (Id -> Maybe AnimId)
subId = Lens.view cursor <&> flip Id.subId

isSubCursor :: (MonadReader env m, HasCursor env) => m (Id -> Bool)
isSubCursor = subId <&> \sub prefix -> sub prefix & Lens.has Lens._Just

assignCursor ::
    (HasCursor env, MonadReader env m) =>
    Id -> Id -> m a -> m a
assignCursor src dest =
    Reader.local (cursor %~ replace)
    where
        replace c
            | c == src = dest
            | otherwise = c

assignCursorPrefix ::
    (HasCursor env, MonadReader env m) =>
    Id -> (AnimId -> Id) -> m a -> m a
assignCursorPrefix srcFolder dest =
    Reader.local (cursor %~ replace)
    where
        replace c =
            case Id.subId srcFolder c of
            Nothing -> c
            Just suffix -> dest suffix
