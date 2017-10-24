{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveGeneric #-}

module GUI.Momentu.State
    ( VirtualCursor(..), virtualCursor
    , GUIState(..), sCursor, sWidgetStates
    , Update(..), uCursor, uWidgetStateUpdates, uVirtualCursor, uAnimIdMapping
    , updateCursor, guiStateUpdate
    , HasCursor(..), subId, isSubCursor, assignCursor, assignCursorPrefix
    , HasWidgetState(..), readWidgetState, updateWidgetState
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Binary (Binary, decodeOrFail)
import           Data.Binary.Utils (encodeS)
import           Data.ByteString.Utils (lazifyBS)
import           Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           GUI.Momentu.Animation (AnimId)
import           GUI.Momentu.Rect (Rect)
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as Id

import           Lamdu.Prelude

-- The virtual cursor is the focal area that would ideally match the
-- direction of user movements
newtype VirtualCursor = VirtualCursor { _virtualCursor :: Rect }
Lens.makeLenses ''VirtualCursor

data GUIState = GUIState
    { _sCursor :: Id
    , _sWidgetStates :: Map Id ByteString
    } deriving Generic
instance Binary GUIState
Lens.makeLenses ''GUIState

data Update = Update
    { _uCursor :: Monoid.Last Id
    , _uWidgetStateUpdates :: Map Id ByteString
    , _uVirtualCursor :: Monoid.Last VirtualCursor
    , _uAnimIdMapping :: Monoid.Endo AnimId
    } deriving Generic
Lens.makeLenses ''Update

instance Monoid Update where
    mempty = def_mempty
    mappend = def_mappend

updateCursor :: Id -> Update
updateCursor c =
    Update
    { _uCursor = Just c & Monoid.Last
    , _uWidgetStateUpdates = mempty
    , _uVirtualCursor = mempty
    , _uAnimIdMapping = mempty
    }

guiStateUpdate :: Update -> GUIState -> GUIState
guiStateUpdate u s =
    case u ^. uCursor . Lens._Wrapped of
    Nothing -> s
    Just c ->
        s
        & sCursor .~ c
        & sWidgetStates %~ Map.filterWithKey f
        where
            f (Id k) _v = k `isPrefixOf` toAnimId c
    & sWidgetStates %~ mappend (u ^. uWidgetStateUpdates)

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

-- TODO: Currently widget state is cleaned for widgets whose id isn't prefix of the cursor.
-- Consider allowing all widgets to store state which are cleaned when not access while generating root widget.
-- That would put more restrictions on the root widget monad.
class HasWidgetState env where
    widgetState :: Lens' env (Map Id ByteString)

readWidgetState ::
    (HasWidgetState env, MonadReader env m, Binary a) =>
    Id -> m (Maybe a)
readWidgetState wid =
    Lens.view widgetState <&> (^. Lens.at wid) <&> (>>= f)
    where
        f x = decodeOrFail (lazifyBS x) ^? Lens._Right . _3

updateWidgetState :: Binary a => Id -> a -> Update
updateWidgetState wid val = mempty & uWidgetStateUpdates . Lens.at wid ?~ encodeS val
