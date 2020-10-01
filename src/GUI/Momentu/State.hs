{-# LANGUAGE TemplateHaskell, DefaultSignatures, DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}

module GUI.Momentu.State
    ( VirtualCursor(..), vcRect
    , GUIState(..), sCursor, sWidgetStates
    , Update(..), uCursor, uPreferStroll, uWidgetStateUpdates, uVirtualCursor, uSetSystemClipboard
    , update
    , updateCursor, fullUpdate
    , HasCursor(..), subId, isSubCursor, assignCursor, assignCursorPrefix
    , HasState, readWidgetState, updateWidgetState
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Binary.Extended (decodeOrFail, encodeS)
import           Data.ByteString.Extended as BS
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           GUI.Momentu.Animation.Id (AnimId)
import           GUI.Momentu.Rect (Rect)
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as Id

import           GUI.Momentu.Prelude

-- The virtual cursor is the focal area that would ideally match the
-- direction of user movements
newtype VirtualCursor = VirtualCursor { _vcRect :: Rect }
Lens.makeLenses ''VirtualCursor

-- The GUIState may persist in user-specified storage (e.g: the Lamdu
-- DB), thus we do not want to store the virtual cursor there. The
-- Update type is still a convenient place to carry the virtual cursor
-- updates.

data GUIState = GUIState
    { _sCursor :: Id
    , _sWidgetStates :: Map Id ByteString
    }
    deriving stock Generic
    deriving anyclass Binary
Lens.makeLenses ''GUIState

data Update = Update
    { _uCursor :: Monoid.Last Id
    , _uPreferStroll :: Monoid.Any
    , _uWidgetStateUpdates :: Map Id ByteString
    , _uVirtualCursor :: Monoid.Last VirtualCursor
    , _uSetSystemClipboard :: Maybe Text
    }
    deriving stock Generic
    deriving (Semigroup, Monoid) via Generically Update
Lens.makeLenses ''Update

updateCursor :: Id -> Update
updateCursor c = mempty { _uCursor = Just c & Monoid.Last }

fullUpdate :: GUIState -> Update
fullUpdate (GUIState c s) = updateCursor c & uWidgetStateUpdates .~ s

update :: Has GUIState env => Update -> env -> env
update u s =
    case u ^? uCursor . Lens._Wrapped . Lens._Just of
    Nothing -> s
    Just c ->
        s
        & has . sCursor .~ c
        & has . sWidgetStates %~ Map.filterWithKey f
        where
            f k _v = Id.subId k c & Lens.has Lens._Just
    & has . sWidgetStates %~ mappend (u ^. uWidgetStateUpdates)

class HasCursor env where
    cursor :: Lens' env Id
    default cursor :: Has GUIState env => Lens' env Id
    cursor = has . sCursor

instance HasCursor GUIState where cursor = sCursor

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
        replace c = Id.subId srcFolder c & maybe c dest

-- TODO: Currently widget state is cleaned for widgets whose id isn't prefix of the cursor.
-- Consider allowing all widgets to store state which are cleaned when not access while generating root widget.
-- That would put more restrictions on the root widget monad.
type HasState env = (HasCursor env, Has GUIState env)

readWidgetState ::
    (Has GUIState env, MonadReader env m, Binary a) =>
    Id -> m (Maybe a)
readWidgetState wid =
    Lens.view (has . sWidgetStates . Lens.at wid) <&> (>>= f)
    where
        f x = decodeOrFail (BS.lazify x) ^? Lens._Right . _3

updateWidgetState :: Binary a => Id -> a -> Update
updateWidgetState wid val = mempty & uWidgetStateUpdates . Lens.at wid ?~ encodeS val
