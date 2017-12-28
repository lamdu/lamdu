-- | For effective text-like keyboard based input, current choices
-- should be finalized without an explicit action, upon further
-- action.  For example when creating a list, and choosing an item by
-- moving the cursor to it, pressing "," or "+" to add another item
-- should finalize the selection of the currently focused item.  This
-- selection of current item is the PreEvent to the other event.

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeFamilies #-}

module GUI.Momentu.PreEvent
    ( PreEvents(..), withPreEvents
    , HasPreEvents(..)
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E

import           Lamdu.Prelude

class HasPreEvents m where
    type Event m
    listenPreEvents :: m a -> m (a, PreEvents (Event m))

concatDescs :: Text -> Text -> Text
concatDescs x y = filter (not . Text.null) [x, y] & Text.intercalate ", "

data PreEvents a = PreEvents
    { pDesc :: E.Subtitle
    , pAction :: a
    , pTextRemainder :: Text
    }
instance Monoid a => Monoid (PreEvents a) where
    mempty = PreEvents mempty mempty mempty
    mappend x y =
        PreEvents
        { pDesc = concatDescs (pDesc x) (pDesc y)
        , pAction = pAction x <> pAction y
        , pTextRemainder = pTextRemainder x <> pTextRemainder y
        }

actionText :: Lens.Traversal' (EventMap a) E.Subtitle
actionText = E.emDocs . E.docStrs . Lens.reversed . Lens.element 0

withPreEvents :: Applicative f => PreEvents (f ()) -> (Text, EventMap (f a) -> EventMap (f a))
withPreEvents pre =
    ( pTextRemainder pre
    , onEventMap
    )
    where
        onEventMap eventMap =
            eventMap
            & actionText %~ concatDescs (pDesc pre)
            <&> (pAction pre *>)
