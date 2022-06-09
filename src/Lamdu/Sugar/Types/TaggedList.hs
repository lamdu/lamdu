{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.TaggedList
    ( TaggedList(..), tlAddFirst, tlItems
    , TaggedSwappableItem(..), tsiItem, tsiSwapWithPrevious
    , TaggedListBody(..), tlHead, tlTail
    , TaggedItem(..), tiTag, tiDelete, tiValue, tiAddAfter
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Types.Tag (TagRef, TagChoice)

import           Lamdu.Prelude

data TaggedItem name i o a = TaggedItem
    { _tiTag :: TagRef name i o
    , _tiDelete :: o ()
    , _tiAddAfter :: i (TagChoice name o)
    , _tiValue :: a
    } deriving (Generic, Functor, Foldable, Traversable)

data TaggedSwappableItem name i o a = TaggedSwappableItem
    { _tsiItem :: TaggedItem name i o a
    , _tsiSwapWithPrevious :: o ()
    } deriving (Generic, Functor, Foldable, Traversable)

data TaggedListBody name i o a = TaggedListBody
    { _tlHead :: TaggedItem name i o a
        -- The 2nd tagged item onwards can be swapped with their previous item
    , _tlTail :: [TaggedSwappableItem name i o a]
    } deriving (Generic, Functor, Foldable, Traversable)

data TaggedList name i o a = TaggedList
    { _tlAddFirst :: i (TagChoice name o)
    , _tlItems :: Maybe (TaggedListBody name i o a)
    } deriving (Generic, Functor, Foldable, Traversable)

traverse Lens.makeLenses
    [ ''TaggedList, ''TaggedListBody, ''TaggedItem, ''TaggedSwappableItem
    ] <&> concat
