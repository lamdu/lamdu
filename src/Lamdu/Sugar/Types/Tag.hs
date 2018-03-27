{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Tag
    ( Tag(..), tagInfo, tagSelection
    , TagInfo(..), tagName, tagVal, tagInstance
    , TagSelection(..), tsOptions, tsNewTag, tsAnon
    , TagOption(..), toInfo, toPick
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data TagInfo name = TagInfo
    { _tagName :: name
    , _tagInstance :: EntityId -- Unique across different uses of a tag
    , _tagVal :: T.Tag
    } deriving (Eq, Ord, Show)

data TagOption name m a = TagOption
    { _toInfo :: TagInfo name
    , _toPick :: m a
    } deriving (Functor, Foldable, Traversable)

data TagSelection name m a = TagSelection
    { _tsOptions :: m [TagOption name m a]
    , -- Ideally tsNewTag would be an additional TagOption,
      -- and this would also fix animation artifacts for picking new tags.
      -- However that would require making a consistent new tag,
      -- which would require either a new Revision.Deltum feature or a Sugar cache.
      _tsNewTag :: m (TagInfo name, a)
    , -- In some cases, like let-items, single params,
      -- the user does not have to choose a tag and can choose to have
      -- an auto-generated name instead.
      _tsAnon :: Maybe (m (EntityId, a))
    } deriving Functor

data Tag name m = Tag
    { _tagInfo :: TagInfo name
    , _tagSelection :: TagSelection name m ()
    }

instance Show name => Show (Tag name m) where
    show (Tag info _) = "(Tag " ++ show info ++ ")"

Lens.makeLenses ''Tag
Lens.makeLenses ''TagInfo
Lens.makeLenses ''TagOption
Lens.makeLenses ''TagSelection
