{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable #-}
module Lamdu.Sugar.Types.Tag
    ( Tag(..), tagName, tagInfo, tagSelection
    , TagInfo(..), tagVal, tagInstance
    , TagSelection(..), tsOptions, tsNewTag
    , TagOption(..), toInfo, toName, toPick
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data TagInfo = TagInfo
    { _tagInstance :: EntityId -- Unique across different uses of a tag
    , _tagVal :: T.Tag
    } deriving (Eq, Ord, Show)

data TagOption name m a = TagOption
    { _toInfo :: TagInfo
    , _toName :: name
    , _toPick :: m a
    } deriving (Functor, Foldable, Traversable)

data TagSelection name m a = TagSelection
    { _tsOptions :: m [TagOption name m a]
    , -- Ideally tsNewTag would be an additional TagOption,
      -- and this would also fix animation artifacts for picking new tags.
      -- However that would require making a consistent new tag,
      -- which would require either a new Data.Store feature or a Sugar cache.
      _tsNewTag :: m (name, TagInfo, a)
    } deriving Functor

data Tag name m = Tag
    { _tagInfo :: TagInfo
    , _tagName :: name
    , _tagSelection :: TagSelection name m ()
    }

Lens.makeLenses ''Tag
Lens.makeLenses ''TagInfo
Lens.makeLenses ''TagOption
Lens.makeLenses ''TagSelection
