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
    } deriving (Eq, Ord, Generic)

data TagOption name o a = TagOption
    { _toInfo :: TagInfo name
    , _toPick :: o a
    } deriving (Functor, Foldable, Traversable)

data TagSelection name i o a = TagSelection
    { _tsOptions :: i [TagOption name o a]
    , -- Ideally tsNewTag would be an additional TagOption,
      -- and this would also fix animation artifacts for picking new tags.
      -- However that would require making a consistent new tag,
      -- which would require either a new Revision.Deltum feature or a Sugar cache.
      _tsNewTag :: Text -> o (EntityId, a)
    , -- In some cases, like let-items, single params,
      -- the user does not have to choose a tag and can choose to have
      -- an auto-generated name instead.
      _tsAnon :: Maybe (o (EntityId, a))
    } deriving (Functor, Generic)

data Tag name i o = Tag
    { _tagInfo :: TagInfo name
    , _tagSelection :: TagSelection name i o ()
    } deriving Generic

Lens.makeLenses ''Tag
Lens.makeLenses ''TagInfo
Lens.makeLenses ''TagOption
Lens.makeLenses ''TagSelection
