{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Tag
    ( Tag(..), tagName, tagVal, tagInstance
    , TagOption(..), toInfo, toPick
    , TagReplace(..), tsOptions, tsNewTag, tsAnon
    , TagRef(..), tagRefTag, tagRefReplace, tagRefJumpTo
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data Tag name = Tag
    { _tagName :: name
    , _tagInstance :: EntityId -- Unique across different uses of a tag
    , _tagVal :: T.Tag
    } deriving (Eq, Ord, Generic)

data TagOption name o a = TagOption
    { _toInfo :: Tag name
    , _toPick :: o a
    } deriving (Functor, Foldable, Traversable)

data TagReplace name i o a = TagReplace
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

-- | A mutable tag (that can be replaced with a different tag)
data TagRef name i o = TagRef
    { _tagRefTag :: Tag name
    , _tagRefReplace :: TagReplace name i o ()
    , _tagRefJumpTo :: Maybe (o EntityId)
    } deriving Generic

Lens.makeLenses ''TagRef
Lens.makeLenses ''Tag
Lens.makeLenses ''TagOption
Lens.makeLenses ''TagReplace
