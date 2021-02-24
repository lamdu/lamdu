{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Tag
    ( Tag(..), tagName, tagVal, tagInstance
    , TagOption(..), toInfo, toPick
    , TagChoice(..), tcOptions, tcNewTag, tcAnon
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

data TagChoice name i o a = TagChoice
    { _tcOptions :: i [TagOption name o a]
    , _tcNewTag :: i (TagOption name o a)
    , -- In some cases, like let-items, single params,
      -- the user does not have to choose a tag and can choose to have
      -- an auto-generated name instead.
      _tcAnon :: Maybe (o (EntityId, a))
    } deriving (Functor, Generic)

-- | A mutable tag (that can be replaced with a different tag)
data TagRef name i o = TagRef
    { _tagRefTag :: Tag name
    , _tagRefReplace :: TagChoice name i o ()
    , _tagRefJumpTo :: Maybe (o EntityId)
    } deriving Generic

traverse Lens.makeLenses [''Tag, ''TagOption, ''TagRef, ''TagChoice] <&> concat
