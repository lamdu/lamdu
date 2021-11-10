{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Tag
    ( Tag(..), tagName, tagVal, tagInstance
    , TagOption(..), toInfo, toPick
    , TagChoice(..), tcOptions, tcNewTag
    , TagRef(..), tagRefTag, tagRefReplace, tagRefJumpTo
    , OptionalTag(..), oTag, oPickAnon
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

data TagOption name o = TagOption
    { _toInfo :: Tag name
    , _toPick :: o ()
    } deriving Generic

data TagChoice name o = TagChoice
    { _tcOptions :: [TagOption name o]
    , _tcNewTag :: TagOption name o
    } deriving Generic

-- | A mutable tag (that can be replaced with a different tag)
data TagRef name i o = TagRef
    { _tagRefTag :: Tag name
    , _tagRefReplace :: i (TagChoice name o)
    , _tagRefJumpTo :: Maybe (o EntityId)
    } deriving Generic

data OptionalTag name i o = OptionalTag
    { _oTag :: TagRef name i o
    , _oPickAnon :: o EntityId
    } deriving Generic

traverse Lens.makeLenses [''Tag, ''TagOption, ''TagRef, ''TagChoice, ''OptionalTag] <&> concat
