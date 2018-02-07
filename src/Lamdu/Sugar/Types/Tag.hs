{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.Sugar.Types.Tag
    ( Tag(..), tagName, tagInfo, tagSelection
    , TagInfo(..), tagVal, tagInstance
    , TagSelection(..), tsOptions, tsSetTag, tsNewTag
    , TagOption(..), toInfo, toName
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data TagInfo = TagInfo
    { _tagInstance :: EntityId -- Unique across different uses of a tag
    , _tagVal :: T.Tag
    } deriving (Eq, Ord, Show)

data TagOption name = TagOption
    { _toInfo :: TagInfo
    , _toName :: name
    }

data TagSelection name m = TagSelection
    { _tsOptions :: m [TagOption name]
    , _tsSetTag :: T.Tag -> m ()
    , _tsNewTag :: m (name, TagInfo)
    }

data Tag name m = Tag
    { _tagInfo :: TagInfo
    , _tagName :: name
    , _tagSelection :: TagSelection name m
    }

Lens.makeLenses ''Tag
Lens.makeLenses ''TagInfo
Lens.makeLenses ''TagOption
Lens.makeLenses ''TagSelection
