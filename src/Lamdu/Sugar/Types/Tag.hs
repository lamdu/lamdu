{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.Sugar.Types.Tag
    ( Tag(..), tagName, tagInfo, tagActions
    , TagInfo(..), tagVal, tagInstance
    , TagActions(..), taOptions, taChangeTag
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data TagInfo = TagInfo
    { _tagInstance :: EntityId -- Unique across different uses of a tag
    , _tagVal :: T.Tag
    } deriving (Eq, Ord, Show)

data TagActions name m = TagActions
    { _taOptions :: m [(name, T.Tag)]
    , _taChangeTag :: T.Tag -> m EntityId
    }

data Tag name m = Tag
    { _tagInfo :: TagInfo
    , _tagName :: name
    , _tagActions :: TagActions name m
    }

Lens.makeLenses ''Tag
Lens.makeLenses ''TagActions
Lens.makeLenses ''TagInfo
