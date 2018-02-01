{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Tag
    ( convertTag
    ) where

import qualified Data.Set as Set
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

-- forbiddenTags are sibling tags in the same record/funcParams/etc,
-- NOT type-level constraints on tags. Violation of constraints is
-- allowed, generating ordinary type errors
convertTag :: Monad m => TagInfo -> Set T.Tag -> (T.Tag -> T m EntityId) -> ConvertM m (Tag InternalName (T m))
convertTag info@(TagInfo _ tag) forbiddenTags setTag =
    ConvertM.readContext <&> (^. ConvertM.scCodeAnchors) <&> Anchors.tags
    <&>
    \publishedTags ->
    Tag
    { _tagInfo = info
    , _tagName = UniqueId.toUUID tag & InternalName
    , _tagActions =
        TagActions
        { _taChangeTag = setTag
        , _taOptions =
            Transaction.getP publishedTags
            <&> (`Set.difference` forbiddenTags)
            <&> Set.toList
            <&> map toOption
        , _taSetPublished =
            \isPublished ->
            Transaction.modP
            publishedTags
            ((if isPublished then Set.insert else Set.delete) tag)
        , _taReplaceWithNew = DataOps.genNewTag >>= setTag
        }
    }
    where
        toOption x = (UniqueId.toUUID x & InternalName, x)
