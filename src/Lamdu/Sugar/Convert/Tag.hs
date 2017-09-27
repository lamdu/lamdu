{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Tag
    ( convertTag
    ) where

import qualified Data.Set as Set
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertTag :: Monad m => TagInfo -> Set T.Tag -> (T.Tag -> Transaction m TagInfo) -> ConvertM m (Tag UUID m)
convertTag info@(TagInfo _ tag) forbiddenTags setTag =
    ConvertM.readContext <&> (^. ConvertM.scCodeAnchors) <&> Anchors.tags
    <&>
    \publishedTags ->
    Tag
    { _tagInfo = info
    , _tagName = UniqueId.toUUID tag
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
        toOption x = (UniqueId.toUUID x, x)
