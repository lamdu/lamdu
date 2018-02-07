{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Tag
    ( convertTag
    ) where

import qualified Control.Lens as Lens
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
convertTag ::
    Monad m =>
    T.Tag -> Set T.Tag -> (T.Tag -> EntityId) -> (T.Tag -> T m ()) ->
    ConvertM m (Tag InternalName (T m))
convertTag tag forbiddenTags mkInstance setTag =
    Lens.view ConvertM.scCodeAnchors <&> Anchors.tags
    <&>
    \publishedTags ->
    Tag
    { _tagInfo = mkInfo tag
    , _tagName = UniqueId.toUUID tag & InternalName
    , _tagSelection =
        TagSelection
        { _tsOptions =
            Transaction.getP publishedTags
            <&> (`Set.difference` forbiddenTags)
            <&> Set.toList
            <&> map toOption
        , _tsNewTag =
            do
                newTag <- DataOps.genNewTag
                setTag newTag
                pure (InternalName (UniqueId.toUUID newTag), mkInfo newTag)
        }
    }
    where
        mkInfo t = TagInfo (mkInstance t) t
        toOption x =
            TagOption
            { _toName = UniqueId.toUUID x & InternalName
            , _toInfo = mkInfo x
            , _toPick = setTag x
            }
