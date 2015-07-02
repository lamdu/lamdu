{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.List
    ( cons
    , nil
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, void, MonadPlus(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.MonadA (MonadA)
import           Data.Foldable (Foldable)
import qualified Data.Map.Utils as MapUtils
import           Data.Maybe.Utils (maybeToMPlus)
import           Data.Monoid (Monoid(..))
import           Data.Store.Transaction (Transaction)
import           Data.Traversable (Traversable(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.RecordVal as RecordVal
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

nil ::
    (MonadA m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
nil (V.Nom tid val) exprPl =
    do
        specialFunctions <-
            lift $ (^. ConvertM.scSpecialFunctions) <$> ConvertM.readContext
        guard $ tid == Anchors.sfList specialFunctions
        injTag <- val ^? V.body . ExprLens._BInject . V.injectTag & maybeToMPlus
        guard $ injTag == Builtins.nilTag
        let mkListActions exprS =
                ListActions
                { addFirstItem = mkListAddFirstItem specialFunctions exprS
                , replaceNil = EntityId.ofValI <$> DataOps.setToHole exprS
                }
        List
            { lValues = []
            , lMActions = mkListActions <$> exprPl ^. Input.mStored
            , lNilEntityId = exprPl ^. Input.entityId
            }
            & BodyList & addActions exprPl
            <&> rPayload . plData
                <>~ val ^. ExprLens.subExprPayloads . Input.userData
            & lift

mkListAddFirstItem ::
    MonadA m =>
    Anchors.SpecialFunctions -> ExprIRef.ValIProperty m -> T m EntityId
mkListAddFirstItem specialFunctions =
    fmap (EntityId.ofValI . snd) . DataOps.addListItem specialFunctions

mkListItem ::
    (MonadA m, Monoid a) =>
    ExpressionU m a ->
    Input.Payload m a -> Val (Input.Payload m a) -> Maybe (T m EntityId) ->
    ListItem m (ExpressionU m a)
mkListItem listItemExpr exprPl tailI mAddNextItem =
    ListItem
    { _liExpr =
        listItemExpr
    , _liMActions =
        do
            addNext <- mAddNextItem
            exprProp <- exprPl ^. Input.mStored
            argProp <- tailI ^. V.payload . Input.mStored
            return ListItemActions
                { _itemAddNext = addNext
                , _itemDelete = void $ replaceWith exprProp argProp
                }
    }

data ConsParams a = ConsParams
    { cpHead :: a
    , cpTail :: a
    } deriving (Functor, Foldable, Traversable)

getSugaredHeadTail ::
    (MonadPlus m, Monoid a) =>
    ExpressionU f a -> m (ConsParams (ExpressionU f a))
getSugaredHeadTail valS =
    do
        Record [headField, tailField] ClosedRecord{} _ <-
            maybeToMPlus $ valS ^? rBody . _BodyRecord
        guard $ Builtins.headTag == headField ^. rfTag . tagVal
        guard $ Builtins.tailTag == tailField ^. rfTag . tagVal
        return ConsParams
            { cpHead = headField ^. rfExpr
            , cpTail = tailField ^. rfExpr
            }

consTags :: ConsParams T.Tag
consTags = ConsParams Builtins.headTag Builtins.tailTag

valConsParams :: Val a -> Maybe ([a], ConsParams (Val a))
valConsParams val =
    do
        recTail ^? ExprLens.valRecEmpty
        consParams <- MapUtils.matchKeys consTags fields
        let payloads = recTail ^. V.payload : consParams ^.. traverse . _1
        return (payloads, consParams <&> snd)
    where
        (fields, recTail) = RecordVal.unpack val

cons ::
    (MonadA m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
cons (V.Nom nomId (Val injPl (V.BInject (V.Inject tag argI)))) exprPl =
    do
        specialFunctions <-
            ConvertM.readContext & lift <&> (^. ConvertM.scSpecialFunctions)
        tag == Builtins.consTag & guard
        nomId == Anchors.sfList specialFunctions & guard
        argS <- ConvertM.convertSubexpression argI & lift
        ConsParams headS tailS <- getSugaredHeadTail argS
        (pls, ConsParams _headI tailI) <- valConsParams argI & maybeToMPlus
        List innerValues innerListMActions nilGuid <-
            tailS ^? rBody . _BodyList & maybeToMPlus
        let listItem =
                mkListItem headS exprPl tailI
                (addFirstItem <$> innerListMActions)
                & liExpr . rPayload . plData <>~ mconcat
                [ tailS ^. rPayload . plData
                , pls ^. traverse . Input.userData
                , injPl ^. Input.userData
                ]
        let mListActions =
                do
                    exprS <- exprPl ^. Input.mStored
                    innerListActions <- innerListMActions
                    pure ListActions
                        { addFirstItem = mkListAddFirstItem specialFunctions exprS
                        , replaceNil = replaceNil innerListActions
                        }
        List (listItem : innerValues) mListActions nilGuid
            & BodyList
            & addActions exprPl & lift

cons _ _ = mzero
