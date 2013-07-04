module Lamdu.Sugar.Convert.List
  ( convert
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (guard, MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (justToLeft, leftToJust)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.MonadA (MonadA)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (First(..), Monoid(..), (<>))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable1)
import Lamdu.Sugar.Convert.Infer (ExprMM, PayloadMM)
import Lamdu.Sugar.Convert.Monad (SugarM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Sugar.Convert.Expression as SugarExpr
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as SugarM

convert ::
  (MonadA m, Typeable1 m, Monoid a) =>
  Expr.Apply (ExprMM m a) ->
  ExpressionU m a ->
  PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
convert app argS exprPl = leftToJust $ do
  justToLeft $ nil app exprPl
  justToLeft $ cons app argS exprPl

nil ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Apply (ExprMM m a) ->
  PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
nil app@(Expr.Apply funcI _) exprPl = do
  specialFunctions <-
    lift $ (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  let
    mkListActions exprS =
      ListActions
      { addFirstItem = mkListAddFirstItem specialFunctions exprS
      , replaceNil = ExprIRef.exprGuid <$> DataOps.setToHole exprS
      }
  guard $
    Lens.anyOf ExprLens.exprDefinitionRef
    (== Anchors.sfNil specialFunctions) funcI
  (lift . SugarExpr.make exprPl . BodyList)
    List
    { lValues = []
    , lMActions = mkListActions <$> exprPl ^. SugarInfer.plStored
    , lNilGuid = exprPl ^. SugarInfer.plGuid
    }
    <&> rPayload . plData <>~ app ^. Lens.traversed . Lens.traversed . SugarInfer.plData

mkListAddFirstItem ::
  MonadA m => Anchors.SpecialFunctions (Tag m) -> SugarInfer.Stored m -> T m Guid
mkListAddFirstItem specialFunctions =
  fmap (ExprIRef.exprGuid . snd) . DataOps.addListItem specialFunctions

isCons ::
  Anchors.SpecialFunctions t ->
  ExprIRef.Expression t a -> Bool
isCons specialFunctions =
  Lens.anyOf
  (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprDefinitionRef)
  (== Anchors.sfCons specialFunctions)

mkListItem ::
  (MonadA m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a ->
  PayloadMM m a -> ExprMM m a -> Maybe (T m Guid) ->
  ListItem m (ExpressionU m a)
mkListItem listItemExpr recordArgS exprPl tailI mAddNextItem =
  ListItem
  { _liExpr =
    listItemExpr
    & rPayload . plData <>~ recordArgS ^. rPayload . plData
  , _liMActions = do
      addNext <- mAddNextItem
      exprProp <- exprPl ^. SugarInfer.plStored
      argProp <- tailI ^. SugarInfer.exprStored
      return ListItemActions
        { _itemAddNext = addNext
        , _itemDelete = SugarInfer.replaceWith exprProp argProp
        }
  }

guardHeadTailTags ::
  MonadPlus m =>
  Anchors.SpecialFunctions t ->
  Lens.Getting (First Guid) s Guid ->
  s -> s -> m ()
guardHeadTailTags specialFunctions tagLens hd tl = do
  guardTag Anchors.sfHeadTag hd
  guardTag Anchors.sfTailTag tl
  where
    guardTag tag x =
      guard . (== tag specialFunctions) =<<
      maybeToMPlus (x ^? tagLens)

getSugaredHeadTail ::
  (MonadPlus m, Monoid a) =>
  Anchors.SpecialFunctions t ->
  ExpressionU f a ->
  m (a, ExpressionU f a, ExpressionU f a)
getSugaredHeadTail specialFunctions argS = do
  Record KVal (FieldList [headField, tailField] _) <-
    maybeToMPlus $ argS ^? rBody . _BodyRecord
  guardHeadTailTags specialFunctions
    (rfTag . rBody . _BodyTag . tagGuid) headField tailField
  return
    ( [headField, tailField] ^. Lens.traversed . rfTag . rPayload . plData
    , headField ^. rfExpr
    , tailField ^. rfExpr
    )

getExprHeadTail ::
  MonadPlus m =>
  Anchors.SpecialFunctions t ->
  Expr.Expression def a ->
  m (Expr.Expression def a, Expr.Expression def a)
getExprHeadTail specialFunctions argI = do
  [(headTagI, headExprI), (tailTagI, tailExprI)] <-
    maybeToMPlus $ argI ^? ExprLens.exprKindedRecordFields KVal
  guardHeadTailTags specialFunctions ExprLens.exprTag headTagI tailTagI
  return (headExprI, tailExprI)

cons ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Apply (ExprMM m a) -> ExpressionU m a -> PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
cons (Expr.Apply funcI argI) argS exprPl = do
  specialFunctions <- lift $ (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  (hidden, headS, tailS) <- getSugaredHeadTail specialFunctions argS
  (_headI, tailI) <- getExprHeadTail specialFunctions argI
  List innerValues innerListMActions nilGuid <- maybeToMPlus $ tailS ^? rBody . _BodyList
  guard $ isCons specialFunctions funcI
  let
    listItem =
      mkListItem headS argS exprPl tailI
      (addFirstItem <$> innerListMActions)
      & liExpr . rPayload . plData <>~
        (funcI ^. Lens.traversed . SugarInfer.plData <>
         tailS ^. rPayload . plData <>
         hidden)
    mListActions = do
      exprS <- exprPl ^. SugarInfer.plStored
      innerListActions <- innerListMActions
      pure ListActions
        { addFirstItem = mkListAddFirstItem specialFunctions exprS
        , replaceNil = replaceNil innerListActions
        }
  lift . SugarExpr.make exprPl . BodyList $
    List (listItem : innerValues) mListActions nilGuid
