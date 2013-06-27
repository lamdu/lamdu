module Lamdu.Sugar.Convert.List
  ( convert
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (justToLeft, leftToJust)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.MonadA (MonadA)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..), (<>))
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
  let
    hiddenData = app ^. Lens.traversed . Lens.traversed . SugarInfer.plData
  (lift . SugarExpr.make exprPl . BodyList)
    (List [] (mkListActions <$> exprPl ^. SugarInfer.plStored))
    <&> rPayload . plData <>~ hiddenData

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
mkListItem listItemExpr recordArgS exprPl argI mAddNextItem =
  ListItem
  { _liExpr =
    listItemExpr
    & rPayload . plData <>~ recordArgS ^. rPayload . plData
  , _liMActions = do
      addNext <- mAddNextItem
      exprProp <- exprPl ^. SugarInfer.plStored
      argProp <- argI ^. SugarInfer.exprStored
      return ListItemActions
        { _itemAddNext = addNext
        , _itemDelete = SugarInfer.replaceWith exprProp argProp
        }
  }

cons ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Apply (ExprMM m a) -> ExpressionU m a -> PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
cons (Expr.Apply funcI argI) argS exprPl = do
  specialFunctions <- lift $ (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  Record KVal (FieldList fields@[headField, tailField] _) <-
    maybeToMPlus $ argS ^? rBody . _BodyRecord
  let
    verifyTag tag field =
      guard . (== tag specialFunctions) =<<
      maybeToMPlus (field ^? rfTag . rBody . _BodyTag . tagGuid)
  verifyTag Anchors.sfHeadTag headField
  verifyTag Anchors.sfTailTag tailField
  List innerValues innerListMActions <-
    maybeToMPlus $ tailField ^? rfExpr . rBody . _BodyList
  guard $ isCons specialFunctions funcI
  let
    listItem =
      mkListItem (headField ^. rfExpr) argS exprPl argI
      (addFirstItem <$> innerListMActions)
      & liExpr . rPayload . plData <>~
        (funcI ^. Lens.traversed . SugarInfer.plData <>
         tailField ^. rfExpr . rPayload . plData <>
         fields ^. Lens.traversed . rfTag . rPayload . plData)
    mListActions = do
      exprS <- exprPl ^. SugarInfer.plStored
      innerListActions <- innerListMActions
      pure ListActions
        { addFirstItem = mkListAddFirstItem specialFunctions exprS
        , replaceNil = replaceNil innerListActions
        }
  lift . SugarExpr.make exprPl . BodyList $
    List (listItem : innerValues) mListActions
