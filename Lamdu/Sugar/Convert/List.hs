{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.List
  ( cons
  , nil
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (guard, MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.MonadA (MonadA)
import Data.Foldable (Foldable)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..), (<>))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable1)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import qualified Control.Lens as Lens
import qualified Data.Map.Utils as MapUtils
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.RecordVal as RecordVal
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as ConvertM

nil ::
  (Typeable1 m, MonadA m, Monoid a) =>
  V.GlobalId ->
  InputPayload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
nil globId exprPl = do
  specialFunctions <-
    lift $ (^. ConvertM.scSpecialFunctions) <$> ConvertM.readContext
  guard $ globId == ExprIRef.globalId (Anchors.sfNil specialFunctions)
  let
    mkListActions exprS =
      ListActions
      { addFirstItem = mkListAddFirstItem specialFunctions exprS
      , replaceNil = ExprIRef.valIGuid <$> DataOps.setToHole exprS
      }
  (lift . ConvertExpr.make exprPl . BodyList)
    List
    { lValues = []
    , lMActions = mkListActions <$> exprPl ^. ipStored
    , lNilGuid = exprPl ^. ipGuid
    }

mkListAddFirstItem ::
  MonadA m => Anchors.SpecialFunctions (Tag m) -> Stored m -> T m Guid
mkListAddFirstItem specialFunctions =
  fmap (ExprIRef.valIGuid . snd) . DataOps.addListItem specialFunctions

mkListItem ::
  (MonadA m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a ->
  InputPayload m a -> InputExpr m a -> Maybe (T m Guid) ->
  ListItem m (ExpressionU m a)
mkListItem listItemExpr recordArgS exprPl tailI mAddNextItem =
  ListItem
  { _liExpr =
    listItemExpr
    & rPayload . plData <>~ recordArgS ^. rPayload . plData
  , _liMActions = do
      addNext <- mAddNextItem
      exprProp <- exprPl ^. ipStored
      argProp <- tailI ^. SugarInfer.exprStored
      return ListItemActions
        { _itemAddNext = addNext
        , _itemDelete = SugarInfer.replaceWith exprProp argProp
        }
  }

data ConsParams a = ConsParams
  { cpHead :: a
  , cpTail :: a
  } deriving (Functor, Foldable, Traversable)

getSugaredHeadTail ::
  (MonadPlus m, Monoid a) =>
  Anchors.SpecialFunctions t ->
  ExpressionU f a ->
  m (ConsParams (ExpressionU f a))
getSugaredHeadTail Anchors.SpecialFunctions{..} argS = do
  Record (FieldList [headField, tailField] _) <-
    maybeToMPlus $ argS ^? rBody . _BodyRecord
  guard $ sfHeadTag == headField ^. rfTag . tagGId
  guard $ sfTailTag == tailField ^. rfTag . tagGId
  return ConsParams
    { cpHead = headField ^. rfExpr
    , cpTail = tailField ^. rfExpr
    }

consTags :: Anchors.SpecialFunctions t -> ConsParams T.Tag
consTags Anchors.SpecialFunctions{..} = ConsParams sfHeadTag sfTailTag

valConsParams ::
  Anchors.SpecialFunctions t -> Val a -> Maybe ([a], ConsParams (Val a))
valConsParams specialFunctions val = do
  recTail ^? ExprLens.valRecEmpty
  consParams <- MapUtils.matchKeys (consTags specialFunctions) fields
  let payloads = recTail ^. V.payload : consParams ^.. traverse . _1
  return (payloads, consParams <&> snd)
  where
    (fields, recTail) = RecordVal.unpack val

cons ::
  (Typeable1 m, MonadA m, Monoid a) =>
  V.Apply (InputExpr m a) -> ExpressionU m a -> InputPayload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
cons (V.Apply funcI argI) argS exprPl = do
  specialFunctions <-
    lift $ (^. ConvertM.scSpecialFunctions) <$> ConvertM.readContext
  let consGlobalId = ExprIRef.globalId $ Anchors.sfCons specialFunctions
  guard $ Lens.anyOf ExprLens.valGlobal (== consGlobalId) funcI
  ConsParams headS tailS <- getSugaredHeadTail specialFunctions argS
  (pls, ConsParams _headI tailI) <-
    maybeToMPlus $ valConsParams specialFunctions argI
  List innerValues innerListMActions nilGuid <- maybeToMPlus $ tailS ^? rBody . _BodyList
  let
    listItem =
      mkListItem headS argS exprPl tailI
      (addFirstItem <$> innerListMActions)
      & liExpr . rPayload . plData <>~
        (funcI ^. Lens.traversed . ipData <>
         tailS ^. rPayload . plData)
    mListActions = do
      exprS <- exprPl ^. ipStored
      innerListActions <- innerListMActions
      pure ListActions
        { addFirstItem = mkListAddFirstItem specialFunctions exprS
        , replaceNil = replaceNil innerListActions
        }
  lift . ConvertExpr.make exprPl . BodyList $
    List (listItem : innerValues) mListActions nilGuid
    <&> rPayload . plData <>~ (pls ^. traverse . ipData)
