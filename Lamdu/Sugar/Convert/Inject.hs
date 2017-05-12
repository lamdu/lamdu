{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Utils (maybeToMPlus)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, addActionsWithSetToInner)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert :: (Monad m, Monoid a) => V.Inject (Val (Input.Payload m a)) -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Inject tag valI) exprPl =
    do
        valS <- ConvertM.convertSubexpression valI & lift
        convertLabeledInject tagS valS valI exprPl & justToLeft
        convertSimple tagS valS valI exprPl & lift
    & runMatcherT
    where
        tagS =
            TagG
            { _tagInstance = exprPl ^. Input.entityId & EntityId.ofInjectTag
            , _tagVal = tag
            , _tagGName = UniqueId.toUUID tag
            }

convertLabeledInject ::
    (Monad m, Monoid a) =>
    TagG UUID -> ExpressionU m a -> Val (Input.Payload m a) ->
    Input.Payload m a -> MaybeT (ConvertM m) (ExpressionU m a)
convertLabeledInject tagS valS valI exprPl =
    do
        record <- valS ^? rBody . _BodyRecord & maybeToMPlus
        ConvertApply.convertLabeled (FuncInject tagS) record valI exprPl
            <&> rPayload . plData <>~ valS ^. rPayload . plData

convertSimple ::
    (Monad m, Monoid a) =>
    TagG UUID -> ExpressionU m a -> Val (Input.Payload m a) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertSimple tagS valS valI exprPl =
    -- TODO: Lots of duplication here from getField, generalize both!
    Inject
    { _iMVal = if isNullary then Nothing else Just valS
    , _iTag = tagS
    }
    & BodyInject
    & doAddActions
    <&> rPayload . plData <>~
        if isNullary then valS ^. rPayload . plData else mempty
    where
        doAddActions
            | isNullary = addActions exprPl
            | otherwise = addActionsWithSetToInner exprPl valI
        isNullary =
            case valS ^. rBody of
            BodyRecord record -> Lens.nullOf traverse record
            _ -> False
