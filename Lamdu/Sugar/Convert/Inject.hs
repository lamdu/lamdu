{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import           Prelude.Compat


import           Control.Lens.Operators
import           Control.MonadA (MonadA)

import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActionsWithSetToInner)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

convert :: (MonadA m, Monoid a) => V.Inject (Val (Input.Payload m a)) -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Inject tag val) exprPl =
    -- TODO: Lots of duplication here from getField, generalize both!
    Inject
    { _iMVal = mVal
    , _iTag =
        TagG
        { _tagInstance = EntityId.ofInjectTag entityId
        , _tagVal = tag
        , _tagGName = UniqueId.toGuid tag
        }
    }
    & traverse ConvertM.convertSubexpression
    <&> BodyInject
    >>= addActionsWithSetToInner exprPl val
    <&> rPayload . plData <>~ hiddenPls
    where
        entityId = exprPl ^. Input.entityId
        (mVal, hiddenPls) =
            case val ^. V.body of
            V.BLeaf V.LRecEmpty -> (Nothing, val ^. V.payload . Input.userData)
            _ -> (Just val, mempty)
