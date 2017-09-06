{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.GUI.ExpressionEdit.Composite
    ( destCursorId
    ) where

import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

destCursorId ::
    [Sugar.CompositeItem name n (Sugar.Expression name n p)] ->
    Sugar.EntityId -> Sugar.EntityId
destCursorId [] defDestId = defDestId
destCursorId (alt : _) _ = alt ^. Sugar.ciExpr . Sugar.rPayload . Sugar.plEntityId

