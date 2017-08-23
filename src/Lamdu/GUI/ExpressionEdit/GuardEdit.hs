{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GuardEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeGuardRow ::
    Monad m =>
    Transaction m Sugar.EntityId -> WithTextPos View -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui m -> ExpressionGui m ->ExpressionGui m)
makeGuardRow delete prefixLabel entityId =
    do
        label <- ExpressionGui.grammarLabel "if "
        colon <- ExpressionGui.grammarLabel ": "
        config <- Lens.view Config.config
        let eventMap =
                delete <&> WidgetIds.fromEntityId
                & Widget.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Guard", "Delete"])
        vbox <- Responsive.vboxSpaced
        indent <- ResponsiveExpr.indent
        return $
            \cond result ->
            let condRow = prefixLabel /|/ label /|/ cond /|/ colon
            in
            Responsive.hboxVertFallback Responsive.disambiguationNone
            id [condRow, result]
            (vbox [condRow, indent animId result])
            & E.weakerEvents eventMap
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId

makeElseIf ::
    Monad m =>
    Sugar.GuardElseIf m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m) -> ExprGuiM m (ExpressionGui m)
makeElseIf (Sugar.GuardElseIf scopes entityId cond res delete) makeRest =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        let mInnerScope = lookupMKey <$> mOuterScopeId <*> scopes
        -- TODO: green evaluation backgrounds, "◗"?
        elseLabel <- ExpressionGui.grammarLabel "else "
        Responsive.vboxSpaced <*>
            sequence
            [ makeGuardRow delete elseLabel entityId
                <*> ExprGuiM.makeSubexpression cond
                <*> ExprGuiM.makeSubexpression res
            , makeRest
            ]
            -- TODO: scope mapping should also apply to later else-ifs and else
            & ExprGuiM.withLocalMScopeId mInnerScope
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromEntityId entityId))
    where
        -- TODO: cleaner way to write this?
        lookupMKey k m = k >>= (`Map.lookup` m)

make ::
    Monad m =>
    Sugar.Guard m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make guards pl =
    do
        ifRow <-
            makeGuardRow (guards ^. Sugar.gDeleteIf) Element.empty (pl ^. Sugar.plEntityId)
            <*> ExprGuiM.makeSubexpression (guards ^. Sugar.gIf)
            <*> ExprGuiM.makeSubexpression (guards ^. Sugar.gThen)
        let makeElse =
                sequence
                [ ExpressionGui.grammarLabel "else: " <&> Responsive.fromTextView
                , ExprGuiM.makeSubexpression (guards ^. Sugar.gElse)
                ]
                <&> Responsive.box Responsive.disambiguationNone
        elses <- foldr makeElseIf makeElse (guards ^. Sugar.gElseIfs)
        Responsive.vboxSpaced ?? [ifRow, elses]
    & ExpressionGui.stdWrapParentExpr pl

