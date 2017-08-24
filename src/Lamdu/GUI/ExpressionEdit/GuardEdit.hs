{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveTraversable, TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.GuardEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Functor.Compose (Compose(..))
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Row a = Row
    { _rIndentId :: AnimId
    , _rKeyword :: a
    , _rPredicate :: a
    , _rResult :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Row

makeGuardRow ::
    Monad m =>
    Transaction m Sugar.EntityId -> WithTextPos View -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui m -> ExpressionGui m -> Row (ExpressionGui m))
makeGuardRow delete prefixLabel entityId =
    do
        label <- ExpressionGui.grammarLabel "if "
        colon <- ExpressionGui.grammarLabel ": "
        let keyword = prefixLabel /|/ label & Responsive.fromTextView
        config <- Lens.view Config.config
        let eventMap =
                delete <&> WidgetIds.fromEntityId
                & Widget.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Guard", "Delete"])
        return $
            \cond result ->
            Row indentAnimId keyword (E.weakerEvents eventMap (cond /|/ colon)) (E.weakerEvents eventMap result)
    where
        indentAnimId = WidgetIds.fromEntityId entityId & Widget.toAnimId

makeElseIf ::
    Monad m =>
    Sugar.GuardElseIf m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m [Row (ExpressionGui m)] -> ExprGuiM m [Row (ExpressionGui m)]
makeElseIf (Sugar.GuardElseIf scopes entityId cond res delete) makeRest =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        let mInnerScope = lookupMKey <$> mOuterScopeId <*> scopes
        -- TODO: green evaluation backgrounds, "◗"?
        elseLabel <- ExpressionGui.grammarLabel "el"
        (:)
            <$>
            ( makeGuardRow delete elseLabel entityId
                <*> ExprGuiM.makeSubexpression cond
                <*> ExprGuiM.makeSubexpression res
            )
            <*>  makeRest
            & Reader.local (Element.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromEntityId entityId))
            & ExprGuiM.withLocalMScopeId mInnerScope
    where
        -- TODO: cleaner way to write this?
        lookupMKey k m = k >>= (`Map.lookup` m)

makeElse :: Monad m => Sugar.Guard m (ExprGuiT.SugarExpr m) -> ExprGuiM m (Row (ExpressionGui m))
makeElse guards =
    ( Row elseAnimId
        <$> (ExpressionGui.grammarLabel "else" <&> Responsive.fromTextView)
        <*> (ExpressionGui.grammarLabel ": " & Reader.local (Element.animIdPrefix .~ elseAnimId) <&> Responsive.fromTextView)
    ) <*> ExprGuiM.makeSubexpression (guards ^. Sugar.gElse)
    where
        elseAnimId = Widget.toAnimId elseId
        elseId = WidgetIds.fromExprPayload (guards ^. Sugar.gElse . Sugar.rPayload)

verticalRowRender :: Monad m => ExprGuiM m (Row (ExpressionGui m) -> ExpressionGui m)
verticalRowRender =
    do
        indent <- ResponsiveExpr.indent
        vbox <- Responsive.vboxSpaced
        box <- Options.boxSpaced ?? Options.disambiguationNone
        return $
            \row ->
            vbox
            [ box [row ^. rKeyword, row ^. rPredicate]
            , indent (row ^. rIndentId) (row ^. rResult)
            ]

renderRows :: Monad m => ExprGuiM m ([Row (ExpressionGui m)] -> ExpressionGui m)
renderRows =
    do
        vspace <- Spacer.getSpaceSize <&> (^._2)
        -- TODO: better way to make space between rows in grid??
        let spaceAbove = (<&> Element.assymetricPad (Vector2 0 vspace) 0)
        let prepareRows [] = []
            prepareRows [x, y] = [prep2 x, spaceAbove (prep2 y)]
            prepareRows (x:xs) = x : (xs <&> spaceAbove)
        vert <- verticalRowRender
        vbox <- Responsive.vboxSpaced
        return $
            \rows ->
            vbox (rows <&> vert)
            & Options.tryWideLayout Options.table (Compose (prepareRows rows))
    where
        -- When there's only "if" and "else", we want to merge the predicate with the keyword
        -- because there are no several predicates to be aligned
        prep2 row =
            row
            & rKeyword .~ Options.box Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
            & rPredicate .~ Element.empty

make ::
    Monad m =>
    Sugar.Guard m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make guards pl =
    renderRows
    <*>
    ( (:)
        <$> makeIf
        <*> foldr makeElseIf (makeElse guards <&> (:[])) (guards ^. Sugar.gElseIfs)
    )
    & Widget.assignCursor myId (WidgetIds.fromExprPayload (guards ^. Sugar.gIf . Sugar.rPayload))
    & ExpressionGui.stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl
        makeIf =
            makeGuardRow (guards ^. Sugar.gDeleteIf) Element.empty (pl ^. Sugar.plEntityId)
            <*> ExprGuiM.makeSubexpression (guards ^. Sugar.gIf)
            <*> ExprGuiM.makeSubexpression (guards ^. Sugar.gThen)
