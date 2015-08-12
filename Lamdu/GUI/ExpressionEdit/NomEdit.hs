{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import           Data.String (IsString(..))
import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Monoid ((<>))
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

addLeft :: Layout.AddLayout w => [w] -> Layout.LayoutType w -> Layout.LayoutType w
addLeft = Layout.addBefore Layout.Horizontal

addRight :: Layout.AddLayout w => [w] -> Layout.LayoutType w -> Layout.LayoutType w
addRight = Layout.addAfter Layout.Horizontal

hover ::
    MonadA m => ExpressionGui n -> ExpressionGui n ->
    ExprGuiM m (ExpressionGui n)
hover gui place =
    gui `Layout.hoverInPlaceOf` place
    & ExpressionGui.liftLayers

type LayoutFunc m =
    Widget.Id -> -- myId
    (String -> ExprGuiM m (ExpressionGui m)) -> -- label
    ExpressionGui m -> -- name gui
    ExpressionGui m -> -- subexpr gui
    Bool -> -- show name
    ExprGuiM m (ExpressionGui m)

expandingName ::
    (MonadA m, MonadA n) => String ->
    ([ExpressionGui n] -> ExpressionGui n -> ExpressionGui n) ->
    ([ExpressionGui n] -> ExpressionGui n -> ExpressionGui n) ->
    Widget.Id -> (String -> ExprGuiM m (ExpressionGui n)) ->
    ExpressionGui n -> ExpressionGui n -> Bool ->
    ExprGuiM m (ExpressionGui n)
expandingName str namePos subExprPos myId label nameGui subexprGui showName =
    label str
    <&> namePos [nameGui | showName]
    >>= ExpressionGui.makeFocusableView myId
    <&> subExprPos [subexprGui]

makeToNom ::
    MonadA m =>
    Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom = mkNomGui $ expandingName "«" addLeft addRight

makeFromNom ::
    MonadA m =>
    Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom = mkNomGui $ expandingName "»" addRight addLeft

addBG :: MonadA m => AnimId -> Widget.Id -> Widget f -> ExprGuiM m (Widget f)
addBG suffix myId gui =
    do
        config <- ExprGuiM.readConfig
        let layer = Config.layerValFrameBG $ Config.layers config
        let color = Config.valNomBGColor config
        Widget.backgroundColor layer animId color gui & return
    where
        animId = Widget.toAnimId myId ++ suffix

mkNomGui ::
    Monad m =>
    LayoutFunc m -> Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
mkNomGui layout nom@(Sugar.Nominal _ val _) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        let label str =
                ExpressionGui.grammarLabel str (Widget.toAnimId myId)
                >>= ExpressionGui.egWidget %%~ addBG ["labelBG: " <> fromString str] nameId
        nameEdit <- mkNameGui "Wrapper" nom nameId
        subexprEdit <- ExprGuiM.makeSubexpression 0 val
        isSelected <- ExprGuiM.isExprSelected pl
        let mk = layout myId label nameEdit subexprEdit
        persistent <- mk False
        if isSelected
            then mk True >>= (`hover` persistent)
            else return persistent
    where
        nameId = Widget.joinId (WidgetIds.fromEntityId (pl ^. Sugar.plEntityId)) ["name"]

mkNameGui ::
    MonadA m =>
    String -> Sugar.Nominal (Name m) m a -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
mkNameGui docName (Sugar.Nominal tidg _val mDel) nameId =
    do
        delEventMap <- mkDelEventMap docName mDel
        ExpressionGui.makeNameView (tidg ^. Sugar.tidgName) (Widget.toAnimId nameId)
            <&> Widget.weakerEvents delEventMap
            >>= addBG ["nameBG"] nameId
    <&> ExpressionGui.fromValueWidget

mkDelEventMap ::
    MonadA m =>
    String -> Maybe (Transaction m Sugar.EntityId) ->
    ExprGuiM m (Widget.EventHandlers (Transaction m))
mkDelEventMap docName mDel =
    do
        config <- ExprGuiM.readConfig
        mDel
            <&> fmap WidgetIds.fromEntityId
            & maybe mempty
            (Widget.keysEventMapMovesCursor (Config.delKeys config) doc)
            & return
    where
        doc = E.Doc ["Edit", "Delete", docName]
