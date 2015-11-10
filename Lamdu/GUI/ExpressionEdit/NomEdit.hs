{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui, Precedence, precLeft, precRight)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

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
    Widget.Id -> -- nomId
    ExpressionGui m -> -- label
    ExpressionGui m -> -- name gui
    ExpressionGui m -> -- subexpr gui
    Bool -> -- show name
    ExprGuiM m (ExpressionGui m)

expandingName ::
    MonadA m =>
    ([ExpressionGui m] -> ExpressionGui m -> ExpressionGui m) ->
    LayoutFunc m
expandingName namePos nomId label nameGui subexprGui showName =
    do
        space <- ExpressionGui.stdSpace
        namePos [nameGui | showName] label
            & ExpressionGui.egWidget %%~
                ExpressionGui.addValBGWithColor Config.valNomBGColor nomId
            <&> (:[]) <&> (`namePos` space)
            <&> (:[]) <&> (`namePos` subexprGui)

makeToNom ::
    MonadA m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom = mkNomGui precLeft "«" $ expandingName addLeft

makeFromNom ::
    MonadA m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom = mkNomGui precRight "»" $ expandingName addRight

nomPrecedence :: Int
nomPrecedence = 9

mkNomGui ::
    Monad m => Lens.ASetter' Precedence Int ->
    String -> LayoutFunc m ->
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
mkNomGui nameSidePrecLens str layout nom@(Sugar.Nominal _ val) pl =
    ExprGuiM.withLocalPrecedence (nameSidePrecLens .~ 0) $
    ExpressionGui.stdWrapParenify pl
    (ExpressionGui.MyPrecedence (fromIntegral nomPrecedence)) $
    \myId ->
    do
        let nomId = Widget.joinId myId ["nom"]
        isSelected <- WE.isSubCursor nomId & ExprGuiM.widgetEnv
        let nameId = Widget.joinId nomId ["name"]
        label <-
            ExpressionGui.grammarLabel str (Widget.toAnimId myId)
            <&> if isSelected then id
                else ExpressionGui.egWidget %~ Widget.takesFocus (const (pure nameId))
        nameEdit <-
            mkNameGui nom nameId
            >>= ExpressionGui.makeFocusableView nameId
        subexprEdit <-
            ExprGuiM.makeSubexpression
            (nameSidePrecLens .~ nomPrecedence+1) val
        let mk = layout nomId label nameEdit subexprEdit
        if ExprGuiT.plOfHoleResult pl
            then mk True
            else do
                compact <- mk False
                if isSelected
                    then mk True >>= (`hover` compact)
                    else return compact
    & ExprGuiM.assignCursor myId valId
    where
        valId = val ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

mkNameGui ::
    MonadA m => Sugar.Nominal (Name m) a -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
mkNameGui (Sugar.Nominal tidg _val) nameId =
    ExpressionGui.makeNameView (tidg ^. Sugar.tidgName) (Widget.toAnimId nameId)
    <&> ExpressionGui.fromValueWidget
