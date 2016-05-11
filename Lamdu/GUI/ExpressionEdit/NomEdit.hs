{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui
    ( ExpressionGui, Precedence, precLeft, precRight, (<||), (||>) )
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Parens (stdWrapParenify)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

hover :: Monad m => ExprGuiM m (Layout n -> Layout n -> Layout n)
hover =
    ExpressionGui.liftLayers
    <&> (\lift gui place -> lift gui `Layout.hoverInPlaceOf` place)

type T = Transaction

data ShowName = NameHovering | NameShowing | NameCollapsed

type LayoutFunc m f =
    Widget.Id -> -- nomId
    ShowName ->
    ExprGuiM m (
        Layout (T f Widget.EventResult) -> -- label
        ExpressionGui f -> -- name gui
        ExpressionGui f -> -- subexpr gui
        ExpressionGui f)

expandingName ::
    Monad m =>
    (Layout (T f Widget.EventResult) -> ExpressionGui f -> ExpressionGui f) ->
    LayoutFunc m f
expandingName (#>) nomId showName =
    do
        space <- ExpressionGui.stdHSpace
        addBg <- ExpressionGui.addValBGWithColor Config.valNomBGColor nomId
        h <- hover
        return $
            \label nameGui subexprGui ->
            let nameShowing =
                    (nameGui ExprGuiT.LayoutWide #> const label)
                    ExprGuiT.LayoutWide
                    & Layout.widget %~ addBg
            in  case showName of
                NameCollapsed -> label & Layout.widget %~ addBg
                NameShowing -> nameShowing
                NameHovering -> nameShowing `h` label
                #> (space #> subexprGui)

makeToNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom = mkNomGui precLeft "«" $ expandingName (||>)

makeFromNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom = mkNomGui precRight "»" $ expandingName (flip (<||))

nomPrecedence :: Int
nomPrecedence = 9

mkNomGui ::
    Monad m =>
    Lens.ASetter' Precedence Int -> String -> LayoutFunc m m ->
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
mkNomGui nameSidePrecLens str layout nom@(Sugar.Nominal _ val) pl =
    ExprGuiM.withLocalPrecedence (nameSidePrecLens .~ 0) $
    stdWrapParenify pl
    (ExpressionGui.MyPrecedence (fromIntegral nomPrecedence)) $
    \myId ->
    do
        let nomId = Widget.joinId myId ["nom"]
        let nameId = Widget.joinId nomId ["name"]
        isSelected <- WE.isSubCursor nomId & ExprGuiM.widgetEnv
        let nameShowing
                | ExprGuiT.plOfHoleResult pl = NameShowing
                | isSelected = NameHovering
                | otherwise = NameCollapsed
        layout nomId nameShowing
            <*> (ExpressionGui.grammarLabel str (Widget.toAnimId myId)
                <&> if isSelected then id
                    else Layout.widget %~ Widget.takesFocus (const (pure nameId))
                )
            <*> (ExpressionGui.makeFocusableView nameId <*> mkNameGui nom nameId)
            <*> ExprGuiM.makeSubexpression (nameSidePrecLens .~ nomPrecedence+1) val
    & ExprGuiM.assignCursor myId valId
    where
        valId = val ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

mkNameGui ::
    Monad m => Sugar.Nominal (Name m) a -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
mkNameGui (Sugar.Nominal tidg _val) nameId =
    ExpressionGui.makeNameView (tidg ^. Sugar.tidgName) (Widget.toAnimId nameId)
    <&> ExpressionGui.fromValueWidget
