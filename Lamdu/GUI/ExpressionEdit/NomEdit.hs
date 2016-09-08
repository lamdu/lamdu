{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Alignment (Alignment)
import           Graphics.UI.Bottle.Widget (WidgetF)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui, Precedence, precBefore, precAfter)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Precedence as Prec
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

hover :: Monad m => ExprGuiM m (WidgetF ((,) Alignment) n -> WidgetF ((,) Alignment) n -> WidgetF ((,) Alignment) n)
hover =
    ExpressionGui.liftLayers
    <&>
    (\lift gui place -> lift gui & Widget.hoist (`Layout.hoverInPlaceOf` place))

type T = Transaction

data ShowName = NameHovering | NameShowing | NameCollapsed
data NomType = ToNom | FromNom

makeToNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom = mkNomGui precBefore "«" ToNom

makeFromNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom = mkNomGui precAfter "»" FromNom

nomPrecedence :: Int
nomPrecedence = 9

-- | Makes the arrows&name combination widget that:
-- Vertical: shows names
-- Horizontal & focused: hovers name
-- Horizontal & not focused: only arrows label
makeIndicator ::
    Monad m =>
    String -> WidgetF ((,) Alignment) (T m Widget.EventResult) ->
    NomType ->
    ShowName -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeIndicator str nameWidget nomType showName myId =
    do
        addBg <- ExpressionGui.addValBGWithColor Config.valNomBGColor myId
        label <-
            ExpressionGui.makeFocusableView myId
            <*> ExpressionGui.grammarLabel str (Widget.toAnimId myId)
        h <- hover
        let combineInd =
                case nomType of
                ToNom -> Layout.addBefore
                FromNom -> Layout.addAfter
        let nameShowing =
                combineInd Layout.Horizontal [nameWidget] label & addBg
        return $ ExprGuiT.ExpressionGui $ \layoutParams ->
            case layoutParams ^. ExprGuiT.layoutContext of
            ExprGuiT.LayoutVertical -> nameShowing
            _ ->
                case showName of
                NameCollapsed -> addBg label
                NameShowing -> nameShowing
                NameHovering -> nameShowing `h` label

mkNomGui ::
    Monad m =>
    Lens.ASetter' Precedence Int ->
    String ->
    NomType ->
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
mkNomGui nameSidePrecLens str nomType nom@(Sugar.Nominal _ val) pl =
    ExprGuiM.withLocalPrecedence (nameSidePrecLens .~ 0) $
    ExpressionGui.stdWrapParentExpr pl $
    \myId ->
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let nomId = Widget.joinId myId ["nom"]
        let needParen =
                Prec.needParens parentPrec
                (ExpressionGui.MyPrecedence (fromIntegral nomPrecedence))
        let mParenInfo
                | needParen = Widget.toAnimId nomId & Just
                | otherwise = Nothing
        isSelected <- WE.isSubCursor nomId & ExprGuiM.widgetEnv
        let showName
                | ExprGuiT.plOfHoleResult pl = NameShowing
                | isSelected = NameHovering
                | otherwise = NameCollapsed
        let nameId = Widget.joinId nomId ["name"]
        nameWidget <- mkNameWidget nom nameId
        (ExpressionGui.addAfterWithMParens mParenInfo
            <&>
            case nomType of
            ToNom -> id
            FromNom -> flip
            )
            <*> makeIndicator str nameWidget nomType showName nomId
            <*> ExprGuiM.makeSubexpression (nameSidePrecLens .~ nomPrecedence+1) val
    & ExprGuiM.assignCursor myId valId
    where
        valId = val ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

mkNameWidget ::
    Monad m => Sugar.Nominal (Name m) a -> Widget.Id ->
    ExprGuiM m (WidgetF ((,) Alignment) b)
mkNameWidget (Sugar.Nominal tidg _val) nameId =
    ExpressionGui.makeNameView (tidg ^. Sugar.tidgName) (Widget.toAnimId nameId)
    <&> Widget.fromView
    <&> Layout.fromCenteredWidget
