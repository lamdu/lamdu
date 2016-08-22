{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Alignment (Alignment)
import           Graphics.UI.Bottle.Widget (WidgetF)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui
    ( ExpressionGui, Precedence, precBefore, precAfter, (<||), (||>) )
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

type LayoutFunc m f =
    Bool -> --need paren
    Widget.Id -> -- nomId
    ShowName ->
    ExprGuiM m (
        WidgetF ((,) Alignment) (T f Widget.EventResult) -> -- label
        WidgetF ((,) Alignment) (T f Widget.EventResult) -> -- name gui
        ExpressionGui f -> -- subexpr gui
        ExpressionGui f)

expandingName ::
    Monad m =>
    (ExpressionGui f -> ExpressionGui f -> [ExpressionGui f]) ->
    (WidgetF ((,) Alignment) (T f Widget.EventResult) -> ExpressionGui f -> ExpressionGui f) ->
    LayoutFunc m f
expandingName vertOrder (#>) needParen nomId showName =
    do
        space <- ExpressionGui.stdHSpace <&> Layout.fromCenteredWidget
        addBg <- ExpressionGui.addValBGWithColor Config.valNomBGColor nomId
        h <- hover
        horizWithFallback <- ExpressionGui.horizVertFallback mParenInfo
        return $
            \label nameGui subexprGui ->
            let nameShowing =
                    ExprGuiT.LayoutParams
                    { ExprGuiT._layoutMode = ExprGuiT.LayoutWide
                    , ExprGuiT._layoutContext = ExprGuiT.LayoutClear
                    }
                    & (nameGui #> ExpressionGui.fromLayout label) ^. ExpressionGui.toLayout
                    & addBg
                horiz =
                    case showName of
                    NameCollapsed -> addBg label
                    NameShowing -> nameShowing
                    NameHovering -> nameShowing `h` label
                    #> (space #> subexprGui)
                vert =
                    ExprGuiT.ExpressionGui (const nameShowing)
                    `vertOrder` subexprGui
                    <&> ExpressionGui.egAlignment . _1 .~ 0
                    & ExpressionGui.vboxTopFocal
            in horiz `horizWithFallback` vert
    where
        mParenInfo
            | needParen = Widget.toAnimId nomId & Just
            | otherwise = Nothing

makeToNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom =
    mkNomGui precBefore "«" $ expandingName (\a b -> [a, b]) (||>)

makeFromNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom =
    mkNomGui precAfter "»" $ expandingName (\a b -> [b, a]) (flip (<||))

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
    ExpressionGui.stdWrapParentExpr pl $
    \myId ->
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let needParen =
                Prec.needParens parentPrec
                (ExpressionGui.MyPrecedence (fromIntegral nomPrecedence))
        let nomId = Widget.joinId myId ["nom"]
        let nameId = Widget.joinId nomId ["name"]
        isSelected <- WE.isSubCursor nomId & ExprGuiM.widgetEnv
        let nameShowing
                | ExprGuiT.plOfHoleResult pl = NameShowing
                | isSelected = NameHovering
                | otherwise = NameCollapsed
        let mTakesFocus
                | isSelected = id
                | otherwise = Widget.takesFocus (const (pure nameId))
        layout needParen nomId nameShowing
            <*> (ExpressionGui.grammarLabel str (Widget.toAnimId myId)
                <&> mTakesFocus)
            <*> (ExpressionGui.makeFocusableView nameId
                 <*> mkNameGui nom nameId)
            <*> ExprGuiM.makeSubexpression (nameSidePrecLens .~ nomPrecedence+1) val
    & ExprGuiM.assignCursor myId valId
    where
        valId = val ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

mkNameGui ::
    Monad m => Sugar.Nominal (Name m) a -> Widget.Id ->
    ExprGuiM m (WidgetF ((,) Alignment) b)
mkNameGui (Sugar.Nominal tidg _val) nameId =
    ExpressionGui.makeNameView (tidg ^. Sugar.tidgName) (Widget.toAnimId nameId)
    <&> Layout.fromCenteredWidget
