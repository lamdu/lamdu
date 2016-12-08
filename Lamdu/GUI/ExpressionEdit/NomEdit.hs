{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
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

import           Lamdu.Prelude

type T = Transaction

data ShowName = NameHovering | NameShowing | NameCollapsed

makeToNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom = mkNomGui precBefore "«" (\a b -> [a, b]) (||>)

makeFromNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom = mkNomGui precAfter "»" (\a b -> [b, a]) (flip (<||))

nomPrecedence :: Int
nomPrecedence = 9

mkNomGui ::
    Monad m =>
    Lens.ASetter' Precedence Int ->
    Text ->
    (ExpressionGui m -> ExpressionGui m -> [ExpressionGui m]) ->
    (AlignedWidget (T m Widget.EventResult) -> ExpressionGui m -> ExpressionGui m) ->
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
mkNomGui nameSidePrecLens str asList hCombine nom@(Sugar.Nominal _ val) pl =
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
        expandingName asList hCombine needParen nomId nameShowing
            <*> (ExpressionGui.grammarLabel str (Widget.toAnimId myId)
                <&> if isSelected then id
                    else AlignedWidget.widget %~ Widget.takesFocus (const (pure nameId))
                )
            <*> (ExpressionGui.makeFocusableView nameId
                 <*> mkNameGui nom nameId)
            <*> ExprGuiM.makeSubexpression (nameSidePrecLens .~ nomPrecedence+1) val
    & ExprGuiM.assignCursor myId valId
    where
        valId = val ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

expandingName ::
    Monad m =>
    (ExpressionGui f -> ExpressionGui f -> [ExpressionGui f]) ->
    (AlignedWidget (T f Widget.EventResult) -> ExpressionGui f -> ExpressionGui f) ->
    Bool -> --need paren
    Widget.Id -> -- nomId
    ShowName ->
    ExprGuiM m
    ( AlignedWidget (T f Widget.EventResult) -> -- label
      AlignedWidget (T f Widget.EventResult) -> -- name gui
      ExpressionGui f -> -- subexpr gui
      ExpressionGui f
    )
expandingName vertOrder (#>) needParen nomId showName =
    do
        space <- ExpressionGui.stdHSpace <&> AlignedWidget.fromCenteredWidget
        addBg <- ExpressionGui.addValBGWithColor Config.valNomBGColor nomId
        horizWithFallback <- ExpressionGui.horizVertFallback mParenInfo
        return $
            \label nameGui subexprGui ->
            let nameShowing =
                    TreeLayout.LayoutParams
                    { TreeLayout._layoutMode = TreeLayout.LayoutWide
                    , TreeLayout._layoutContext = TreeLayout.LayoutClear
                    }
                    & (nameGui #> TreeLayout.fromAlignedWidget label) ^. TreeLayout.render
                    & AlignedWidget.widget %~ addBg
                horiz =
                    case showName of
                    NameCollapsed -> label & AlignedWidget.widget %~ addBg
                    NameShowing -> nameShowing
                    NameHovering -> nameShowing `AlignedWidget.hoverInPlaceOf` label
                    #> (space #> subexprGui)
                vert =
                    TreeLayout.fromAlignedWidget nameShowing
                    `vertOrder` subexprGui
                    <&> TreeLayout.alignment . _1 .~ 0
                    & ExpressionGui.vboxTopFocal
            in horiz `horizWithFallback` vert
    where
        mParenInfo
            | needParen = Widget.toAnimId nomId & Just
            | otherwise = Nothing

mkNameGui ::
    Monad m => Sugar.Nominal (Name m) a -> Widget.Id ->
    ExprGuiM m (AlignedWidget b)
mkNameGui (Sugar.Nominal tidg _val) nameId =
    ExpressionGui.makeNameView (tidg ^. Sugar.tidgName) (Widget.toAnimId nameId)
    <&> Widget.fromView
    <&> AlignedWidget.fromCenteredWidget
