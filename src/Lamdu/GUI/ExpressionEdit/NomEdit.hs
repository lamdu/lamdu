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
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui
    ( ExpressionGui, Precedence, before, after, (<||), (||>) )
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Precedence as Prec
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data ShowName = NameHovering | NameShowing | NameCollapsed

makeToNom ::
    Monad m =>
    Sugar.Nominal (Name m) (Sugar.BinderBody (Name m) m (ExprGuiT.SugarExpr m)) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom nom pl =
    nom <&> BinderEdit.makeBinderBodyEdit
    & mkNomGui before "«" (\a b -> [a, b]) (||>) valId pl
    where
        valId =
            nom ^. Sugar.nVal . Sugar.bbContent . SugarLens.binderContentExpr
                . Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

makeFromNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom nom pl =
    nom <&> ExprGuiM.makeSubexpressionWith 0 (after .~ nomPrecedence+1)
    & mkNomGui after "»" (\a b -> [b, a]) (flip (<||)) valId pl
    where
        valId = nom ^. Sugar.nVal . Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

nomPrecedence :: Int
nomPrecedence = 9

mkNomGui ::
    Monad m =>
    Lens.ASetter' Precedence Int ->
    Text ->
    (ExpressionGui m -> ExpressionGui m -> [ExpressionGui m]) ->
    (AlignedWidget (T m Widget.EventResult) -> ExpressionGui m -> ExpressionGui m) ->
    Widget.Id ->
    Sugar.Payload m ExprGuiT.Payload ->
    Sugar.Nominal (Name m) (ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
mkNomGui nameSidePrecLens str asList hCombine valId pl (Sugar.Nominal tid val) =
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let needParen = Prec.needParens parentPrec (Prec.my nomPrecedence)
        let nomId = Widget.joinId myId ["nom"]
        let nameId = Widget.joinId nomId ["name"]
        isSelected <- Widget.isSubCursor ?? nomId
        isVerbose <- ExprGuiM.readVerbose
        let nameShowing
                | isVerbose || ExprGuiT.plOfHoleResult pl = NameShowing
                | isSelected = NameHovering
                | otherwise = NameCollapsed
        expandingName asList hCombine needParen nomId nameShowing
            <*> (ExpressionGui.grammarLabel str (Widget.toAnimId myId)
                <&> if isSelected then id
                    else AlignedWidget.widget %~ Widget.takesFocus (const (pure nameId))
                )
            <*> (ExpressionGui.makeFocusableView nameId
                 <*> mkNameGui tid nameId)
            <*> val
    & Widget.assignCursor myId valId
    & ExpressionGui.stdWrapParentExpr pl
    & ExprGuiM.withLocalPrecedence 0 (nameSidePrecLens .~ 0)
    where
        myId = WidgetIds.fromExprPayload pl

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
        addBg <- ExpressionGui.addValBGWithColor Theme.valNomBGColor nomId
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
    Monad m => Sugar.TIdG (Name m) -> Widget.Id -> ExprGuiM m (AlignedWidget b)
mkNameGui tidg nameId =
    ExpressionGui.makeNameView (tidg ^. Sugar.tidgName) (Widget.toAnimId nameId)
    <&> Widget.fromView
    <&> AlignedWidget.fromCenteredWidget
