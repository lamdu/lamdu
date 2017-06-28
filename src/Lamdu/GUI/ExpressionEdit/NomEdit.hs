{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget(..))
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui, before, (<||), (||>))
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

makeToNom ::
    Monad m =>
    Sugar.Nominal (Name m) (Sugar.BinderBody (Name m) m (ExprGuiT.SugarExpr m)) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom nom pl =
    nom <&> BinderEdit.makeBinderBodyEdit
    & mkNomGui "«" (||>) valId pl
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
    nom <&> ExprGuiM.makeSubexpressionWith 0 (before .~ nomPrecedence+1)
    & mkNomGui "»" (flip (<||)) valId pl
    where
        valId = nom ^. Sugar.nVal . Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

nomPrecedence :: Int
nomPrecedence = 9

mkNomGui ::
    Monad m =>
    Text ->
    (AlignedWidget (T m Widget.EventResult) -> ExpressionGui m -> ExpressionGui m) ->
    Widget.Id ->
    Sugar.Payload m ExprGuiT.Payload ->
    Sugar.Nominal (Name m) (ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
mkNomGui str hCombine valId pl (Sugar.Nominal tid val) =
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let mParenInfo
                | Prec.needParens parentPrec (Prec.my nomPrecedence) =
                    Widget.toAnimId nomId & Just
                | otherwise = Nothing
        ExpressionGui.combineSpacedMParens mParenInfo <*>
            sequence
            [ do
                label <- ExpressionGui.grammarLabel str <&> TreeLayout.fromAlignedWidget
                nameGui <-
                    (Widget.makeFocusableView ?? nameId)
                    <*> (ExpressionGui.makeNameView (tid ^. Sugar.tidgName) (Widget.toAnimId nameId) <&> AlignedWidget.fromView 0)
                nameGui `hCombine` label & TreeLayout.alignment .~ 0
                    & return
            , val
            ]
    & Widget.assignCursor myId valId
    & ExpressionGui.stdWrapParentExpr pl
    & ExprGuiM.withLocalPrecedence 0 (before .~ 0)
    where
        myId = WidgetIds.fromExprPayload pl
        nomId = Widget.joinId myId ["nom"]
        nameId = Widget.joinId nomId ["name"]
