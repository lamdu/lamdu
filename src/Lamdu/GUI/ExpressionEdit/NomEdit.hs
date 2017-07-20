{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Graphics.UI.Bottle.View ((/|/))
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui, before, after)
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

mReplaceParent :: Lens.Traversal' (Sugar.Expression name m a) (T m Sugar.EntityId)
mReplaceParent = Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent . Lens._Just

makeToNom ::
    forall m.
    Monad m =>
    Sugar.Nominal (Name m) (Sugar.BinderBody (Name m) m (ExprGuiT.SugarExpr m)) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom nom pl =
    nom <&> BinderEdit.makeBinderBodyEdit
    & mkNomGui id "ToNominal" "«" mDel valId pl
    where
        bbContent = nom ^. Sugar.nVal . Sugar.bbContent
        mDel = bbContent ^? Sugar._BinderExpr . mReplaceParent
        valId =
            bbContent ^. SugarLens.binderContentExpr . Sugar.rPayload .
            Sugar.plEntityId & WidgetIds.fromEntityId

makeFromNom ::
    Monad m =>
    Sugar.Nominal (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom nom pl =
    nom <&> ExprGuiM.makeSubexpressionWith 0 (after .~ nomPrecedence+1)
    & mkNomGui reverse "FromNominal" "»" mDel valId pl
    where
        mDel = nom ^? Sugar.nVal . mReplaceParent
        valId = nom ^. Sugar.nVal . Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

nomPrecedence :: Int
nomPrecedence = 9

mkNomGui ::
    Monad m =>
    (forall a. [a] -> [a]) ->
    Text -> Text -> Maybe (T m Sugar.EntityId) -> Widget.Id ->
    Sugar.Payload m ExprGuiT.Payload ->
    Sugar.Nominal (Name m) (ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
mkNomGui ordering nomStr str mDel valId pl (Sugar.Nominal tid val) =
    do
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let mParenInfo
                | Prec.needParens parentPrec (Prec.my nomPrecedence) =
                    Widget.toAnimId nomId & Just
                | otherwise = Nothing
        nomColor <- Lens.view Theme.theme <&> Theme.nomColor
        config <- Lens.view Config.config
        let mkEventMap action =
                action <&> WidgetIds.fromEntityId
                & Widget.keysEventMapMovesCursor (Config.delKeys config)
                (E.Doc ["Edit", "Nominal", "Delete " <> nomStr])
        let eventMap = mDel ^. Lens._Just . Lens.to mkEventMap
        ExpressionGui.combineSpacedMParens mParenInfo <*>
            ( ordering
                [ (Widget.makeFocusableView ?? nameId) <*>
                do
                    label <- ExpressionGui.grammarLabel str
                    nameGui <- ExpressionGui.makeNameView (tid ^. Sugar.tidgName) (Widget.toAnimId nameId)
                    label /|/ nameGui & TreeLayout.fromTextView & return
                & Reader.local (TextView.color .~ nomColor)
                <&> E.weakerEvents eventMap
              , val
              ] & sequence
            )
    & Widget.assignCursor myId valId
    & ExpressionGui.stdWrapParentExpr pl
    & ExprGuiM.withLocalPrecedence 0 (before .~ 0)
    where
        myId = WidgetIds.fromExprPayload pl
        nomId = Widget.joinId myId ["nom"]
        nameId = Widget.joinId nomId ["name"]
