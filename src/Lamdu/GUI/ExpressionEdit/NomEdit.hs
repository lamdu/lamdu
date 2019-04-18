{-# LANGUAGE NoMonomorphismRestriction #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import           AST (Tree, Ann(..), ann)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.NameView as NameView
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.I18N.Texts (Texts)
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

mReplaceParent ::
    Lens.Traversal'
    (Sugar.Expression name i o (Sugar.Payload name i o a))
    (o Sugar.EntityId)
mReplaceParent = ann . Sugar.plActions . Sugar.mReplaceParent . Lens._Just

makeToNom ::
    (Monad i, Monad o) =>
    Sugar.Nominal (Name o)
        (Tree
            (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
            (Sugar.Binder (Name o) i o)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
makeToNom nom pl =
    nom <&> ExprGuiM.makeBinder
    & mkNomGui id "ToNominal" Texts.toNom mDel pl
    where
        mDel =
            nom ^. Sugar.nVal . ann . Sugar.plActions .
            Sugar.mReplaceParent


makeFromNom ::
    (Monad i, Monad o) =>
    Sugar.Nominal (Name o) (ExprGui.SugarExpr i o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
makeFromNom nom pl =
    nom <&> ExprGuiM.makeSubexpression
    & mkNomGui reverse "FromNominal" Texts.fromNom mDel pl
    where
        mDel = nom ^? Sugar.nVal . mReplaceParent

mkNomGui ::
    (Monad i, Monad o) =>
    ([Gui Responsive o] -> [Gui Responsive o]) ->
    Text -> Lens.ALens' Texts Text -> Maybe (o Sugar.EntityId) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    Sugar.Nominal (Name o) (ExprGuiM i o (Gui Responsive o)) ->
    ExprGuiM i o (Gui Responsive o)
mkNomGui ordering nomStr textLens mDel pl (Sugar.Nominal tid val) =
    do
        nomColor <- Lens.view (Theme.theme . Theme.textColors . TextColors.nomColor)
        config <- Lens.view Config.config
        let mkEventMap action =
                action <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys config)
                (E.Doc ["Edit", "Nominal", "Delete " <> nomStr])
        let eventMap = mDel ^. Lens._Just . Lens.to mkEventMap
        stdWrapParentExpr pl
            <*> ( (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
                    <*>
                    ( sequence
                    [ (Widget.makeFocusableView ?? nameId <&> (Align.tValue %~))
                        <*> grammar (label textLens)
                            /|/ NameView.make (tid ^. Sugar.tidName)
                        <&> Responsive.fromWithTextPos
                        & Reader.local (TextView.color .~ nomColor)
                        <&> Widget.weakerEvents eventMap
                    , val
                    ] <&> ordering
                    )
                )
    where
        myId = WidgetIds.fromExprPayload pl
        nameId = Widget.joinId myId ["name"]
