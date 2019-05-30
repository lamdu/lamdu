{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}
module Lamdu.GUI.Expr.NominalEdit
    ( makeFromNom, makeToNom
    ) where

import           AST (Tree, Ann(..), ann)
import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import qualified Control.Monad.Reader as Reader
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
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
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

mReplaceParent ::
    Lens.Traversal'
    (Sugar.Expression name i o (Sugar.Payload name i o a))
    (o Sugar.EntityId)
mReplaceParent = ann . Sugar.plActions . Sugar.mReplaceParent . Lens._Just

makeToNom ::
    ( Monad i, Monad o, Grid.HasTexts env
    , Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Definitions Text) env
    ) =>
    Sugar.Nominal (Name o)
        (Tree
            (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
            (Sugar.Binder (Name o) i o)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM env i o (Gui Responsive o)
makeToNom nom pl =
    nom <&> ExprGuiM.makeBinder
    & mkNomGui id Texts.deleteToNominal Texts.toNom mDel pl
    where
        mDel =
            nom ^. Sugar.nVal . ann . Sugar.plActions .
            Sugar.mReplaceParent


makeFromNom ::
    ( Monad i, Monad o, Grid.HasTexts env
    , Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Definitions Text) env
    ) =>
    Sugar.Nominal (Name o) (ExprGui.SugarExpr i o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM env i o (Gui Responsive o)
makeFromNom nom pl =
    nom <&> ExprGuiM.makeSubexpression
    & mkNomGui reverse Texts.deleteFromNominal Texts.fromNom mDel pl
    where
        mDel = nom ^? Sugar.nVal . mReplaceParent

mkNomGui ::
    ( Monad i, Monad o, Grid.HasTexts env
    , Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Definitions Text) env
    ) =>
    ([Gui Responsive o] -> [Gui Responsive o]) ->
    OneOf Texts.CodeUI -> OneOf Texts.Code -> Maybe (o Sugar.EntityId) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    Sugar.Nominal (Name o) (ExprGuiM env i o (Gui Responsive o)) ->
    ExprGuiM env i o (Gui Responsive o)
mkNomGui ordering deleteNomText textLens mDel pl (Sugar.Nominal tid val) =
    do
        nomColor <- Lens.view (has . Theme.textColors . TextColors.nomColor)
        env <- Lens.view id
        let mkEventMap action =
                action <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.nominal
                    , has . deleteNomText
                    ])
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
