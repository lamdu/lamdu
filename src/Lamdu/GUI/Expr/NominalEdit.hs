{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.Expr.NominalEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
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

makeToNom ::
    ( Monad i, Monad o, Grid.HasTexts env
    , Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Definitions Text) env
    ) =>
    Sugar.Expr (Sugar.Nominal (Sugar.EvaluationScopes Name i)) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
makeToNom (Ann (Const pl) (Sugar.Nominal tid binder)) =
    do
        env <- Lens.view id
        let mkEventMap action =
                action <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.nominal
                    , has . Texts.deleteToNominal
                    ])
        let eventMap =
                binder ^.
                annotation . Sugar.plActions . Sugar.mReplaceParent .
                Lens._Just . Lens.to mkEventMap
        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? nameId <&> (Align.tValue %~))
                <*> mkNomLabel Texts.toNom tid
                <&> Responsive.fromWithTextPos
                <&> Widget.weakerEvents eventMap
            , GuiM.makeBinder binder
            ] & stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl
        nameId = Widget.joinId myId ["name"]

makeFromNom ::
    ( Monad i, Monad o, Grid.HasTexts env
    , Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Definitions Text) env
    ) =>
    Annotated (Sugar.Payload Name i o ExprGui.Payload) # Const (Sugar.TId Name) ->
    GuiM env i o (Responsive o)
makeFromNom (Ann (Const pl) (Const nom)) =
    mkNomLabel Texts.fromNom nom <&> Responsive.fromTextView
    & stdWrapParentExpr pl

mkNomLabel ::
    (Monad i, Has (Texts.Code Text) env, Has (Texts.Name Text) env) =>
    OneOf Texts.Code ->
    Sugar.TId Name ->
    GuiM env i o (WithTextPos View)
mkNomLabel textLens tid =
    do
        nomColor <- Lens.view (has . Theme.textColors . TextColors.nomColor)
        grammar (label textLens)
            /|/ NameView.make (tid ^. Sugar.tidName)
            & Reader.local (TextView.color .~ nomColor)
