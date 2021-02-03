{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.Expr.NominalEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
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
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.NameView as NameView
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
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
    ExprGui.Expr Sugar.Nominal i o -> GuiM env i o (Responsive o)
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
                annotation . _1 . Sugar.plActions . Sugar.mReplaceParent .
                Lens._Just . Lens.to mkEventMap
        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? nameId <&> (Align.tValue %~))
                <*> mkNomLabel tid
                <&> Responsive.fromWithTextPos
                <&> Widget.weakerEvents eventMap
            , GuiM.makeBinder binder
            ] & stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload (pl ^. _1)
        nameId = Widget.joinId myId ["name"]

makeFromNom ::
    ( Monad i, Monad o, Grid.HasTexts env
    , Has (Texts.Navigation Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Definitions Text) env
    ) =>
    Annotated (ExprGui.Payload i o) # Const (Sugar.TId Name) ->
    GuiM env i o (Responsive o)
makeFromNom (Ann (Const pl) (Const nom)) =
    Styled.grammar (Label.make ".")
    /|/ mkNomLabel nom
    <&> Responsive.fromTextView
    & stdWrapParentExpr pl

mkNomLabel ::
    (Monad i, Has (Texts.Name Text) env) =>
    Sugar.TId Name ->
    GuiM env i o (WithTextPos View)
mkNomLabel tid =
    do
        nomColor <- Lens.view (has . Theme.textColors . TextColors.nomColor)
        NameView.make (tid ^. Sugar.tidName) & Reader.local (TextView.color .~ nomColor)
