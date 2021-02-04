{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.Expr.InjectEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (text, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

injectIndicator ::
    ( MonadReader env f, Has TextView.Style env, Has Theme env
    , Element.HasAnimIdPrefix env, Has (Texts.Code Text) env
    , Has Dir.Layout env
    ) => OneOf Texts.Code -> f (WithTextPos View)
injectIndicator l = grammar (text ["injectIndicator"] l)

make ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    ExprGui.Expr Sugar.Inject i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Inject tag val)) =
    do
        env <- Lens.view id
        let delDoc = E.toDoc env [has . MomentuTexts.edit, has . MomentuTexts.delete]
        arg <- GuiM.makeSubexpression val
        let replaceParentEventMap replaceParent =
                -- Deleting the inject is replacing the whole expr
                -- with the injected value "child"
                replaceParent <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env) delDoc

        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*>
            ( TagEdit.makeVariantTag tag
                /|/ injectIndicator Texts.injectSymbol
                <&> Lens.mapped %~ Widget.weakerEvents (foldMap replaceParentEventMap mReplaceParent)
                <&> Responsive.fromWithTextPos
                <&> (: [arg])
            )
        & stdWrapParentExpr pl
    where
        mReplaceParent = val ^. annotation . _1 . Sugar.plActions . Sugar.mReplaceParent
