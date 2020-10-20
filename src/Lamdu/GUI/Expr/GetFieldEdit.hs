module Lamdu.GUI.Expr.GetFieldEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
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
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    ExprGui.Expr Sugar.GetField i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.GetField recExpr tag)) =
    do
        recExprEdit <- GuiM.makeSubexpression recExpr
        env <- Lens.view id
        let mkDelEventMap del =
                del <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env
                    [has . MomentuTexts.edit, has . MomentuTexts.delete])
        let delEventMap =
                recExpr ^. annotation . _1 . Sugar.plActions . Sugar.mReplaceParent
                & foldMap mkDelEventMap
        tagEdit <-
            Styled.grammar (Label.make ".")
            /|/
            (TagEdit.makeRecordTag tag <&> Lens.mapped %~ Widget.weakerEvents delEventMap)
        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl) ??
            [ recExprEdit
            , Responsive.fromWithTextPos tagEdit
            ] & stdWrapParentExpr pl
