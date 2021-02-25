module Lamdu.GUI.Expr.InjectEdit
    ( make
    ) where

import           GUI.Momentu ((/|/))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (text, grammar)
import           Lamdu.GUI.Wrap (stdWrap)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

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
    Annotated (ExprGui.Payload i o) # Const (Sugar.TagRef Name i o) ->
    GuiM env i o (Responsive o)
make (Ann (Const pl) (Const tag)) =
    maybe (pure id) (ResponsiveExpr.addParens ??) (ExprGui.mParensId pl)
    <*> grammar (text ["injectIndicator"] Texts.injectSymbol) /|/ TagEdit.makeVariantTag tag
    <&> Responsive.fromWithTextPos
    & stdWrap pl
