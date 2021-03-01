module Lamdu.GUI.Expr.GetFieldEdit
    ( make
    ) where

import           GUI.Momentu ((/|/))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: _ => ExprGui.Payload i o -> Sugar.TagRef Name i o -> GuiM env i o (Responsive o)
make pl tag =
    Styled.grammar (Label.make ".") /|/ TagEdit.makeRecordTag tag
    <&> Responsive.fromWithTextPos
    & stdWrapParentExpr pl
