module Lamdu.GUI.Expr.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import           GUI.Momentu (Responsive, (/|/))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.CompositeEdit as CompositeEdit
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

caseConf :: _ => m CompositeEdit.Config
caseConf =
    Lens.view id
    <&> \env -> CompositeEdit.Config
    { CompositeEdit._name      = env ^. has . Texts.case_
    , CompositeEdit._itemName  = env ^. has . Texts.alternative
    , CompositeEdit._opener    = Texts.caseOpener
    , CompositeEdit._closer    = Texts.caseCloser
    , CompositeEdit._tagColor  = TextColors.caseTagColor
    }

make :: _ => ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make expr =
    do
        header <- grammar (Label.make ".") /|/ makeCaseLabel
        conf <- caseConf
        hbox <- Options.boxSpaced Options.disambiguationNone & pushToReader
        let prependCase altsGui = hbox [header, altsGui]
        CompositeEdit.make (Just prependCase) bodyId conf expr
    where
        myId = expr ^. annotation & WidgetIds.fromExprPayload
        headerId = myId <> "header"
        bodyId = myId <> "body"
        makeCaseLabel =
            grammar (label Texts.case_)
            >>= M.tValue (Widget.makeFocusableView headerId)
            <&> Responsive.fromWithTextPos
