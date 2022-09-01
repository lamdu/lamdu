module Lamdu.GUI.Expr.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (Responsive)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.CompositeEdit as CompositeEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Types as ExprGui
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
    , CompositeEdit._tailColor = TextColors.caseTailColor
    , CompositeEdit._tagColor  = TextColors.caseTagColor
    }

make :: _ => ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make expr = caseConf >>= (CompositeEdit.make ?? expr)
