module Lamdu.GUI.Expr.RecordEdit
    ( make
    ) where

import           GUI.Momentu (Responsive)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.CompositeEdit as CompositeEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.Sugar.Types as Sugar

recordConf :: CompositeEdit.Config
recordConf = CompositeEdit.Config
    { CompositeEdit._name = Texts.record
    , CompositeEdit._itemName = Texts.field
    , CompositeEdit._opener = Texts.recordOpener
    , CompositeEdit._closer = Texts.recordCloser
    , CompositeEdit._tailColor = TextColors.recordTailColor
    , CompositeEdit._tagColor = TextColors.recordTagColor
    }

make :: _ => ExprGui.Expr Sugar.Composite i o -> GuiM env i o (Responsive o)
make = CompositeEdit.make recordConf
