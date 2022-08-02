module Lamdu.GUI.Expr.HoleOptEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as SBS8
import           Hyper
import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.Styled (grammar, label)
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    _ => ExprGui.Expr Sugar.HoleOpt i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) b) =
    case b of
    Sugar.HoleBinder x -> GuiM.makeBinder (Ann (Const pl) x)
    Sugar.HoleVarsRecord fieldNames ->
        do
            (Options.box ?? Options.disambiguationNone)
                <*> sequence
                [ grammar (label Texts.recordOpener) <&> Responsive.fromTextView
                , (Options.boxSpaced ?? Options.disambiguationNone)
                <*>
                ( fieldNames
                    & Lens.itraverse
                    (\i fieldName ->
                        let paramId = ["params", SBS8.pack (show (i :: Int))]
                        in
                        Widget.joinId myId paramId
                        & GetVarEdit.makeSimpleView TextColors.variableColor fieldName
                        <&> Responsive.fromWithTextPos
                        & local (M.animIdPrefix %~ (<> paramId))
                    )
                )
                , grammar (label Texts.recordCloser) <&> Responsive.fromTextView
                ] & stdWrapParentExpr pl
        & local (M.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = WidgetIds.fromExprPayload pl
