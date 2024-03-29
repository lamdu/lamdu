module Lamdu.GUI.Expr.LambdaEdit
    ( make
    ) where

import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import           Lamdu.GUI.Expr.EventMap (closeParenEvent)
import qualified Lamdu.GUI.Expr.ParamsEdit as ParamsEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: _ => ExprGui.Expr Sugar.Lambda i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) lam) =
    do
        AssignmentEdit.Parts lhsEventMap paramsEdit mScopeEdit scopeEventMap scopeId <-
            AssignmentEdit.makeFunctionParts (lam ^. Sugar.lamApplyLimit)
            (Ann (Const pl) func) (WidgetIds.fromEntityId bodyId)
        bodyEdit <- func ^. Sugar.fBody & GuiM.makeBinder & GuiM.withLocalMScopeId scopeId
        rhsJumperEquals <- func ^. Sugar.fBody . annotation & WidgetIds.fromExprPayload & AssignmentEdit.makeJumpToRhs
        paramsAndLabelEdits <- ParamsEdit.makeLhs (lam ^. Sugar.lamLightweight) params paramsEdit mScopeEdit lhsEventMap myId
        navigateOut <-
            closeParenEvent
            [has . MomentuTexts.navigation, has . Texts.lambda, has . Texts.leaveSubexpression]
            (pure myId)
        Options.boxSpaced Options.disambiguationNone paramsAndLabelEdits
            <&> Widget.strongerEvents rhsJumperEquals
            <&> (: [bodyEdit])
            >>= ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
            & stdWrapParentExpr pl
            <&> M.weakerEvents (scopeEventMap <> navigateOut)
    where
        myId = WidgetIds.fromExprPayload pl
        params = func ^. Sugar.fParams
        func = lam ^. Sugar.lamFunc
        bodyId = func ^. Sugar.fBody . annotation . Sugar.plEntityId
