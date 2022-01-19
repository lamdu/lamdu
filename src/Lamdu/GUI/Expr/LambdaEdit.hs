module Lamdu.GUI.Expr.LambdaEdit
    ( make
    ) where

import qualified GUI.Momentu as M
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import           Lamdu.GUI.Expr.EventMap (closeParenEvent)
import qualified Lamdu.GUI.Expr.ParamsEdit as ParamsEdit
import           Lamdu.GUI.Monad (GuiM)
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
        AssignmentEdit.Parts lhsEventMap mParamsEdit mScopeEdit bodyEdit scopeEventMap _wrap rhsId <-
            AssignmentEdit.makeFunctionParts (lam ^. Sugar.lamApplyLimit)
            (Ann (Const pl) func) (WidgetIds.fromEntityId bodyId)
        rhsJumperEquals <- AssignmentEdit.makeJumpToRhs rhsId
        paramsAndLabelEdits <- ParamsEdit.makeLhs (lam ^. Sugar.lamLightweight) params mParamsEdit mScopeEdit lhsEventMap myId
        navigateOut <-
            closeParenEvent
            [has . MomentuTexts.navigation, has . Texts.lambda, has . Texts.leaveSubexpression]
            (pure myId)
        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*> (Options.boxSpaced ?? Options.disambiguationNone ?? paramsAndLabelEdits
                <&> Widget.strongerEvents rhsJumperEquals
                <&> (: [bodyEdit]))
            & stdWrapParentExpr pl
            <&> M.weakerEvents (scopeEventMap <> navigateOut)
    where
        myId = WidgetIds.fromExprPayload pl
        params = func ^. Sugar.fParams
        func = lam ^. Sugar.lamFunc
        bodyId = func ^. Sugar.fBody . annotation . Sugar.plEntityId
