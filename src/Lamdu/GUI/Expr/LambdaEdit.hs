module Lamdu.GUI.Expr.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (noMods)
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import           Lamdu.GUI.Expr.EventMap (closeParenEvent)
import qualified Lamdu.GUI.LightLambda as LightLambda
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

addScopeEdit :: _ => m (Maybe (M.Widget o) -> Responsive o -> Responsive o)
addScopeEdit =
    Glue.mkGlue ?? Glue.Vertical
    <&> (\(|---|) mScopeEdit ->
                (|---| maybe M.empty (M.WithTextPos 0) mScopeEdit))

mkLhsEdits :: _ => m (Maybe (Responsive o) -> Maybe (M.Widget o) -> [Responsive o])
mkLhsEdits =
    addScopeEdit <&> \add mParamsEdit mScopeEdit ->
    mParamsEdit ^.. Lens._Just <&> add mScopeEdit

mkExpanded :: _ => f (Maybe (Responsive o) -> Maybe (M.Widget o) -> [Responsive o])
mkExpanded =
    do
        lam <- grammar (label Texts.lam) <&> Responsive.fromTextView
        lhsEdits <- mkLhsEdits
        arrow <- grammar (label Texts.arrow) <&> Responsive.fromTextView
        pure (\mParamsEdit mScopeEdit -> lam : lhsEdits mParamsEdit mScopeEdit <> [arrow])

lamId :: Widget.Id -> Widget.Id
lamId = (`Widget.joinId` ["lam"])

mkShrunk :: _ => [Sugar.EntityId] -> Widget.Id -> f (Maybe (M.Widget o) -> [Responsive o])
mkShrunk paramIds myId =
    do
        env <- Lens.view id
        let expandEventMap =
                paramIds ^? Lens.traverse
                & foldMap
                  (E.keysEventMapMovesCursor
                      (env ^. has . Config.jumpToDefinitionKeys)
                      ( E.toDoc env
                          [ has . MomentuTexts.view
                          , has . Texts.expandLambdaParams
                          ]
                      ) . pure . WidgetIds.fromEntityId)
        theme <- Lens.view has
        lamLabel <-
            (Widget.makeFocusableView ?? lamId myId <&> (M.tValue %~))
            <*> grammar (label Texts.lam)
            <&> Responsive.fromWithTextPos
            & local (TextView.underline ?~ LightLambda.underline theme)
        addScopeEd <- addScopeEdit
        pure $ \mScopeEdit ->
            [ addScopeEd mScopeEdit lamLabel
              & M.weakerEvents expandEventMap
            ]

mkLightLambda ::
    _ =>
    Sugar.BinderParams v a i o -> Widget.Id ->
    f (Maybe (Responsive o) -> Maybe (M.Widget o) -> [Responsive o])
mkLightLambda params myId =
    do
        isSelected <-
            paramIds <&> WidgetIds.fromEntityId
            & traverse (GuiState.isSubCursor ??)
            <&> or
        let shrinkKeys = [noMods ModKey.Key'Escape]
        env <- Lens.view id
        let shrinkEventMap =
                E.keysEventMapMovesCursor shrinkKeys
                (E.toDoc env
                    [ has . MomentuTexts.view
                    , has . Texts.shrinkLambdaParams
                    ]) (pure (lamId myId))
        if isSelected
            then
                 mkExpanded
                 <&> Lens.mapped . Lens.mapped . Lens.mapped %~
                     M.weakerEvents shrinkEventMap
            else mkShrunk paramIds myId
                 <&> \mk _mParamsEdit mScopeEdit -> mk mScopeEdit
    where
        paramIds =
            case params of
            Sugar.NullParam{} -> []
            Sugar.Params ps -> ps <&> (^. _2 . Sugar.piTag . Sugar.tagRefTag . Sugar.tagInstance)

make :: _ => ExprGui.Expr Sugar.Lambda i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) lam) =
    do
        AssignmentEdit.Parts mParamsEdit mScopeEdit bodyEdit eventMap _wrap rhsId <-
            AssignmentEdit.makeFunctionParts (lam ^. Sugar.lamApplyLimit)
            (Ann (Const pl) func) (WidgetIds.fromEntityId bodyId)
        rhsJumperEquals <- AssignmentEdit.makeJumpToRhs rhsId
        paramsAndLabelEdits <-
            case (lam ^. Sugar.lamMode, params) of
            (_, Sugar.NullParam{}) -> mkLhsEdits ?? mParamsEdit ?? mScopeEdit
            (Sugar.LightLambda, _) -> mkLightLambda params myId ?? mParamsEdit ?? mScopeEdit
            _ -> mkExpanded ?? mParamsEdit ?? mScopeEdit
        navigateOut <-
            closeParenEvent
            [has . MomentuTexts.navigation, has . Texts.lambda, has . Texts.leaveSubexpression]
            (pure myId)
        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*> (Options.boxSpaced ?? Options.disambiguationNone ?? paramsAndLabelEdits
                <&> Widget.strongerEvents rhsJumperEquals
                <&> (: [bodyEdit]))
            & stdWrapParentExpr pl
            <&> M.weakerEvents (eventMap <> navigateOut)
    where
        myId = WidgetIds.fromExprPayload pl
        params = func ^. Sugar.fParams
        func = lam ^. Sugar.lamFunc
        bodyId = func ^. Sugar.fBody . annotation . Sugar.plEntityId
