module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import           AST (Tree, Ann(..), ann)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.GUI.ExpressionEdit.AssignmentEdit as AssignmentEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.LightLambda as LightLambda
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

addScopeEdit ::
    (MonadReader env m, Applicative o, Glue.HasTexts env) =>
    m (Maybe (Gui Widget o) -> Gui Responsive o -> Gui Responsive o)
addScopeEdit =
    Glue.mkGlue ?? Glue.Vertical
    <&> (\(|---|) mScopeEdit ->
                (|---| maybe Element.empty (WithTextPos 0) mScopeEdit))

mkLhsEdits ::
    (MonadReader env m, Applicative o, Glue.HasTexts env) =>
    m
    (Maybe (Gui Responsive o) ->
     Maybe (Gui Widget o) -> [Gui Responsive o])
mkLhsEdits =
    addScopeEdit <&> \add mParamsEdit mScopeEdit ->
    mParamsEdit ^.. Lens._Just <&> add mScopeEdit

mkExpanded ::
    ( Monad o, MonadReader env f, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env, Glue.HasTexts env, Has (Texts.Code Text) env
    ) =>
    f (Maybe (Gui Responsive o) -> Maybe (Gui Widget o) -> [Gui Responsive o])
mkExpanded =
    (,)
    <$> mkLhsEdits
    <*> (grammar (label Texts.arrow) <&> Responsive.fromTextView)
    <&> \(lhsEdits, labelEdit) mParamsEdit mScopeEdit ->
    lhsEdits mParamsEdit mScopeEdit ++ [labelEdit]

lamId :: Widget.Id -> Widget.Id
lamId = (`Widget.joinId` ["lam"])

mkShrunk ::
    ( Monad o, MonadReader env f, Has Config env, Has Theme env
    , GuiState.HasCursor env, Element.HasAnimIdPrefix env, Has TextView.Style env
    , Has (Texts.Code Text) env, Glue.HasTexts env
    ) => [Sugar.EntityId] -> Widget.Id ->
    f (Maybe (Gui Widget o) -> [Gui Responsive o])
mkShrunk paramIds myId =
    do
        jumpKeys <- Lens.view (has . Config.jumpToDefinitionKeys)
        let expandEventMap =
                paramIds ^? Lens.traverse
                & foldMap
                  (E.keysEventMapMovesCursor jumpKeys
                   (E.Doc ["View", "Expand Lambda Params"]) . pure .
                   WidgetIds.fromEntityId)
        theme <- Lens.view has
        lamLabel <-
            (Widget.makeFocusableView ?? lamId myId <&> (Align.tValue %~))
            <*> grammar (label Texts.lam)
            <&> Responsive.fromWithTextPos
            & Reader.local (TextView.underline ?~ LightLambda.underline theme)
        addScopeEd <- addScopeEdit
        pure $ \mScopeEdit ->
            [ addScopeEd mScopeEdit lamLabel
              & Widget.weakerEvents expandEventMap
            ]

mkLightLambda ::
    ( Monad o, MonadReader env f, GuiState.HasCursor env
    , Element.HasAnimIdPrefix env, Has TextView.Style env, Has Theme env
    , Has Config env, Has (Texts.Code Text) env, Glue.HasTexts env
    ) =>
    Sugar.BinderParams a i o -> Widget.Id ->
    f
    (Maybe (Gui Responsive o) -> Maybe (Gui Widget o) ->
     [Gui Responsive o])
mkLightLambda params myId =
    do
        isSelected <-
            paramIds <&> WidgetIds.fromEntityId
            & traverse (GuiState.isSubCursor ??)
            <&> or
        let shrinkKeys = [MetaKey noMods MetaKey.Key'Escape]
        let shrinkEventMap =
                E.keysEventMapMovesCursor shrinkKeys
                (E.Doc ["View", "Shrink Lambda Params"]) (pure (lamId myId))
        if isSelected
            then
                 mkExpanded
                 <&> Lens.mapped . Lens.mapped . Lens.mapped %~
                     Widget.weakerEvents shrinkEventMap
            else mkShrunk paramIds myId
                 <&> \mk _mParamsEdit mScopeEdit -> mk mScopeEdit
    where
        paramIds =
            case params of
            Sugar.NullParam{} -> []
            Sugar.Params ps -> ps <&> (^. Sugar.fpInfo . Sugar.piTag . Sugar.tagInfo . Sugar.tagInstance)

make ::
    (Monad i, Monad o) =>
    Tree (Sugar.Lambda (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make lam pl =
    do
        AssignmentEdit.Parts mParamsEdit mScopeEdit bodyEdit eventMap _wrap _rhsId <-
            AssignmentEdit.makeFunctionParts (lam ^. Sugar.lamApplyLimit)
            func pl (WidgetIds.fromEntityId bodyId)
        paramsAndLabelEdits <-
            case (lam ^. Sugar.lamMode, params) of
            (_, Sugar.NullParam{}) -> mkLhsEdits ?? mParamsEdit ?? mScopeEdit
            (Sugar.LightLambda, _) -> mkLightLambda params myId ?? mParamsEdit ?? mScopeEdit
            _ -> mkExpanded ?? mParamsEdit ?? mScopeEdit
        stdWrapParentExpr pl
            <*> ( (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
                    <*> (Options.boxSpaced ?? Options.disambiguationNone ?? paramsAndLabelEdits
                        <&> (: [bodyEdit]))
                )
            <&> Widget.weakerEvents eventMap
    where
        myId = WidgetIds.fromExprPayload pl
        params = func ^. Sugar.fParams
        func = lam ^. Sugar.lamFunc
        bodyId = func ^. Sugar.fBody . ann . Sugar.plEntityId
