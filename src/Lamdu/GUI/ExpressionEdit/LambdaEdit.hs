module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
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
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

addScopeEdit ::
    Functor o => Maybe (Gui Widget o) -> Gui Responsive o -> Gui Responsive o
addScopeEdit mScopeEdit = (/-/ maybe Element.empty (WithTextPos 0) mScopeEdit)

mkLhsEdits ::
    Functor o =>
    Maybe (Gui Responsive o) ->
    Maybe (Gui Widget o) -> [Gui Responsive o]
mkLhsEdits mParamsEdit mScopeEdit =
    mParamsEdit <&> addScopeEdit mScopeEdit & (^.. Lens._Just)

mkExpanded ::
    ( Monad o, MonadReader env f, HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) =>
    f (Maybe (Gui Responsive o) -> Maybe (Gui Widget o) ->
     [Gui Responsive o])
mkExpanded =
    Styled.grammarLabel "→" <&> Responsive.fromTextView
    <&> \labelEdit mParamsEdit mScopeEdit ->
    mkLhsEdits mParamsEdit mScopeEdit ++ [labelEdit]

lamId :: Widget.Id -> Widget.Id
lamId = (`Widget.joinId` ["lam"])

mkShrunk ::
    ( Monad o, MonadReader env f, HasConfig env, HasTheme env
    , GuiState.HasCursor env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => [Sugar.EntityId] -> Widget.Id ->
    f (Maybe (Gui Widget o) -> [Gui Responsive o])
mkShrunk paramIds myId =
    do
        jumpKeys <- Lens.view (Config.config . Config.jumpToDefinitionKeys)
        let expandEventMap =
                paramIds ^? Lens.traverse
                & foldMap
                  (E.keysEventMapMovesCursor jumpKeys
                   (E.Doc ["View", "Expand Lambda Params"]) . pure .
                   WidgetIds.fromEntityId)
        theme <- Lens.view Theme.theme
        lamLabel <-
            (Widget.makeFocusableView ?? lamId myId <&> (Align.tValue %~))
            <*> Styled.grammarLabel "λ"
            <&> Responsive.fromWithTextPos
            & Reader.local (TextView.underline ?~ LightLambda.underline theme)
        pure $ \mScopeEdit ->
            [ addScopeEdit mScopeEdit lamLabel
              & Widget.weakerEvents expandEventMap
            ]

mkLightLambda ::
    ( Monad o, MonadReader env f, GuiState.HasCursor env
    , Element.HasAnimIdPrefix env, TextView.HasStyle env, HasTheme env
    , HasConfig env
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
    Sugar.Lambda (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make lam pl =
    do
        BinderEdit.Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            BinderEdit.makeFunctionParts (lam ^. Sugar.lamApplyLimit)
            func (WidgetIds.fromEntityId bodyId) myId
        paramsAndLabelEdits <-
            case (lam ^. Sugar.lamMode, params) of
            (_, Sugar.NullParam{}) -> mkLhsEdits mParamsEdit mScopeEdit & pure
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
        bodyId = func ^. Sugar.fBody . Sugar.bContent . SugarLens.binderContentEntityId
