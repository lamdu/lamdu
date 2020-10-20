module Lamdu.GUI.Expr.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.LightLambda as LightLambda
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

addScopeEdit ::
    (MonadReader env m, Applicative o, Glue.HasTexts env) =>
    m (Maybe (Widget o) -> Responsive o -> Responsive o)
addScopeEdit =
    Glue.mkGlue ?? Glue.Vertical
    <&> (\(|---|) mScopeEdit ->
                (|---| maybe Element.empty (WithTextPos 0) mScopeEdit))

mkLhsEdits ::
    (MonadReader env m, Applicative o, Glue.HasTexts env) =>
    m
    (Maybe (Responsive o) ->
     Maybe (Widget o) -> [Responsive o])
mkLhsEdits =
    addScopeEdit <&> \add mParamsEdit mScopeEdit ->
    mParamsEdit ^.. Lens._Just <&> add mScopeEdit

mkExpanded ::
    ( Monad o, MonadReader env f, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env, Glue.HasTexts env, Has (Texts.Code Text) env
    ) =>
    f (Maybe (Responsive o) -> Maybe (Widget o) -> [Responsive o])
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
    , Glue.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    ) => [Sugar.EntityId] -> Widget.Id ->
    f (Maybe (Widget o) -> [Responsive o])
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
    , Has Config env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Glue.HasTexts env
    ) =>
    Sugar.BinderParams v a i o -> Widget.Id ->
    f
    (Maybe (Responsive o) -> Maybe (Widget o) ->
     [Responsive o])
mkLightLambda params myId =
    do
        isSelected <-
            paramIds <&> WidgetIds.fromEntityId
            & traverse (GuiState.isSubCursor ??)
            <&> or
        let shrinkKeys = [MetaKey noMods MetaKey.Key'Escape]
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
                     Widget.weakerEvents shrinkEventMap
            else mkShrunk paramIds myId
                 <&> \mk _mParamsEdit mScopeEdit -> mk mScopeEdit
    where
        paramIds =
            case params of
            Sugar.NullParam{} -> []
            Sugar.Params ps -> ps <&> (^. _2 . Sugar.piTag . Sugar.tagRefTag . Sugar.tagInstance)

make ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    ExprGui.Expr Sugar.Lambda i o -> GuiM env i o (Responsive o)
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
        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*> (Options.boxSpaced ?? Options.disambiguationNone ?? paramsAndLabelEdits
                <&> Widget.strongerEvents rhsJumperEquals
                <&> (: [bodyEdit]))
            & stdWrapParentExpr pl
            <&> Widget.weakerEvents eventMap
    where
        myId = WidgetIds.fromExprPayload (pl ^. _1)
        params = func ^. Sugar.fParams
        func = lam ^. Sugar.lamFunc
        bodyId = func ^. Sugar.fBody . annotation . _1 . Sugar.plEntityId
