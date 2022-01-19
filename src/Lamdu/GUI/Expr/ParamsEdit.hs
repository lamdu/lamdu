module Lamdu.GUI.Expr.ParamsEdit
    ( make
    , ScopeCursor(..), scopeCursor
    , IsScopeNavFocused(..)
    , makeLhs
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev, current)
import qualified GUI.Momentu as M
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (noMods)
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Annotation as Annotation
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.LightLambda as LightLambda
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TaggedList as TaggedList
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data IsScopeNavFocused = ScopeNavIsFocused | ScopeNavNotFocused
    deriving (Eq, Ord)

data ScopeCursor = ScopeCursor
    { sBinderScope :: Sugar.BinderParamScopeId
    , sMPrevParamScope :: Maybe Sugar.BinderParamScopeId
    , sMNextParamScope :: Maybe Sugar.BinderParamScopeId
    }

scopeCursor :: Maybe Sugar.BinderParamScopeId -> [Sugar.BinderParamScopeId] -> Maybe ScopeCursor
scopeCursor mChosenScope scopes =
    do
        chosenScope <- mChosenScope
        (prevs, it:nexts) <- break (== chosenScope) scopes & Just
        Just ScopeCursor
            { sBinderScope = it
            , sMPrevParamScope = reverse prevs ^? Lens.traversed
            , sMNextParamScope = nexts ^? Lens.traversed
            }
    <|> (scopes ^? Lens.traversed <&> def)
    where
        def binderScope =
            ScopeCursor
            { sBinderScope = binderScope
            , sMPrevParamScope = Nothing
            , sMNextParamScope = scopes ^? Lens.ix 1
            }

addScopeEdit :: _ => m (Maybe (M.Widget o) -> Responsive o -> Responsive o)
addScopeEdit =
    Glue.mkGlue ?? Glue.Vertical
    <&> (\(|---|) mScopeEdit -> (|---| maybe M.empty (M.WithTextPos 0) mScopeEdit))

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

lamId :: Widget.Id -> Widget.Id
lamId = (`Widget.joinId` ["lam"])

mkLightLambda ::
    _ =>
    Sugar.Params v a i o -> Widget.Id ->
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
            Sugar.VarParam (_, p) -> [p ^. Sugar.vpiTag . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance]
            Sugar.RecordParams ps -> ps ^.. SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagInstance

makeLhs ::
    _ =>
    Bool -> Sugar.Params v a i o ->
    Maybe (Responsive o) -> Maybe (Widget.Widget o) -> EventMap (o GuiState.Update) -> Widget.Id ->
    m [Responsive o]
makeLhs _ Sugar.NullParam{} mParamsEdit mScopeEdit _ _ = mkLhsEdits ?? mParamsEdit ?? mScopeEdit
makeLhs True params mParamsEdit mScopeEdit _ myId = mkLightLambda params myId ?? mParamsEdit ?? mScopeEdit
makeLhs _ _ mParamsEdit mScopeEdit lhsEventMap _ = mkExpanded ?? mParamsEdit ?? mScopeEdit <&> Lens.ix 0 %~ M.weakerEvents lhsEventMap

make ::
    _ =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    Widget.Id ->
    Sugar.Params (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o ->
    GuiM env i o (EventMap (o GuiState.Update), Responsive o)
make mScopeCursor isScopeNavFocused delVarBackwardsId myId bodyId params =
    makeBody annotationMode delVarBackwardsId myId bodyId params
    & GuiM.withLocalMScopeId (mScopeCursor <&> Lens.traversed %~ (^. Sugar.bParamScopeId) . sBinderScope)
    >>= _2 mAddFrame
    where
        mAddFrame [x] = pure x
        mAddFrame xs = Styled.addValFrame <*> (Options.boxSpaced ?? Options.disambiguationNone ?? xs)
        mCurCursor =
            do
                ScopeNavIsFocused == isScopeNavFocused & guard
                mScopeCursor ^. current
        annotationMode =
            Annotation.NeighborVals
            (mCurCursor >>= sMPrevParamScope)
            (mCurCursor >>= sMNextParamScope)
            & Annotation.WithNeighbouringEvalAnnotations

makeBody ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.Params (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o ->
    GuiM env i o (EventMap (o GuiState.Update), [Responsive o])
makeBody annotationOpts delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.NullParam (p, actions) ->
        Widget.weakerEvents
        <$> ( Lens.view id <&>
                \env ->
                E.keyPresses (env ^. has . (Config.delForwardKeys <> Config.delBackwardKeys))
                (E.toDoc env [has . MomentuTexts.edit, has . MomentuTexts.delete])
                (GuiState.updateCursor rhsId <$ actions ^. Sugar.npDeleteLambda)
            )
        <*> ( (Widget.makeFocusableView ?? nullParamId <&> (M.tValue %~))
                <*> grammar (label Texts.defer)
                >>= ParamEdit.addAnnotation annotationOpts p nullParamId
            )
        <&> (:[])
        <&> (,) mempty
        where
            nullParamId = Widget.joinId lhsId ["param"]
    Sugar.RecordParams items -> ParamEdit.makeParams annotationOpts delVarBackwardsId rhsId items
    Sugar.VarParam (param, pInfo) ->
        do
            env <- Lens.view id
            let eventMap =
                    TaggedList.delEventMap (has . Texts.parameter) (pInfo ^. Sugar.vpiDelete) delVarBackwardsId rhsId env <>
                    ParamEdit.eventMapAddNextParamOrPickTag widgetId (pInfo ^. Sugar.vpiAddNext) env
            mconcat
                [ foldMap (`ParamEdit.mkAddParam` lhsId) (pInfo ^? Sugar.vpiAddPrev . Sugar._AddNext)
                , TagEdit.makeParamTag (Just (tag ^. Sugar.oPickAnon)) (tag ^. Sugar.oTag)
                    >>= ParamEdit.addAnnotation annotationOpts param widgetId
                    <&> M.weakerEvents eventMap <&> (:[])
                , foldMap (`ParamEdit.mkAddParam` widgetId) (pInfo ^? Sugar.vpiAddNext . Sugar._AddNext)
                ]
                <&> (,) (ParamEdit.eventMapAddNextParamOrPickTag lhsId (pInfo ^. Sugar.vpiAddPrev) env)
        where
            tag = pInfo ^. Sugar.vpiTag
            widgetId = tag ^. Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
    & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods M.Key'Space])
