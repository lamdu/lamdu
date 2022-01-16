module Lamdu.GUI.Expr.ParamsEdit
    ( make
    , ScopeCursor(..), scopeCursor
    , IsScopeNavFocused(..)
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev, current)
import qualified GUI.Momentu as M
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (noMods)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Annotation as Annotation
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
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
