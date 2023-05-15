module Lamdu.GUI.Expr.ParamsEdit
    ( make
    , ScopeCursor(..), scopeCursor
    , IsScopeNavFocused(..)
    , makeLhs
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev, current)
import           GUI.Momentu (Responsive, EventMap, noMods, Update)
import qualified GUI.Momentu as M
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import           GUI.Momentu.Widgets.StdKeys (dirKey)
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Annotation as Annotation
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.LightLambda as LightLambda
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import           Lamdu.GUI.Styled (grammar, label, withColor)
import qualified Lamdu.GUI.TaggedList as TaggedList
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Style as Style
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
            , sMPrevParamScope = prevs ^? Lens._last
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

addScopeEdit :: _ => Maybe (M.Widget o) -> Responsive o -> m (Responsive o)
addScopeEdit Nothing paramsEdit = pure paramsEdit
addScopeEdit (Just scopeEdit) paramsEdit =
    pure paramsEdit M./-/ pure (M.WithTextPos 0 scopeEdit)
    <&> Responsive.rWide . Responsive.lForm .~ Responsive.WideOneLiner

mkLhsEdits :: _ => Responsive o -> Maybe (M.Widget o) -> m [Responsive o]
mkLhsEdits paramsEdit mScopeEdit =
    addScopeEdit mScopeEdit paramsEdit <&> (:[])

mkExpanded :: _ => Responsive o -> Maybe (M.Widget o) -> f [Responsive o]
mkExpanded paramsEdit mScopeEdit =
    do
        lam <- grammar (label Texts.lam) <&> Responsive.fromTextView
        lhsEdits <- mkLhsEdits paramsEdit mScopeEdit
        arrow <- grammar (label Texts.arrow) <&> Responsive.fromTextView
        pure (lam : lhsEdits <> [arrow])

mkShrunk :: _ => [Sugar.EntityId] -> ElemId -> Maybe (M.Widget o) -> f [Responsive o]
mkShrunk paramIds myId mScopeEdit =
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
            grammar (label Texts.lam)
            >>=  M.tValue (Widget.makeFocusableView (lamId myId))
            <&> Responsive.fromWithTextPos
            & local (TextView.underline ?~ LightLambda.underline theme)
        addScopeEdit mScopeEdit lamLabel
            <&> M.weakerEvents expandEventMap
    <&> (:[])

lamId :: ElemId -> ElemId
lamId = (<> "lam")

mkLightLambda ::
    _ =>
    Sugar.LhsNames a i o v -> ElemId -> Responsive o -> Maybe (M.Widget o) -> f [Responsive o]
mkLightLambda params myId paramsEdit mScopeEdit =
    do
        isSelected <-
            paramIds <&> WidgetIds.fromEntityId
            & traverse GuiState.isSubCursor
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
                 mkExpanded paramsEdit mScopeEdit
                 <&> Lens.mapped %~
                     M.weakerEvents shrinkEventMap
            else mkShrunk paramIds myId mScopeEdit
    where
        paramIds =
            case params of
            Sugar.LhsVar p -> [p ^. Sugar.vTag . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance]
            Sugar.LhsRecord ps -> ps ^.. SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagInstance

makeLhs ::
    _ =>
    Bool -> Sugar.LhsNames a i o v ->
    Responsive o -> Maybe (Widget.Widget o) -> EventMap (o Update) -> ElemId ->
    m [Responsive o]
makeLhs _ (Sugar.LhsVar p) paramsEdit mScopeEdit _ _
    | p ^. Sugar.vIsNullParam = mkLhsEdits paramsEdit mScopeEdit
makeLhs True params paramsEdit mScopeEdit _ myId = mkLightLambda params myId paramsEdit mScopeEdit
makeLhs _ _ paramsEdit mScopeEdit lhsEventMap _ = mkExpanded paramsEdit mScopeEdit <&> Lens.ix 0 %~ M.weakerEvents lhsEventMap

make ::
    _ =>
    Bool ->
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    ElemId -> ElemId ->
    ElemId ->
    Sugar.LhsNames Name i o (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) ->
    GuiM env i o (EventMap (o Update), Responsive o)
make isLet mScopeCursor isScopeNavFocused delVarBackwardsId myId bodyId params =
    makeBody isLet annotationMode delVarBackwardsId myId bodyId params
    & fixScopeId
    where
        mCurCursor =
            do
                ScopeNavIsFocused == isScopeNavFocused & guard
                mScopeCursor ^. current
        fixScopeId
            | isLet = id
            | otherwise =
                GuiM.withLocalMScopeId
                (mScopeCursor <&> Lens.traversed %~ (^. Sugar.bParamScopeId) . sBinderScope)
        annotationMode
            | isLet = Annotation.NormalEvalAnnotation
            | otherwise =
                Annotation.NeighborVals
                (mCurCursor >>= sMPrevParamScope)
                (mCurCursor >>= sMNextParamScope)
                & Annotation.WithNeighbouringEvalAnnotations

makeBody ::
    _ =>
    Bool ->
    Annotation.EvalAnnotationOptions ->
    ElemId -> ElemId -> ElemId ->
    Sugar.LhsNames Name i o (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) ->
    GuiM env i o (EventMap (o Update), Responsive o)
makeBody isLet annotationOpts delVarBackwardsId lhsId rhsId params =
    case params of
    Sugar.LhsRecord items -> makeParams annotationOpts delVarBackwardsId rhsId items
    Sugar.LhsVar p | p ^. Sugar.vIsNullParam ->
        Widget.weakerEvents
        <$> ( Lens.view id <&>
                \env ->
                E.keyPresses (env ^. has . (Config.delForwardKeys <> Config.delBackwardKeys))
                (E.toDoc env [has . MomentuTexts.edit, has . MomentuTexts.delete])
                (GuiState.updateCursor rhsId <$ p ^. Sugar.vDelete)
            )
        <*> ( grammar (label Texts.defer)
                >>= M.tValue (Widget.makeFocusableView nullParamId)
                >>= ParamEdit.addAnnotationAndEvents annotationOpts (p ^. Sugar.vParam) nullParamId
            )
        <&> (,) mempty
        where
            nullParamId = lhsId <> "param"
    Sugar.LhsVar p ->
        do
            env <- Lens.view id
            let eventMap
                    | isLet = mempty
                    | otherwise =
                        TaggedList.delEventMap [env ^. has . Texts.parameter] (p ^. Sugar.vDelete)
                        (pure delVarBackwardsId) (pure rhsId) env <>
                        ParamEdit.eventMapAddNextParamOrPickTag widgetId (p ^. Sugar.vAddNext) env
            mconcat
                [ foldMap (`ParamEdit.mkAddParam` lhsId) (p ^? Sugar.vAddPrev . Sugar._AddNext)
                , TagEdit.makeLHSTag onPickNext TextColors.variableColor (Just (tag ^. Sugar.oPickAnon)) (tag ^. Sugar.oTag)
                    >>= ParamEdit.addAnnotationAndEvents annotationOpts (p ^. Sugar.vParam) widgetId
                    <&> M.weakerEvents eventMap <&> (:[])
                , foldMap (`ParamEdit.mkAddParam` widgetId) (p ^? Sugar.vAddNext . Sugar._AddNext)
                ] >>= Options.boxSpaced Options.disambiguationNone
                <&> (,) (ParamEdit.eventMapAddNextParamOrPickTag lhsId (p ^. Sugar.vAddPrev) env)
        where
            tag = p ^. Sugar.vTag
            widgetId = tag ^. Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
            onPickNext pos
                | isLet = Nothing
                | otherwise = WidgetIds.fromEntityId pos & TagEdit.addItemId & Just
    & local
        (has . Menu.configKeysPickOptionAndGotoNext .~
            [noMods M.Key'Space | not isLet || Lens.has Sugar._LhsRecord params])

makeParams ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    ElemId -> ElemId ->
    Sugar.TaggedList Name i o (Sugar.LhsField Name (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name)) ->
    GuiM env i o (EventMap (o Update), Responsive o)
makeParams annotationOpts prevId nextId items =
    do
        env <- Lens.view id
        let o = dirKey (env ^. has) Horizontal
        keys <-
            traverse Lens.view TaggedList.Keys
            { TaggedList._kAdd = has . Config.addNextParamKeys
            , TaggedList._kOrderBefore = has . Config.orderDirKeys . o Backward
            , TaggedList._kOrderAfter = has . Config.orderDirKeys . o Forward
            }
        (addFirstEventMap, itemsR) <-
            TaggedList.make [env ^. has . Texts.parameter] keys (pure prevId) (pure nextId) items
        sequenceA
            [ mkLabel (label Texts.recordOpener)
            , ParamEdit.mkAddParam (items ^. Sugar.tlAddFirst) prevId
                <> (traverse makeParam itemsR <&> concat)
                >>= Options.boxSpaced Options.disambiguationNone
            , mkLabel (label Texts.recordCloser)
            ] >>= Options.box Options.disambiguationNone
            <&> (,) addFirstEventMap
    where
        mkLabel x = grammar x <&> Responsive.fromTextView
        makeParam x =
            do
                p <-
                    foldr
                    (`GuiState.assignCursor` myId)
                    (ParamEdit.makeParam annotationOpts x)
                    (x ^.. TaggedList.iValue . Sugar.fSubFields . Lens._Just . traverse . _1 . Sugar.tagInstance
                        <&> WidgetIds.fromEntityId)
                s <-
                    traverse makeSubFields
                    (x ^.. TaggedList.iValue . Sugar.fSubFields . Lens._Just)
                    & local (M.elemIdPrefix .~ M.asElemId myId)
                newP0 <- Options.box Options.disambiguationNone (p ^.. Lens.ix 0 <> s)
                p & Lens.ix 0 .~ newP0 & pure
            where
                myId = x ^. TaggedList.iTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
        makeSubFields subFields =
            sequenceA
            [ mkLabel (label Texts.recordOpener)
            , traverse makeSubField subFields >>= Options.boxSpaced Options.disambiguationNone
            , mkLabel (label Texts.recordCloser)
            ] >>= Options.box Options.disambiguationNone
        makeSubField (subTag, f) =
            -- TODO: Recursive sub-fields
            NameView.make (subTag ^. Sugar.tagName)
            & withColor TextColors.variableColor
            & local (\env -> env & has .~ env ^. has . Style.nameAtBinder)
            <&> M.tValue %~ Widget.fromView
            >>= ParamEdit.addAnnotation annotationOpts (f ^. Sugar.fParam) subId
            & local (M.elemIdPrefix .~ M.asElemId subId)
            where
                subId = WidgetIds.fromEntityId (subTag ^. Sugar.tagInstance)
