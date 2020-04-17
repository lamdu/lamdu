module Lamdu.GUI.Expr.RecordEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedList, tagPost)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

doc ::
    ( Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    env -> Lens.ALens' (Texts.CodeUI Text) Text -> E.Doc
doc env lens = E.toDoc env [has . MomentuTexts.edit, has . Texts.record, has . lens]

addFieldId :: Widget.Id -> Widget.Id
addFieldId = (`Widget.joinId` ["add field"])

mkAddFieldEventMap ::
    ( MonadReader env m
    , Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Applicative o
    ) =>
    Widget.Id -> m (EventMap (o GuiState.Update))
mkAddFieldEventMap myId =
    Lens.view id
    <&>
    \env ->
    addFieldId myId
    & pure
    & E.keysEventMapMovesCursor (env ^. has . Config.recordAddFieldKeys)
    (doc env Texts.addField)

addFieldWithSearchTermEventMap ::
    ( Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Applicative o
    ) => env -> Widget.Id -> EventMap (o GuiState.Update)
addFieldWithSearchTermEventMap env myId =
    E.charEventMap "Letter" (doc env Texts.addField) f
    where
        f c
            | Char.isAlpha c =
                addFieldId myId
                & SearchMenu.enterWithSearchTerm (Text.singleton c)
                & pure
                & Just
            | otherwise = Nothing

makeUnit ::
    ( Monad i, Monad o
    , Has (Texts.Definitions Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Grid.HasTexts env
    ) =>
    (Sugar.Payload (Sugar.EvaluationScopes Name i) Name i o, ExprGui.Payload) ->
    GuiM env i o (Responsive o)
makeUnit pl =
    do
        makeFocusable <- Widget.makeFocusableView ?? myId <&> (Align.tValue %~)
        addFieldEventMap <- mkAddFieldEventMap myId
        env <- Lens.view id
        grammar (label Texts.recordOpener)
            /|/ grammar (label Texts.recordCloser)
            <&> makeFocusable
            <&> Align.tValue %~ Widget.weakerEvents
                (addFieldEventMap <> addFieldWithSearchTermEventMap env myId)
            <&> Responsive.fromWithTextPos
            & stdWrap pl
    where
        myId = WidgetIds.fromExprPayload (pl ^. _1)

make ::
    ( Monad i, Monad o
    , SearchMenu.HasTexts env
    , Grid.HasTexts env
    , Has (TextEdit.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Expr Sugar.Composite (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Composite [] [] Sugar.ClosedComposite{} addField)) =
    -- Ignore the ClosedComposite actions - it only has the open
    -- action which is equivalent ot deletion on the unit record
    do
        isAddField <- GuiState.isSubCursor ?? addFieldId (WidgetIds.fromExprPayload (pl ^. _1))
        if isAddField
            then
                makeAddFieldRow addField (pl ^. _1) <&> (:[]) >>= makeRecord pure
                & stdWrapParentExpr pl
            else makeUnit pl
make (Ann (Const pl) (Sugar.Composite fields punned recordTail addField)) =
    do
        addFieldEventMap <- mkAddFieldEventMap (WidgetIds.fromExprPayload (pl ^. _1))
        tailEventMap <-
            case recordTail of
            Sugar.ClosedComposite actions ->
                closedRecordEventMap actions
            Sugar.OpenComposite actions restExpr ->
                openRecordEventMap actions restExpr
        punnedGuis <-
            case punned of
            [] -> pure []
            _ ->
                GetVarEdit.makePunnedVars punned
                <&> (\x -> [TaggedItem Nothing x Nothing])
        fieldGuis <- traverse makeFieldRow fields <&> (++ punnedGuis)
        isAddField <- GuiState.isSubCursor ?? addFieldId (WidgetIds.fromExprPayload (pl ^. _1))
        addFieldGuis <-
            if isAddField
            then makeAddFieldRow addField (pl ^. _1) <&> (:[])
            else pure []
        env <- Lens.view id
        let goToRecordEventMap =
                WidgetIds.fromExprPayload (pl ^. _1) & GuiState.updateCursor & pure & const
                & E.charGroup Nothing
                (E.toDoc env
                    [ has . MomentuTexts.navigation
                    , has . Texts.goToParent
                    ]) "}"
        makeRecord postProcess (fieldGuis ++ addFieldGuis)
            <&> Widget.weakerEvents goToRecordEventMap
            <&> Widget.weakerEvents (addFieldEventMap <> tailEventMap)
            & stdWrapParentExpr pl
    where
        postProcess =
            case recordTail of
            Sugar.OpenComposite actions restExpr ->
                makeOpenRecord actions restExpr
            _ -> pure

makeRecord ::
    ( MonadReader env m, Has Theme env, Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, Applicative o
    , Glue.HasTexts env, Has (Texts.Code Text) env
    ) =>
    (Responsive o -> m (Responsive o)) ->
    [TaggedItem o] ->
    m (Responsive o)
makeRecord _ [] = error "makeRecord with no fields"
makeRecord postProcess fieldGuis =
    Styled.addValFrame <*>
    ( grammar (label Texts.recordOpener)
        /|/ (taggedList
                <*> addPostTags fieldGuis
                >>= postProcess)
    )

addPostTags ::
    ( MonadReader env m, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env, Has (Texts.Code Text) env, Has Dir.Layout env
    ) =>
    [TaggedItem o] -> m [TaggedItem o]
addPostTags items =
    do
        let f idx item =
                ( if idx < lastIdx
                    then label Texts.recordSep
                    else label Texts.recordCloser
                ) & grammar
                & Element.locallyAugmented idx
                <&> \lbl -> item & tagPost ?~ (lbl <&> Widget.fromView)
        Lens.itraverse f items
    where
        lastIdx = length items - 1

makeAddFieldRow ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    ) =>
    Sugar.TagReplace Name i o Sugar.EntityId ->
    Sugar.Payload v name i o ->
    GuiM env i o (TaggedItem o)
makeAddFieldRow addField pl =
    TagEdit.makeTagHoleEdit addField mkPickResult tagHoleId
    & Styled.withColor TextColors.recordTagColor
    <&>
    \tagHole ->
    TaggedItem
    { _tagPre = Just tagHole
    , _taggedItem = Element.empty
    , _tagPost = Just Element.empty
    }
    where
        tagHoleId = addFieldId (WidgetIds.fromExprPayload pl)
        mkPickResult _ dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId dst
            , Menu._pickMNextEntry = WidgetIds.fromEntityId dst & Just
            }

makeFieldRow ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Body Sugar.CompositeItem (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    GuiM env i o (TaggedItem o)
makeFieldRow (Sugar.CompositeItem delete tag fieldExpr) =
    do
        itemEventMap <- recordDelEventMap delete
        fieldGui <- GuiM.makeSubexpression fieldExpr
        pre <-
            ( TagEdit.makeRecordTag tag
                <&> Align.tValue %~ Widget.weakerEvents itemEventMap
            ) /|/ Spacer.stdHSpace
        pure TaggedItem
            { _tagPre = Just pre
            , _taggedItem = Widget.weakerEvents itemEventMap fieldGui
            , _tagPost = Just Element.empty
            }

separationBar :: TextColors -> Anim.AnimId -> Widget.R -> View
separationBar theme animId width =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (theme ^. TextColors.recordTailColor)
    & Element.scale (Vector2 width 10)

makeOpenRecord ::
    (Monad i, Monad o, Glue.HasTexts env, Has (Texts.CodeUI Text) env) =>
    Sugar.OpenCompositeActions o ->
    Sugar.Expr Sugar.Term (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    Responsive o -> GuiM env i o (Responsive o)
makeOpenRecord (Sugar.OpenCompositeActions close) rest fieldsGui =
    do
        theme <- Lens.view has
        vspace <- Spacer.stdVSpace
        env <- Lens.view id
        let restEventMap =
                close <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env)
                (doc env Texts.close)
        restExpr <-
            Styled.addValPadding <*> GuiM.makeSubexpression rest
            <&> Widget.weakerEvents restEventMap
        animId <- Lens.view Element.animIdPrefix
        (|---|) <- Glue.mkGlue ?? Glue.Vertical
        Responsive.vboxWithSeparator ?? False
            ?? (separationBar (theme ^. Theme.textColors) animId <&> (|---| vspace))
            ?? fieldsGui ?? restExpr

openRecordEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    Sugar.OpenCompositeActions o ->
    Annotated a # Sugar.Term v name i o ->
    m (EventMap (o GuiState.Update))
openRecordEventMap (Sugar.OpenCompositeActions close) restExpr
    | isHole restExpr =
        Lens.view id
        <&>
        \env ->
        close <&> WidgetIds.fromEntityId
        & E.keysEventMapMovesCursor (env ^. has . Config.recordCloseKeys)
        (doc env Texts.close)
    | otherwise = pure mempty
    where
        isHole = Lens.has (hVal . Sugar._BodyHole)

closedRecordEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    Sugar.ClosedCompositeActions o -> m (EventMap (o GuiState.Update))
closedRecordEventMap (Sugar.ClosedCompositeActions open) =
    Lens.view id
    <&>
    \env ->
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.recordOpenKeys)
    (doc env Texts.open)

recordDelEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , Functor o
    ) =>
    o Sugar.EntityId -> m (EventMap (o GuiState.Update))
recordDelEventMap delete =
    Lens.view id
    <&>
    \env ->
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys env) (doc env Texts.deleteField)
