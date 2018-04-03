{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.CaseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Calc.Type (Tag)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (MonadExprGui, IM, AM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

doc :: E.Subtitle -> E.Doc
doc text = E.Doc ["Edit", "Case", text]

addAltId :: Widget.Id -> Widget.Id
addAltId = (`Widget.joinId` ["add alt"])

make ::
    MonadExprGui m =>
    Sugar.Case (Name (AM m)) (IM m) (AM m) (ExprGui.SugarExpr (IM m) (AM m)) ->
    Sugar.Payload (Name (AM m)) (IM m) (AM m) ExprGui.Payload ->
    m (ExpressionGui (AM m))
make (Sugar.Case mArg (Sugar.Composite alts caseTail addAlt)) pl =
    do
        config <- Lens.view Config.config
        let mExprAfterHeader =
                ( alts ^.. Lens.traversed . Lens.traversed
                ++ caseTail ^.. Lens.traversed
                ) ^? Lens.traversed
        labelJumpHoleEventMap <-
            mExprAfterHeader <&> ExprGui.nextHolesBefore
            & maybe (pure mempty) ExprEventMap.jumpHolesEventMap
        let responsiveLabel text =
                Styled.grammarLabel text <&> Responsive.fromTextView
        let headerLabel text =
                (Widget.makeFocusableView ?? headerId <&> (Align.tValue %~))
                <*> Styled.grammarLabel text
                <&> Responsive.fromWithTextPos
                <&> Widget.weakerEvents labelJumpHoleEventMap
        caseLabel <- headerLabel "case"
        ofLabel <- responsiveLabel "of"
        (mActiveTag, header) <-
            case mArg of
            Sugar.LambdaCase ->
                do
                    lambdaLabel <- responsiveLabel "λ"
                    Options.boxSpaced
                        ?? Options.disambiguationNone
                        ?? [caseLabel, lambdaLabel, ofLabel]
                        <&> (,) Nothing
            Sugar.CaseWithArg (Sugar.CaseArg arg toLambdaCase) ->
                do
                    argEdit <-
                        ExprGuiM.makeSubexpression arg
                        <&> Widget.weakerEvents (toLambdaCaseEventMap config toLambdaCase)
                    mTag <-
                        Annotation.evaluationResult (arg ^. Sugar.rPayload)
                        <&> (>>= (^? Sugar.resBody . Sugar._RInject . Sugar.riTag))
                    Options.boxSpaced
                        ?? Options.disambiguationNone
                        ?? [caseLabel, argEdit, ofLabel]
                        <&> (,) mTag
        altsGui <-
            do
                altsGui <-
                    makeAltsWidget (mActiveTag <&> (^. Sugar.tagVal))
                    alts addAlt altsId
                case caseTail of
                    Sugar.ClosedComposite actions ->
                        Widget.weakerEvents (closedCaseEventMap config actions) altsGui
                        & pure
                    Sugar.OpenComposite actions rest ->
                        makeOpenCase actions rest (Widget.toAnimId myId) altsGui
        let addAltEventMap =
                addAltId altsId
                & pure
                & E.keysEventMapMovesCursor (config ^. Config.caseAddAltKeys)
                    (doc "Add Alt")
        stdWrapParentExpr pl
            <*> (Styled.addValFrame <*> (Responsive.vboxSpaced ?? [header, altsGui]))
            <&> Widget.weakerEvents addAltEventMap
    where
        myId = WidgetIds.fromExprPayload pl
        headerId = Widget.joinId myId ["header"]
        altsId = Widget.joinId myId ["alts"]

makeAltRow ::
    MonadExprGui m =>
    Maybe Tag ->
    Sugar.CompositeItem (Name (AM m)) (IM m) (AM m)
    (Sugar.Expression (Name (AM m)) (IM m) (AM m) ExprGui.Payload) ->
    m (Responsive.TaggedItem ((AM m) GuiState.Update))
makeAltRow mActiveTag (Sugar.CompositeItem delete tag altExpr) =
    do
        config <- Lens.view Config.config
        addBg <- Styled.addBgColor Theme.evaluatedPathBGColor
        let itemEventMap = caseDelEventMap config delete
        tagLabel <-
            TagEdit.makeVariantTag (ExprGui.nextHolesBefore altExpr) tag
            <&> Align.tValue %~ Widget.weakerEvents itemEventMap
            <&> if mActiveTag == Just (tag ^. Sugar.tagInfo . Sugar.tagVal)
                then addBg
                else id
        hspace <- Spacer.stdHSpace
        altExprGui <-
            ExprGuiM.makeSubexpression altExpr <&> Widget.weakerEvents itemEventMap
        colonLabel <- Styled.grammarLabel ":"
        pure Responsive.TaggedItem
            { Responsive._tagPre = tagLabel /|/ colonLabel /|/ hspace
            , Responsive._taggedItem = altExprGui
            , Responsive._tagPost = Element.empty
            }
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId altId)
    where
        altId = tag ^. Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId

makeAltsWidget ::
    MonadExprGui m =>
    Maybe Tag ->
    [Sugar.CompositeItem (Name (AM m)) (IM m) (AM m)
     (Sugar.Expression (Name (AM m)) (IM m) (AM m) ExprGui.Payload)] ->
    Sugar.TagSelection (Name (AM m)) (IM m) (AM m) Sugar.EntityId ->
    Widget.Id ->
    m (ExpressionGui (AM m))
makeAltsWidget mActiveTag alts addAlt altsId =
    do
        existingAltWidgets <- traverse (makeAltRow mActiveTag) alts
        newAlts <-
            GuiState.isSubCursor ?? addAltId altsId
            <&> guard
            <&> Lens.mapped .~ makeAddAltRow addAlt (addAltId altsId)
            >>= sequenceA
        case existingAltWidgets ++ newAlts of
            [] ->
                (Widget.makeFocusableView ?? Widget.joinId altsId ["Ø"] <&> (Align.tValue %~))
                <*> Styled.grammarLabel "Ø"
                <&> Responsive.fromWithTextPos
            altWidgtes -> Responsive.taggedList ?? altWidgtes

makeAddAltRow ::
    MonadExprGui m =>
    Sugar.TagSelection (Name (AM m)) (IM m) (AM m) Sugar.EntityId -> Widget.Id ->
    m (Responsive.TaggedItem ((AM m) GuiState.Update))
makeAddAltRow addAlt myId =
    TagEdit.makeTagHoleEdit addAlt mkPickResult myId
    & Styled.withColor TextColors.caseTagColor
    <&>
    \tagHole ->
    Responsive.TaggedItem
    { Responsive._tagPre = tagHole
    , Responsive._taggedItem = Element.empty
    , Responsive._tagPost = Element.empty
    }
    where
        mkPickResult _ dst =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId dst
            , Menu._pickNextEntryPoint = WidgetIds.fromEntityId dst
            }

separationBar :: TextColors -> Widget.R -> Anim.AnimId -> View
separationBar theme width animId =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (theme ^. TextColors.caseTailColor)
    & Element.scale (Vector2 width 10)

makeOpenCase ::
    MonadExprGui m =>
    Sugar.OpenCompositeActions (AM m) -> ExprGui.SugarExpr (IM m) (AM m) ->
    AnimId -> ExpressionGui (AM m) ->
    m (ExpressionGui (AM m))
makeOpenCase actions rest animId altsGui =
    do
        theme <- Lens.view Theme.theme
        vspace <- Spacer.stdVSpace
        restExpr <-
            Styled.addValPadding
            <*> ExprGuiM.makeSubexpression rest
        config <- Lens.view Config.config
        pure $ altsGui & Responsive.render . Lens.imapped %@~
            \layoutMode alts ->
            let restLayout =
                    layoutMode & restExpr ^. Responsive.render
                    <&> Widget.weakerEvents (openCaseEventMap config actions)
                minWidth = restLayout ^. Element.width
                targetWidth = alts ^. Element.width
            in
            alts
            /-/
            separationBar (theme ^. Theme.textColors) (max minWidth targetWidth) animId
            /-/
            vspace
            /-/
            restLayout

openCaseEventMap ::
    Monad am =>
    Config -> Sugar.OpenCompositeActions am ->
    EventMap (am GuiState.Update)
openCaseEventMap config (Sugar.OpenCompositeActions close) =
    close <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys config) (doc "Close")

closedCaseEventMap ::
    Monad am =>
    Config -> Sugar.ClosedCompositeActions am ->
    EventMap (am GuiState.Update)
closedCaseEventMap config (Sugar.ClosedCompositeActions open) =
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (config ^. Config.caseOpenKeys) (doc "Open")

caseDelEventMap ::
    Monad am =>
    Config -> am Sugar.EntityId -> EventMap (am GuiState.Update)
caseDelEventMap config delete =
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys config) (doc "Delete Alt")

toLambdaCaseEventMap ::
    Monad am =>
    Config -> am Sugar.EntityId -> EventMap (am GuiState.Update)
toLambdaCaseEventMap config toLamCase =
    toLamCase <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys config) (doc "Turn to Lambda-Case")
