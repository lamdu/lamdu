{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make, makeGetBinder
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as SBS8
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Font (Underline(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout)
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.Calc.Type.Scheme (schemeType)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Hover as Hover
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeSimpleView ::
    (Monad f, Monad m) =>
    Name m -> Widget.Id ->
    ExprGuiM m (ExpressionGui f)
makeSimpleView name myId =
    ExprGuiM.widgetEnv (BWidgets.makeFocusableView myId)
    <*> (ExpressionGui.makeNameView name (Widget.toAnimId myId) <&> Widget.fromView)
    <&> TreeLayout.fromCenteredWidget

makeParamsRecord ::
    Monad m => Widget.Id -> Sugar.ParamsRecordVar (Name m) ->
    ExprGuiM m (ExpressionGui m)
makeParamsRecord myId paramsRecordVar =
    do
        config <- ExprGuiM.readConfig
        let Config.Name{..} = Config.name config
        sequence
            [ ExpressionGui.makeLabel "Params {"
              (Widget.toAnimId myId <> ["prefix"])
              <&> TreeLayout.fromAlignedWidget
            , ExpressionGui.combineSpaced Nothing
              <*>
              ( fieldNames
                & Lens.itraverse
                (\i fieldName ->
                    Widget.joinId myId ["params", SBS8.pack (show (i::Int))]
                    & makeSimpleView fieldName
                    & ExprGuiM.withFgColor parameterColor
                )
              )
            , ExpressionGui.makeLabel "}" (Widget.toAnimId myId <> ["suffix"])
              <&> TreeLayout.fromAlignedWidget
            ] <&> ExpressionGui.combine
    where
        Sugar.ParamsRecordVar fieldNames = paramsRecordVar

makeNameRef ::
    Monad m => Widget.Id -> Sugar.NameRef name m ->
    (name -> Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
makeNameRef myId nameRef makeView =
    do
        cp <- ExprGuiM.readCodeAnchors
        config <- ExprGuiM.readConfig
        let jumpToDefinitionEventMap =
                Widget.keysEventMapMovesCursor
                (Config.jumpToDefinitionKeys config ++ Config.extractKeys config)
                (E.Doc ["Navigation", "Jump to definition"]) $
                do
                    DataOps.savePreJumpPosition cp myId
                    WidgetIds.fromEntityId <$> nameRef ^. Sugar.nrGotoDefinition
        makeView (nameRef ^. Sugar.nrName) nameId
            <&> TreeLayout.widget %~ Widget.weakerEvents jumpToDefinitionEventMap
    & ExprGuiM.assignCursor myId nameId
    where
        nameId = Widget.joinId myId ["name"]

makeInlineEventMap ::
    Monad m =>
    Config -> Sugar.BinderVarInline m ->
    Widget.EventMap (T m Widget.EventResult)
makeInlineEventMap config (Sugar.InlineVar inline) =
    inline <&> WidgetIds.fromEntityId
    & Widget.keysEventMapMovesCursor (Config.inlineKeys config)
      (E.Doc ["Edit", "Inline"])
makeInlineEventMap config (Sugar.CannotInlineDueToUses (x:_)) =
    WidgetIds.fromEntityId x & return
    & Widget.keysEventMapMovesCursor (Config.inlineKeys config)
      (E.Doc ["Navigation", "Jump to next use"])
makeInlineEventMap _ _ = mempty

definitionTypeChangeBox ::
    Monad m =>
    Sugar.DefinitionOutdatedType m -> Widget.Id ->
    ExprGuiM m (AlignedWidget (T m Widget.EventResult))
definitionTypeChangeBox info getVarId =
    do
        headerLabel <-
            ExpressionGui.makeLabel "Type was:" animId
        typeWhenUsed <-
            mkTypeWidget "typeWhenUsed" (info ^. Sugar.defTypeWhenUsed)
        spacing <- ExpressionGui.stdVSpace <&> AlignedWidget.fromCenteredWidget
        sepLabel <-
            ExpressionGui.makeFocusableView myId
            <*> ExpressionGui.makeLabel "Update to:" animId
        typeCurrent <- mkTypeWidget "typeCurrent" (info ^. Sugar.defTypeCurrent)
        config <- ExprGuiM.readConfig
        let padding = realToFrac <$> Config.valFramePadding config
        let box =
                [headerLabel, typeWhenUsed, spacing, sepLabel, typeCurrent]
                <&> AlignedWidget.alignment .~ 0
                & AlignedWidget.vbox 0
                & AlignedWidget.pad padding
                & AlignedWidget.alignment .~ 0
                & AlignedWidget.widget %~
                    Hover.addBackground animId (Config.hoverBGColor config)
        -- TODO: unify config's button press keys
        let keys = Config.newDefinitionButtonPressKeys (Config.pane config)
        let update = (info ^. Sugar.defTypeUseCurrent) >> return getVarId
        Hover.addDarkBackground animId
            ?? box
            <&> AlignedWidget.widget %~
                Widget.weakerEvents
                (Widget.keysEventMapMovesCursor keys
                 (E.Doc ["Edit", "Update definition type"]) update)
    where
        mkTypeWidget idSuffix scheme =
            TypeView.make (scheme ^. schemeType) (animId ++ [idSuffix])
            <&> Widget.fromView <&> AlignedWidget.fromCenteredWidget
        myId = Widget.joinId getVarId ["type change"]
        animId = Widget.toAnimId myId

processDefinitionWidget ::
    Monad m =>
    Sugar.DefinitionForm m -> Widget.Id ->
    ExprGuiM m (TreeLayout (T m Widget.EventResult)) ->
    ExprGuiM m (TreeLayout (T m Widget.EventResult))
processDefinitionWidget Sugar.DefUpToDate _myId mkLayout = mkLayout
processDefinitionWidget Sugar.DefDeleted myId mkLayout =
    do
        addDiagonal <- ExpressionGui.addDeletionDiagonal
        mkLayout <&> TreeLayout.widget . Widget.view %~ addDiagonal 0.1 animId
    where
        animId = Widget.toAnimId myId
processDefinitionWidget (Sugar.DefTypeChanged info) myId mkLayout =
    do
        config <- ExprGuiM.readConfig
        layout <-
            ExprGuiM.withLocalUnderline Underline
                { _underlineColor = Config.typeIndicatorErrorColor config
                , _underlineWidth = Config.underlineWidth config
                }
            mkLayout
        isSelected <- ExprGuiM.widgetEnv $ WE.isSubCursor myId
        if isSelected
            then
            do
                box <- definitionTypeChangeBox info myId
                layout
                    & TreeLayout.alignment . _1 .~ 0
                    & TreeLayout.alignedWidget %~
                        AlignedWidget.addAfter AlignedWidget.Vertical
                        [box `AlignedWidget.hoverInPlaceOf` AlignedWidget.empty]
                    & return
            else return layout

makeGetBinder ::
    Monad m =>
    Sugar.BinderVar (Name m) m -> Widget.Id ->
    ExprGuiM m (TreeLayout (T m Widget.EventResult))
makeGetBinder binderVar myId =
    do
        config <- ExprGuiM.readConfig
        let Config.Name{..} = Config.name config
        let (color, processDef) =
                case binderVar ^. Sugar.bvForm of
                Sugar.GetLet -> (letColor, id)
                Sugar.GetDefinition defForm ->
                    ( definitionColor
                    , processDefinitionWidget defForm myId
                    )
        makeSimpleView
            <&> Lens.mapped %~ ExprGuiM.withFgColor color
            & makeNameRef myId (binderVar ^. Sugar.bvNameRef)
            <&> TreeLayout.widget %~
            Widget.weakerEvents
            (makeInlineEventMap config (binderVar ^. Sugar.bvInline))
            & processDef


make ::
    Monad m =>
    Sugar.GetVar (Name m) m ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make getVar pl =
    do
        config <- ExprGuiM.readConfig
        let Config.Name{..} = Config.name config
        case getVar of
            Sugar.GetBinder binderVar -> makeGetBinder binderVar myId
            Sugar.GetParam param ->
                case param ^. Sugar.pBinderMode of
                Sugar.LightLambda ->
                    makeSimpleView
                    <&> Lens.mapped %~ LightLambda.withUnderline config
                    <&> Lens.mapped %~ ExpressionGui.styleNameOrigin name parameterColor
                _ ->
                    makeSimpleView
                    <&> Lens.mapped %~ ExprGuiM.withFgColor parameterColor
                & makeNameRef myId (param ^. Sugar.pNameRef)
                where
                    name = param ^. Sugar.pNameRef . Sugar.nrName
            Sugar.GetParamsRecord paramsRecordVar -> makeParamsRecord myId paramsRecordVar
            & ExpressionGui.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl
