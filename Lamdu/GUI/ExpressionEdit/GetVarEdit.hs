{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as SBS8
import           Data.Store.Transaction (Transaction)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Font (Underline(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout)
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeSimpleView ::
    (Monad f, Monad m) =>
    Draw.Color -> Name m -> Widget.Id ->
    ExprGuiM m (ExpressionGui f)
makeSimpleView color name myId =
    ExprGuiM.widgetEnv (BWidgets.makeFocusableView myId)
    <*> (ExpressionGui.makeNameView name (Widget.toAnimId myId) <&> Widget.fromView)
    & ExprGuiM.withFgColor color
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
                    & makeSimpleView parameterColor fieldName
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
        makeView (nameRef ^. Sugar.nrName) myId
            <&> TreeLayout.widget %~ Widget.weakerEvents jumpToDefinitionEventMap

makeInlineEventMap ::
    Monad m =>
    Config -> Sugar.BinderVarInline m ->
    Widget.EventMap (Transaction m Widget.EventResult)
makeInlineEventMap config (Sugar.InlineVar inline) =
    inline <&> WidgetIds.fromEntityId
    & Widget.keysEventMapMovesCursor (Config.inlineKeys config)
      (E.Doc ["Edit", "Inline"])
makeInlineEventMap config (Sugar.CannotInlineDueToUses (x:_)) =
    WidgetIds.fromEntityId x & return
    & Widget.keysEventMapMovesCursor (Config.inlineKeys config)
      (E.Doc ["Navigation", "Jump to next use"])
makeInlineEventMap _ _ = mempty

processDefinitionWidget ::
    Functor m =>
    Sugar.DefinitionForm m -> Widget.Id ->
    ExprGuiM m (TreeLayout a) -> ExprGuiM m (TreeLayout a)
processDefinitionWidget defForm myId =
    deleted . outdated
    where
        deleted =
            Lens.mapped . TreeLayout.widget . Widget.view %~
            ExpressionGui.deletionDiagonal 0.1
            (Widget.toAnimId myId) (defForm ^. Sugar.defLifeState)
        outdated =
            case defForm ^. Sugar.defTypeState of
            Sugar.DefTypeUpToDate -> id
            Sugar.DefTypeChanged _info ->
                ExprGuiM.withLocalUnderline Underline
                { _underlineColor = Draw.Color 1 0 0 1
                , _underlineWidth = 2
                }

make ::
    Monad m =>
    Sugar.GetVar (Name m) m ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make getVar pl =
    do
        config <- ExprGuiM.readConfig
        let Config.Name{..} = Config.name config
        ExpressionGui.stdWrap pl
            <*>
            case getVar of
            Sugar.GetBinder binderVar ->
                case binderVar ^. Sugar.bvForm of
                Sugar.GetLet -> (letColor, id)
                Sugar.GetDefinition defForm ->
                    ( definitionColor
                    , processDefinitionWidget defForm myId
                    )
                & \(color, processDef) ->
                    makeSimpleView color
                    <&> Lens.mapped %~ processDef
                    & makeNameRef myId (binderVar ^. Sugar.bvNameRef)
                    <&> TreeLayout.widget %~
                    Widget.weakerEvents
                    (makeInlineEventMap config (binderVar ^. Sugar.bvInline))
            Sugar.GetParam param ->
                case param ^. Sugar.pBinderMode of
                Sugar.LightLambda ->
                    makeSimpleView nameOriginFGColor
                    <&> Lens.mapped %~
                        LightLambda.withUnderline (Config.lightLambda config)
                _ -> makeSimpleView parameterColor
                & makeNameRef myId (param ^. Sugar.pNameRef)
            Sugar.GetParamsRecord paramsRecordVar -> makeParamsRecord myId paramsRecordVar
    where
        myId = WidgetIds.fromExprPayload pl
