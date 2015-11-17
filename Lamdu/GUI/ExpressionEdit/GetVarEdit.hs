{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.ByteString.Char8 as SBS8
import           Data.Monoid ((<>))
import           Data.Store.Transaction (Transaction)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
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

import           Prelude.Compat

type T = Transaction

makeSimpleView ::
    MonadA m => Draw.Color -> Name m -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeSimpleView color name myId =
    ExpressionGui.makeNameView name (Widget.toAnimId myId)
    >>= ExprGuiM.widgetEnv . BWidgets.makeFocusableView myId
    <&> ExpressionGui.fromValueWidget
    & ExprGuiM.withFgColor color

makeJumpToDefinitionEventMap ::
    MonadA m => Widget.Id -> Sugar.NameRef name m -> ExprGuiM m (Widget.EventHandlers (T m))
makeJumpToDefinitionEventMap myId nameRef =
    do
        cp <- ExprGuiM.readCodeAnchors
        config <- ExprGuiM.readConfig
        return $ Widget.keysEventMapMovesCursor
            (Config.jumpToDefinitionKeys config ++ Config.extractKeys config)
            (E.Doc ["Navigation", "Jump to definition"]) $
            do
                DataOps.savePreJumpPosition cp myId
                WidgetIds.fromEntityId <$> nameRef ^. Sugar.nrGotoDefinition

make ::
    MonadA m =>
    Sugar.GetVar (Name m) m ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make getVar pl =
    do
        config <- ExprGuiM.readConfig
        let Config.Name{..} = Config.name config
        case getVar of
            Sugar.GetVarNamed namedVar ->
                do
                    jumpToDefinitionEventMap <-
                        makeJumpToDefinitionEventMap myId (namedVar ^. Sugar.nvNameRef)
                    makeView (namedVar ^. Sugar.nvNameRef . Sugar.nrName) myId
                        <&> ExpressionGui.egWidget %~ Widget.weakerEvents jumpToDefinitionEventMap
                        & ExpressionGui.stdWrap pl
                where
                    makeView =
                        case namedVar ^. Sugar.nvMode of
                        Sugar.LightLambda ->
                            makeSimpleView nameOriginFGColor
                            <&> Lens.mapped %~
                                LightLambda.withUnderline (Config.lightLambda config)
                        _ ->
                            case namedVar ^. Sugar.nvVarType of
                            Sugar.GetDefinition -> makeSimpleView definitionColor
                            Sugar.GetParameter -> makeSimpleView parameterColor
                            Sugar.GetFieldParameter -> makeSimpleView parameterColor
            Sugar.GetVarParamsRecord paramsRecordVar ->
                sequence
                [ ExpressionGui.makeLabel "Params {" (Widget.toAnimId myId <> ["prefix"])
                , zip [0..] fieldNames
                    & mapM
                        (\(i, fieldName) ->
                          makeSimpleView parameterColor fieldName $
                          Widget.joinId myId ["params", SBS8.pack (show (i::Int))])
                    >>= ExpressionGui.hboxSpaced
                , ExpressionGui.makeLabel "}" (Widget.toAnimId myId <> ["suffix"])
                ] <&> ExpressionGui.hbox
                & ExpressionGui.stdWrap pl
                where
                    Sugar.ParamsRecordVar fieldNames = paramsRecordVar
    where
        myId = WidgetIds.fromExprPayload pl
