{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.Main
    ( make
    ) where

import           Control.Lens.Operators
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import           Graphics.UI.Bottle.WidgetsEnvT (runWidgetEnvT, Env(..))
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Eval.Results (EvalResults(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.VersionControl as VersionControl

make ::
    EvalResults (ExprIRef.ValI DbLayout.ViewM) ->
    Config -> Settings -> TextEdit.Style ->
    Widget.Size -> Widget.Id -> Widget.Id ->
    Transaction DbLayout.DbM (Widget (Transaction DbLayout.DbM))
make evalMap config settings style fullSize cursor rootId =
    do
        actions <- VersionControl.makeActions
        let widgetEnv = Env
                { _envCursor = cursor
                , _envTextStyle = style
                , backgroundCursorId = WidgetIds.backgroundCursorId
                , cursorBGColor = Config.cursorBGColor config
                , layerCursor = Config.layerCursor $ Config.layers config
                , layerInterval = Config.layerInterval $ Config.layers config
                , verticalSpacing = Config.verticalSpacing config
                , stdSpaceWidth = Config.spaceWidth config
                }
        runWidgetEnvT widgetEnv $
            do
                branchGui <-
                    VersionControlGUI.make (Config.versionControl config)
                    (Config.layerChoiceBG (Config.layers config))
                    id actions $
                    \branchSelector ->
                        do
                            let nonCodeHeight =
                                    hoverPadding   ^. Widget.height +
                                    branchSelector ^. Widget.height
                            let codeSize = fullSize - Vector2 0 nonCodeHeight
                            codeEdit <-
                                CodeEdit.make (env codeSize) rootId
                                & WE.mapWidgetEnvT VersionControl.runAction
                                <&> Widget.events %~ VersionControl.runEvent cursor
                                <&> Widget.padToSizeAlign codeSize 0
                            Box.vbox [(0.5, hoverPadding), (0.5, codeEdit), (0.5, branchSelector)]
                                & return
                let quitEventMap =
                        Widget.keysEventMap (Config.quitKeys config) (EventMap.Doc ["Quit"]) (error "Quit")
                branchGui
                    & Widget.strongerEvents quitEventMap
                    & return
    where
        hoverPadding = Spacer.makeWidget $ Vector2 0 $ Config.paneHoverPadding $ Config.pane config
        env size = CodeEdit.Env
            { CodeEdit.codeProps = DbLayout.codeProps
            , CodeEdit.evalMap = evalMap
            , CodeEdit.totalSize = size
            , CodeEdit.config = config
            , CodeEdit.settings = settings
            }
