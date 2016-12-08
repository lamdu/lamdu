{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, RankNTypes, DisambiguateRecordFields, NamedFieldPuns, OverloadedStrings #-}
module Lamdu.GUI.Main
    ( make
    , Env(..), CodeEdit.ExportActions(..)
      , envEvalRes, envExportActions
      , envConfig, envSettings, envStyle, envFullSize, envCursor
    , CodeEdit.M(..), CodeEdit.m
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import           Graphics.UI.Bottle.WidgetsEnvT (runWidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.Scroll as Scroll
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import           Lamdu.Style (Style)
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VersionControl.Actions

import           Lamdu.Prelude

type T = Transaction

data Env = Env
    { _envEvalRes :: CurAndPrev (EvalResults (ExprIRef.ValI DbLayout.ViewM))
    , _envExportActions :: CodeEdit.ExportActions DbLayout.ViewM
    , _envConfig :: Config
    , _envSettings :: Settings
    , _envStyle :: Style
    , _envFullSize :: Widget.Size
    , _envCursor :: Widget.Id
    }
Lens.makeLenses ''Env

make ::
    Env -> Widget.Id ->
    T DbLayout.DbM (Widget (CodeEdit.M DbLayout.DbM Widget.EventResult))
make env rootId =
    do
        actions <-
            VersionControl.makeActions
            <&> VersionControl.Actions.hoist CodeEdit.mLiftTrans
        runWidgetEnvT widgetEnv $
            do
                branchGui <-
                    VersionControlGUI.make (Config.versionControl config)
                    CodeEdit.mLiftTrans id actions $
                    \branchSelector ->
                    do
                        let codeSize = fullSize - Vector2 0 (branchSelector ^. Widget.height)
                        codeEdit <-
                            CodeEdit.make codeEditEnv rootId ?? (codeSize ^. _1)
                            & WE.mapWidgetEnvT VersionControl.runAction
                            <&> Widget.events . CodeEdit.m %~ fmap (VersionControl.runEvent cursor)
                        hoverPadding <-
                            Config.pane config & Config.paneHoverPadding
                            & BWidgets.vspacer
                        let scrollBox =
                                Box.vbox [(0.5, hoverPadding), (0.5, codeEdit)]
                                & Widget.padToSizeAlign codeSize 0
                                & Scroll.focusAreaIntoWindow fullSize
                                & Widget.size .~ codeSize
                        Box.vbox [(0.5, scrollBox), (0.5, branchSelector)]
                            & return
                let quitEventMap =
                        Widget.keysEventMap (Config.quitKeys config) (EventMap.Doc ["Quit"]) (error "Quit")
                branchGui
                    & Widget.strongerEvents quitEventMap
                    & return
    where
        Env evalResults exportActions config settings style fullSize cursor = env
        codeEditEnv = CodeEdit.Env
            { codeProps = DbLayout.codeProps
            , evalResults
            , config
            , settings
            , style
            , exportActions
            }
        widgetEnv = WE.Env
            { WE._envCursor = cursor
            , WE._envTextStyle = Style.styleBase style
            , WE.stdSpacing = Config.stdSpacing config
            }
