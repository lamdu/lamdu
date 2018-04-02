-- | REPL Edit
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings #-}
module Lamdu.GUI.ReplEdit
    ( ExportRepl(..), make
    ) where

import qualified Control.Lens as Lens
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config, config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM')
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ExportRepl m = ExportRepl
    { exportRepl :: IOTrans m ()
    , -- Fancy export is intended for sending code to someone who doesn't have
      -- Lamdu installed. It bundles together in a zipfile a screenshot,
      -- a README, the repl export, and compiled JS  (that requires a new
      -- version of nodejs supporting TCO).  It is intended to enable Lamdu
      -- to be used in competitions such as Google codejam which require
      -- [readable] code upload.
      exportFancy :: IOTrans m ()
    , -- Executing code is in a way a form of exporting.
      executeIOProcess :: IO ()
    }

extractEventMap ::
    Functor m =>
    Sugar.Expression name (T m) (T m) a -> [MetaKey] ->
    EventMap (T m GuiState.Update)
extractEventMap replExpr keys =
    replExpr ^. Sugar.rPayload . Sugar.plActions . Sugar.extract
    <&> ExprEventMap.extractCursor
    & E.keysEventMapMovesCursor keys (E.Doc ["Edit", "Extract to definition"])

replEventMap ::
    Monad m =>
    Config -> ExportRepl m ->
    Sugar.Expression name (T m) (T m) a -> EventMap (IOTrans m GuiState.Update)
replEventMap theConfig (ExportRepl exportRepl exportFancy _execRepl) replExpr =
    mconcat
    [ extractEventMap replExpr (theConfig ^. Config.extractKeys)
        <&> IOTrans.liftTrans
    , E.keysEventMap (exportConfig ^. Config.exportKeys)
      (E.Doc ["Collaboration", "Export repl to JSON file"]) exportRepl
    , E.keysEventMap (exportConfig ^. Config.exportFancyKeys)
      (E.Doc ["Collaboration", "Export repl as JS"]) exportFancy
    ]
    where
        exportConfig = theConfig ^. Config.export

make ::
    Monad m =>
    ExportRepl m ->
    ExprGui.SugarExpr (T m) (T m) ->
    ExprGuiM' (T m) (Responsive (IOTrans m GuiState.Update))
make exportRepl replExpr =
    do
        theConfig <- Lens.view config
        let buttonExtractKeys = theConfig ^. Config.actionKeys
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? Widget.joinId WidgetIds.replId ["symbol"] <&> (Align.tValue %~))
              <*> TextView.makeLabel "⋙"
              <&> Lens.mapped %~ Widget.weakerEvents (extractEventMap replExpr buttonExtractKeys)
              <&> Responsive.fromWithTextPos
            , ExprGuiM.makeSubexpression replExpr
            ]
            <&> Lens.mapped %~ IOTrans.liftTrans
            <&> Widget.weakerEvents (replEventMap theConfig exportRepl replExpr)
            & GuiState.assignCursor WidgetIds.replId exprId
    where
        exprId = replExpr ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId
