-- | REPL Edit
{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings #-}
module Lamdu.GUI.ReplEdit
    ( ExportRepl(..), make
    ) where

import qualified Control.Lens as Lens
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config, config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.IOTrans (IOTrans)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data ExportRepl m = ExportRepl
    { exportRepl :: IOTrans m ()
    , -- Fancy export is intended for sending code to someone who doesn't have
      -- Lamdu installed. It bundles together in a zipfile a screenshot,
      -- a README, the repl export, and compiled JS  (that requires a new
      -- version of nodejs supporting TCO).  It is intended to enable Lamdu
      -- to be used in competitions such as Google codejam which require
      -- [readable] code upload.
      exportFancy :: IOTrans m ()
    }

extractEventMap ::
    Functor m => Sugar.Expression name m a -> [MetaKey] ->
    Widget.EventMap (IOTrans m Widget.EventResult)
extractEventMap replExpr keys =
  replExpr ^. Sugar.rPayload . Sugar.plActions . Sugar.extract
  <&> ExprEventMap.extractCursor & IOTrans.liftTrans
  & Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "Extract to definition"])

replEventMap ::
    Monad m =>
    Config -> ExportRepl m ->
    Sugar.Expression name m a -> Widget.EventMap (IOTrans m Widget.EventResult)
replEventMap theConfig (ExportRepl exportRepl exportFancy) replExpr =
    mconcat
    [ extractEventMap replExpr (Config.extractKeys theConfig)
    , Widget.keysEventMap exportKeys
      (E.Doc ["Collaboration", "Export repl to JSON file"]) exportRepl
    , Widget.keysEventMap exportFancyKeys
      (E.Doc ["Collaboration", "Export repl for Codejam"]) exportFancy
    ]
    where
        Config.Export{exportKeys, exportFancyKeys} = Config.export theConfig

make ::
    Monad m =>
    ExportRepl m ->
    ExprGuiT.SugarExpr m ->
    ExprGuiM m (Responsive (IOTrans m Widget.EventResult))
make exportRepl replExpr =
    do
        theConfig <- Lens.view config
        let buttonExtractKeys = Config.newDefinitionButtonPressKeys (Config.pane theConfig)
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*> sequence
            [ (Widget.makeFocusableView ?? Widget.joinId WidgetIds.replId ["symbol"] <&> (Align.tValue %~))
              <*> TextView.makeLabel "⋙"
              <&> Lens.mapped %~ E.weakerEvents (extractEventMap replExpr buttonExtractKeys)
              <&> Responsive.fromWithTextPos
            , ExprGuiM.makeSubexpressionWith 0 replExpr
              <&> Lens.mapped %~ IOTrans.liftTrans
            ]
            <&> E.weakerEvents (replEventMap theConfig exportRepl replExpr)
            & Widget.assignCursor WidgetIds.replId exprId
    where
        exprId = replExpr ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId
