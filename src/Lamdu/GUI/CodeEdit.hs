{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveFunctor, TemplateHaskell, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.GUI.CodeEdit
    ( make
    , Env(..), codeProps, exportActions, evalResults, eConfig, theme, settings, style
    , ExportActions(..)
    , M(..), m, mLiftTrans
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Main as Main
import           Graphics.UI.Bottle.MetaKey (MetaKey)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Align as Align
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout)
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Lamdu.Calc.Type.Scheme as Scheme
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (DefI, ValI)
import           Lamdu.GUI.CodeEdit.Settings (Settings)
import qualified Lamdu.GUI.DefinitionEdit as DefinitionEdit
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.RedundantAnnotations as RedundantAnnotations
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Style (Style)
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.Names.Add as AddNames
import           Lamdu.Sugar.Names.Types (Name)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

newtype M m a = M { _m :: IO (T m (Main.EventResult a)) }
    deriving (Functor)
Lens.makeLenses ''M

instance Monad m => Applicative (M m) where
    pure = M . pure . pure . pure
    M f <*> M x = (liftA2 . liftA2 . liftA2) ($) f x & M

mLiftTrans :: Functor m => T m a -> M m a
mLiftTrans = M . pure . fmap pure

data ExportActions m = ExportActions
    { exportRepl :: M m ()
    , exportAll :: M m ()
    , -- Fancy export is intended for sending code to someone who doesn't have
      -- Lamdu installed. It bundles together in a zipfile a screenshot,
      -- a README, the repl export, and compiled JS  (that requires a new
      -- version of nodejs supporting TCO).  It is intended to enable Lamdu
      -- to be used in competitions such as Google codejam which require
      -- [readable] code upload.
      exportFancy :: M m ()
    , exportDef :: DefI m -> M m ()
    , importAll :: FilePath -> M m ()
    }

data Env m = Env
    { _codeProps :: Anchors.CodeProps m
    , _exportActions :: ExportActions m
    , _evalResults :: CurAndPrev (EvalResults (ValI m))
    , _eConfig :: Config
    , _theme :: Theme
    , _settings :: Settings
    , _style :: Style
    }
Lens.makeLenses ''Env

instance Config.HasConfig (Env m) where config = eConfig
instance Theme.HasTheme (Env m) where theme = theme

toExprGuiMPayload :: ([Sugar.EntityId], NearestHoles) -> ExprGuiT.Payload
toExprGuiMPayload (entityIds, nearestHoles) =
    ExprGuiT.emptyPayload nearestHoles & ExprGuiT.plStoredEntityIds .~ entityIds

traverseAddNearestHoles ::
    Traversable t =>
    t (Sugar.Expression name m a) ->
    t (Sugar.Expression name m (a, NearestHoles))
traverseAddNearestHoles binder =
    binder
    <&> Lens.mapped %~ (,)
    & NearestHoles.add traverse

exprAddNearestHoles ::
    Sugar.Expression name m a ->
    Sugar.Expression name m (a, NearestHoles)
exprAddNearestHoles expr =
    Identity expr
    & traverseAddNearestHoles
    & runIdentity

postProcessExpr ::
    Sugar.Expression name m ([Sugar.EntityId], NearestHoles) ->
    Sugar.Expression name m ExprGuiT.Payload
postProcessExpr expr =
    expr
    <&> toExprGuiMPayload
    & RedundantAnnotations.markAnnotationsToDisplay

loadWorkArea :: Monad m => Env m -> T m (Sugar.WorkArea (Name m) m ExprGuiT.Payload)
loadWorkArea env =
    do
        Sugar.WorkArea { _waPanes, _waRepl } <-
            SugarConvert.loadWorkArea (env ^. evalResults) (env ^. codeProps)
            >>= AddNames.addToWorkArea
        Sugar.WorkArea
            (_waPanes <&> Sugar.paneDefinition %~ fmap postProcessExpr . traverseAddNearestHoles)
            (_waRepl & exprAddNearestHoles & postProcessExpr)
            & pure

makeReplEdit ::
    Monad m =>
    Env m -> ExprGuiT.SugarExpr m ->
    ExprGuiM m (TreeLayout (M m Widget.EventResult))
makeReplEdit env replExpr =
    ExpressionGui.combineSpaced
    <*> sequence
    [ (Widget.makeFocusableView ?? Widget.joinId WidgetIds.replId ["symbol"])
      <*> (TextView.makeLabel "⋙" <&> TreeLayout.fromTextView)
    , ExprGuiM.makeSubexpressionWith 0 id replExpr
    ]
    <&> Lens.mapped %~ mLiftTrans
    <&> E.weakerEvents (replEventMap env replExpr)
    & Widget.assignCursor WidgetIds.replId exprId
    where
        exprId = replExpr ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId

make ::
    ( Monad m, MonadTransaction m n, MonadReader env n, Config.HasConfig env
    , Theme.HasTheme env, Widget.HasCursor env, TextEdit.HasStyle env
    , Spacer.HasStdSpacing env
    ) =>
    Widget.R ->
    Env m -> n (Widget (M m Widget.EventResult))
make width env =
    do
        workArea <- loadWorkArea env & transaction
        replGui <- makeReplEdit env (workArea ^. Sugar.waRepl)
        panesEdits <- workArea ^. Sugar.waPanes & traverse (makePaneEdit env)
        newDefinitionButton <- makeNewDefinitionButton <&> fmap mLiftTrans <&> TreeLayout.fromWidget
        eventMap <- panesEventMap env
        TreeLayout.vboxSpaced
            ?? (replGui : panesEdits ++ [newDefinitionButton])
            <&> E.weakerEvents eventMap
    & ExprGuiM.run ExpressionEdit.make
      (env ^. codeProps) (env ^. settings) (env ^. style)
    <&> ExpressionGui.render width
    <&> (^. Align.tValue)

makePaneEdit ::
    Monad m =>
    Env m -> Sugar.Pane (Name m) m ExprGuiT.Payload ->
    ExprGuiM m (TreeLayout (M m Widget.EventResult))
makePaneEdit env pane =
    DefinitionEdit.make (pane ^. Sugar.paneDefinition)
    <&> Lens.mapped %~ mLiftTrans
    <&> E.weakerEvents paneEventMap
    where
        paneCloseKeys = Config.paneCloseKeys (Config.pane (env ^. eConfig))
        exportKeys = Config.exportKeys (Config.export (env ^. eConfig))
        paneEventMap =
            [ pane ^. Sugar.paneClose & mLiftTrans
              <&> WidgetIds.fromEntityId
              & Widget.keysEventMapMovesCursor paneCloseKeys
                (E.Doc ["View", "Pane", "Close"])
            , do
                  Transaction.setP (pane ^. Sugar.paneDefinition . Sugar.drDefinitionState)
                      Sugar.DeletedDefinition
                  pane ^. Sugar.paneClose
              & mLiftTrans
              <&> WidgetIds.fromEntityId
              & Widget.keysEventMapMovesCursor (Config.delKeys env)
                (E.Doc ["Edit", "Definition", "Delete"])
            , exportDef (env ^. exportActions) (pane ^. Sugar.paneDefinition . Sugar.drDefI)
              & Widget.keysEventMap exportKeys
                (E.Doc ["Collaboration", "Export definition to JSON file"])
            ] & mconcat

makeNewDefinitionEventMap ::
    (Monad m, MonadReader env n, Widget.HasCursor env) =>
    Anchors.CodeProps m ->
    n ([MetaKey] -> Widget.EventMap (T m Widget.EventResult))
makeNewDefinitionEventMap cp =
    do
        curCursor <- Lens.view Widget.cursor
        let newDefinition =
                do
                    holeI <- DataOps.newHole
                    newDefI <-
                        Definition
                        (Definition.BodyExpr (Definition.Expr holeI mempty))
                        Scheme.any ()
                        & DataOps.newPublicDefinitionWithPane "" cp
                    DataOps.savePreJumpPosition cp curCursor
                    return newDefI
                <&> WidgetIds.nameEditOf . WidgetIds.fromIRef
        return $ \newDefinitionKeys ->
            Widget.keysEventMapMovesCursor newDefinitionKeys
            (E.Doc ["Edit", "New definition"]) newDefinition

makeNewDefinitionButton :: Monad m => ExprGuiM m (Widget (T m Widget.EventResult))
makeNewDefinitionButton =
    do
        codeAnchors <- ExprGuiM.readCodeAnchors
        newDefinitionEventMap <- makeNewDefinitionEventMap codeAnchors

        Config.Pane{newDefinitionButtonPressKeys} <- Lens.view Config.config <&> Config.pane
        color <- Lens.view Theme.theme <&> Theme.newDefinitionActionColor

        TextView.makeFocusableLabel "New..."
            & Reader.local (TextView.color .~ color)
            <&> (^. Align.tValue)
            <&> E.weakerEvents (newDefinitionEventMap newDefinitionButtonPressKeys)

replEventMap ::
    Monad m =>
    Env m -> Sugar.Expression name m a -> Widget.EventMap (M m Widget.EventResult)
replEventMap env replExpr =
    mconcat
    [ replExpr ^. Sugar.rPayload . Sugar.plActions . Sugar.extract
      <&> ExprEventMap.extractCursor & mLiftTrans
      & Widget.keysEventMapMovesCursor
        (Config.newDefinitionButtonPressKeys (Config.pane (env ^. eConfig)))
        (E.Doc ["Edit", "Extract to definition"])
    , Widget.keysEventMap exportKeys
      (E.Doc ["Collaboration", "Export repl to JSON file"]) exportRepl
    , Widget.keysEventMap exportFancyKeys
      (E.Doc ["Collaboration", "Export repl for Codejam"]) exportFancy
    ]
    where
        ExportActions{exportRepl, exportFancy} = env ^. exportActions
        Config.Export{exportKeys, exportFancyKeys} = Config.export (env ^. eConfig)

panesEventMap ::
    (Monad m, MonadTransaction m n, MonadReader env n, Widget.HasCursor env) =>
    Env m -> n (Widget.EventMap (M m Widget.EventResult))
panesEventMap env =
    do
        mJumpBack <- DataOps.jumpBack (env ^. codeProps) & transaction <&> fmap mLiftTrans
        newDefinitionEventMap <- makeNewDefinitionEventMap (env ^. codeProps)
        return $ mconcat
            [ newDefinitionEventMap (Config.newDefinitionKeys (Config.pane (env ^. eConfig)))
              <&> mLiftTrans
            , E.dropEventMap "Drag&drop JSON files"
              (E.Doc ["Collaboration", "Import JSON file"]) (Just . traverse_ importAll)
              <&> fmap (\() -> mempty)
            , maybe mempty
              (Widget.keysEventMapMovesCursor (Config.previousCursorKeys (env ^. eConfig))
               (E.Doc ["Navigation", "Go back"])) mJumpBack
            , Widget.keysEventMap exportAllKeys
              (E.Doc ["Collaboration", "Export everything to JSON file"]) exportAll
            , importAll exportPath
              & Widget.keysEventMap importKeys
                (E.Doc ["Collaboration", "Import repl from JSON file"])
            ]
    where
        ExportActions{importAll,exportAll} = env ^. exportActions
        Config.Export{exportPath,importKeys,exportAllKeys} = Config.export (env ^. eConfig)
