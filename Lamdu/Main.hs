{-# LANGUAGE OverloadedStrings, Rank2Types, RecordWildCards #-}
module Main
    ( main
    ) where

import           Control.Applicative ((<$>), (<*))
import           Control.Applicative (Applicative(..))
import           Control.Concurrent (threadDelay, forkIO, ThreadId)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join, unless, forever, replicateM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Maybe
import           Data.Monoid (Monoid(..))
import qualified Data.Monoid as Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Db (Db)
import qualified Data.Store.Db as Db
import           Data.Store.Guid (Guid)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Change as Change
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.MainLoop (mainLoopWidget, AnimConfig (..))
import           Graphics.UI.Bottle.SizedFont (SizedFont(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import qualified Lamdu.Builtins as Builtins
import qualified Lamdu.Builtins.Anchors as BuiltinAnchors
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.ExampleDB as ExampleDB
import qualified Lamdu.Eval.Background as EvalBG
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Val as V
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.Zoom as Zoom
import qualified Lamdu.Opts as Opts
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import           Paths_lamdu_ide (getDataFileName)
import qualified System.Directory as Directory
import           System.Environment (getArgs)
import           System.FilePath ((</>))

undo :: Transaction DbLayout.DbM Widget.Id
undo =
    do
        actions <- VersionControl.makeActions
        fromMaybe (fail "Cannot undo any further") $ mUndo actions

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb lamduDir body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        Db.withDb (lamduDir </> "codeedit.db") $ \db ->
            do
                ExampleDB.initDB db
                body db

main :: IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        args <- getArgs
        home <- Directory.getHomeDirectory
        let lamduDir = home </> ".lamdu"
        Opts.Parsed{..} <- either fail return $ Opts.parse args
        if _poShouldDeleteDB
            then do
                putStrLn "Deleting DB..."
                Directory.removeDirectoryRecursive lamduDir
            else
                if _poUndoCount > 0
                then do
                    putStrLn $ "Undoing " ++ show _poUndoCount ++ " times"
                    withDb lamduDir $ \db ->
                        DbLayout.runDbTransaction db $ replicateM_ _poUndoCount undo
                else runEditor lamduDir _poMFontPath

loadConfig :: FilePath -> IO Config
loadConfig configPath =
    do
        eConfig <- Aeson.eitherDecode' <$> LBS.readFile configPath
        either (fail . (msg ++)) return eConfig
    where
        msg = "Failed to parse config file contents at " ++ show configPath ++ ": "

accessDataFile :: FilePath -> (FilePath -> IO a) -> FilePath -> IO a
accessDataFile startDir accessor fileName =
    (accessor =<< getDataFileName fileName)
    `E.catch` \(E.SomeException _) ->
    accessor $ startDir </> fileName

type Version = Int

sampler :: Eq a => IO a -> IO (ThreadId, IO (Version, a))
sampler sample =
    do
        ref <- newMVar . (,) 0 =<< E.evaluate =<< sample
        let updateMVar new =
                modifyMVar_ ref $ \(ver, old) -> return $
                if old == new
                then (ver, old)
                else (ver+1, new)
        tid <-
            forkIO . forever $
            do
                threadDelay 200000
                (updateMVar =<< sample) `E.catch` \E.SomeException {} -> return ()
        return (tid, readMVar ref)

runEditor :: FilePath -> Maybe FilePath -> IO ()
runEditor lamduDir mFontPath =
    do
        -- GLFW changes the directory from start directory, at least on macs.
        startDir <- Directory.getCurrentDirectory

        -- Load config as early as possible, before we open any windows/etc
        (_, getConfig) <- sampler $ accessDataFile startDir loadConfig "config.json"

        GLFWUtils.withGLFW $
            do
                Vector2 displayWidth displayHeight <- GLFWUtils.getVideoModeSize
                win <- GLFWUtils.createWindow displayWidth displayHeight "Lamdu"
                -- Fonts must be loaded after the GL context is created..
                let getFont path =
                        do
                            exists <- Directory.doesFileExist path
                            unless exists . ioError . userError $ path ++ " does not exist!"
                            Draw.openFont path
                font <-
                    case mFontPath of
                    Nothing -> accessDataFile startDir getFont "fonts/DejaVuSans.ttf"
                    Just path -> getFont path
                withDb lamduDir $ runDb win getConfig font


mainLoopDebugMode ::
    GLFW.Window ->
    IO Bool ->
    IO (Version, Config) ->
    ( Config -> Widget.Size ->
        ( IO (Widget IO)
        , Widget IO -> IO (Widget IO)
        )
    ) -> IO ()
mainLoopDebugMode win shouldRefresh getConfig iteration =
    do
        debugModeRef <- newIORef False
        lastVersionNumRef <- newIORef 0
        let getAnimHalfLife =
                do
                    isDebugMode <- readIORef debugModeRef
                    if isDebugMode
                        then return $ AnimConfig 6.64 0.01
                        else do
                            (_, config) <- getConfig
                            return $
                                AnimConfig
                                (realToFrac (Config.animationTimePeriodSec config))
                                (realToFrac (Config.animationRemainInPeriod config))
            addDebugMode config widget =
                do
                    isDebugMode <- readIORef debugModeRef
                    let doc = EventMap.Doc $ "Debug Mode" : if isDebugMode then ["Disable"] else ["Enable"]
                        set = writeIORef debugModeRef (not isDebugMode)
                    return $
                        Widget.strongerEvents
                        (Widget.keysEventMap (Config.debugModeKeys config) doc set)
                        widget
            makeDebugModeWidget size =
                do
                    (_, config) <- getConfig
                    let (makeWidget, addHelp) = iteration config size
                    addHelp =<< addDebugMode config =<< makeWidget
            tickHandler =
                do
                    (curVersionNum, _) <- getConfig
                    configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                        (curVersionNum, lastVersionNum /= curVersionNum)
                    if configChanged
                        then return True
                        else shouldRefresh
        mainLoopWidget win tickHandler makeDebugModeWidget getAnimHalfLife

cacheMakeWidget :: Eq a => (a -> IO (Widget IO)) -> IO (IO (), a -> IO (Widget IO))
cacheMakeWidget mkWidget =
    do
        widgetCacheRef <- newIORef =<< memoIO mkWidget
        let invalidateCache = writeIORef widgetCacheRef =<< memoIO mkWidget
        return
            ( invalidateCache
            , \x ->
                readIORef widgetCacheRef
                >>= ($ x)
                <&> Widget.events %~ (<* invalidateCache)
            )

flyNavConfig :: FlyNav.Config
flyNavConfig = FlyNav.Config
    { FlyNav.configLayer = -10000 -- that should cover it :-)
    }

makeFlyNav :: IO (Widget IO -> IO (Widget IO))
makeFlyNav =
    do
        flyNavState <- newIORef FlyNav.initState
        return $ \widget ->
            do
                fnState <- readIORef flyNavState
                return $
                    FlyNav.make flyNavConfig WidgetIds.flyNav fnState (writeIORef flyNavState) widget

helpConfig :: Draw.Font -> Config.Help -> EventMapDoc.Config
helpConfig font Config.Help{..} =
    EventMapDoc.Config
    { EventMapDoc.configStyle =
        TextView.Style
        { TextView._styleColor = helpTextColor
        , TextView._styleFont = SizedFont font helpTextSize
        }
    , EventMapDoc.configInputDocColor = helpInputDocColor
    , EventMapDoc.configBGColor = helpBGColor
    , EventMapDoc.configOverlayDocKeys = helpKeys
    }

baseStyle :: Config -> Draw.Font -> TextEdit.Style
baseStyle config font = TextEdit.Style
    { TextEdit._sTextViewStyle =
        TextView.Style
        { TextView._styleColor = Config.baseColor config
        , TextView._styleFont = SizedFont font (Config.baseTextSize config)
        }
    , TextEdit._sCursorColor = TextEdit.defaultCursorColor
    , TextEdit._sCursorWidth = TextEdit.defaultCursorWidth
    , TextEdit._sTextCursorId = WidgetIds.textCursorId
    , TextEdit._sBGColor = Config.cursorBGColor config
    , TextEdit._sEmptyUnfocusedString = ""
    , TextEdit._sEmptyFocusedString = ""
    }

runViewTransactionInIO :: Db -> Transaction DbLayout.ViewM a -> IO a
runViewTransactionInIO db =
        DbLayout.runDbTransaction db .
        VersionControl.runAction

type ValIRef = ExprIRef.ValI DbLayout.ViewM

evalActions :: IO () -> Db -> EvalBG.Actions ValIRef
evalActions invalidateCache db = EvalBG.Actions
    { _aLoadGlobal = runViewTransactionInIO db . loadDef
    , _aRunBuiltin = Builtins.eval
    , _aReportUpdatesAvailable = invalidateCache
    }
    where
        loadDef globalId =
            Load.loadDef (ExprIRef.defI globalId)
            <&> asDef globalId
        asDef globalId x =
            x ^. Def.defBody
            <&> Lens.mapped %~ Property.value
            <&> replaceRecursiveReferences globalId
            & Just
        replaceRecursiveReferences globalId (V.Val pl (V.BLeaf (V.LVar v)))
            | v == BuiltinAnchors.recurseVar = V.Val pl (V.BLeaf (V.LGlobal globalId))
        replaceRecursiveReferences globalId val =
            val & V.body . Lens.traversed %~ replaceRecursiveReferences globalId

startEval :: IO () -> Db -> IO [(ExprIRef.DefI DbLayout.ViewM, EvalBG.Evaluator ValIRef)]
startEval invalidateCache db =
    DbLayout.panes DbLayout.codeIRefs
    & Transaction.readIRef

    >>= mapM loadDef
    & runViewTransactionInIO db

    <&> mapMaybe (_2 %%~ (^? Def.defBody . Def._BodyExpr . Def.expr))
    <&> Lens.mapped . _2 . Lens.mapped %~ Property.value

    >>= Lens.traverse . _2 %%~ EvalBG.start (evalActions invalidateCache db)
    where
        loadDef defI = Load.loadDef defI <&> (,) defI

sumDependency ::
    ExprIRef.DefI m -> (Set (ExprIRef.ValI m), Set V.GlobalId) -> Set Guid
sumDependency defI (subexprs, globals) =
    mconcat
    [ Set.singleton (IRef.guid defI)
    , Set.map (IRef.guid . ExprIRef.unValI) subexprs
    , Set.map (IRef.guid . ExprIRef.defI) globals
    ]

startEvaluators ::
    Db -> IORef [(ExprIRef.DefI DbLayout.ViewM, EvalBG.Evaluator ValIRef)] ->
    IO () -> IO ()
startEvaluators db evaluatorsRef invalidateCache =
    startEval invalidateCache db >>= writeIORef evaluatorsRef

rootGuid :: Guid
rootGuid = IRef.guid $ DbLayout.panes DbLayout.codeIRefs

runTransactionReevaluate ::
    Db ->
    IORef [(ExprIRef.DefI DbLayout.ViewM, EvalBG.Evaluator ValIRef)] ->
    IO () ->
    Transaction DbLayout.DbM a ->
    IO a
runTransactionReevaluate db evaluatorsRef invalidateCache transaction =
    do
        defEvaluators <- readIORef evaluatorsRef
        dependencies <-
            defEvaluators
            & Lens.traverse . _2 %%~ EvalBG.pauseLoading
            <&> Lens.mapped %~ uncurry sumDependency
            <&> mconcat
            <&> Set.insert rootGuid
        (dependencyChanged, result) <-
            do
                (oldVersion, result, newVersion) <-
                    (,,)
                    <$> VersionControl.getVersion
                    <*> transaction
                    <*> VersionControl.getVersion
                let checkDependencyChange versionData =
                            Version.changes versionData
                            <&> Change.objectKey
                            <&> (`Set.member` dependencies)
                            <&> Monoid.Any
                            & mconcat
                            & return
                Monoid.Any dependencyChanged <-
                    Version.walk checkDependencyChange checkDependencyChange oldVersion newVersion
                return (dependencyChanged, result)
            & DbLayout.runDbTransaction db
        let evaluators = map snd defEvaluators
        if dependencyChanged
            then do
                mapM_ EvalBG.stop evaluators
                startEvaluators db evaluatorsRef invalidateCache
            else mapM_ EvalBG.resumeLoading evaluators
        return result

runDb :: GLFW.Window -> IO (Version, Config) -> Draw.Font -> Db -> IO ()
runDb win getConfig font db =
    do
        zoom <- Zoom.make =<< GLFWUtils.getDisplayScale win
        addHelpWithStyle <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
        settingsRef <- newIORef Settings
            { _sInfoMode = Settings.defaultInfoMode
            }
        wrapFlyNav <- makeFlyNav
        evaluatorsRef <- newIORef []
        invalidateCacheRef <- newIORef (return ())
        let invalidateCache = join (readIORef invalidateCacheRef)
        let makeWidget (config, size) =
                do
                    cursor <-
                        DbLayout.cursor DbLayout.revisionProps
                        & Transaction.getP
                        & DbLayout.runDbTransaction db
                    sizeFactor <- Zoom.getSizeFactor zoom
                    globalEventMap <- mkGlobalEventMap config settingsRef
                    let eventMap = globalEventMap `mappend` Zoom.eventMap zoom (Config.zoom config)
                    evalResults <-
                        readIORef evaluatorsRef
                        >>= mapM (EvalBG.getResults . snd)
                        <&> mconcat
                    settings <- readIORef settingsRef
                    let env = GUIMain.Env
                            { envEvalMap = evalResults
                            , envConfig = config
                            , envSettings = settings
                            , envStyle = baseStyle config font
                            , envFullSize = size / sizeFactor
                            , envCursor = cursor
                            }
                    let dbToIO = runTransactionReevaluate db evaluatorsRef invalidateCache
                    widget <- mkWidgetWithFallback dbToIO env
                    return . Widget.scale sizeFactor $ Widget.weakerEvents eventMap widget
        (invalidateCacheAction, makeWidgetCached) <- cacheMakeWidget makeWidget
        refreshRef <- newIORef False
        let shouldRefresh = atomicModifyIORef refreshRef $ \r -> (False, r)
        writeIORef invalidateCacheRef $
            do
                invalidateCacheAction
                writeIORef refreshRef True
        startEvaluators db evaluatorsRef invalidateCache

        mainLoopDebugMode win shouldRefresh getConfig $ \config size ->
            ( wrapFlyNav =<< makeWidgetCached (config, size)
            , addHelpWithStyle (helpConfig font (Config.help config)) size
            )

mkGlobalEventMap :: Config -> IORef Settings -> IO (Widget.EventHandlers IO)
mkGlobalEventMap config settingsRef =
    do
        settings <- readIORef settingsRef
        let curInfoMode = settings ^. Settings.sInfoMode
            next = Settings.nextInfoMode curInfoMode
            nextDoc = EventMap.Doc ["View", "Subtext", "Show " ++ show next]
        return .
            Widget.keysEventMap (Config.nextInfoModeKeys config) nextDoc .
            modifyIORef settingsRef $ Settings.sInfoMode .~ next

mkWidgetWithFallback ::
    (forall a. Transaction DbLayout.DbM a -> IO a) ->
    GUIMain.Env -> IO (Widget IO)
mkWidgetWithFallback dbToIO env =
    do
        (isValid, widget) <-
            dbToIO $
            do
                candidateWidget <- makeMainGui dbToIO env
                (isValid, widget) <-
                    if candidateWidget ^. Widget.isFocused
                    then return (True, candidateWidget)
                    else do
                        finalWidget <- makeMainGui dbToIO env { GUIMain.envCursor = rootCursor }
                        Transaction.setP (DbLayout.cursor DbLayout.revisionProps) rootCursor
                        return (False, finalWidget)
                unless (widget ^. Widget.isFocused) $
                    fail "Root cursor did not match"
                return (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (GUIMain.envCursor env)
        widget
            & Widget.backgroundColor
              (Config.layerMax (Config.layers config))
              ["background"] (bgColor isValid config)
            & return
    where
        config = GUIMain.envConfig env
        bgColor False = Config.invalidCursorBGColor
        bgColor True = Config.backgroundColor
        rootCursor = WidgetIds.fromGuid rootGuid

makeMainGui ::
    (forall a. Transaction DbLayout.DbM a -> f a) ->
    GUIMain.Env -> Transaction DbLayout.DbM (Widget f)
makeMainGui runTransaction env =
    GUIMain.make env (WidgetIds.fromGuid rootGuid)
    <&> Widget.events %~ runTransaction . (attachCursor =<<)
    where
        attachCursor eventResult =
            do
                maybe (return ()) (Transaction.setP (DbLayout.cursor DbLayout.revisionProps)) .
                    Monoid.getLast $ eventResult ^. Widget.eCursor
                return eventResult
