{-# LANGUAGE OverloadedStrings, Rank2Types, RecordWildCards #-}
module Main
    ( main
    ) where

import           Control.Applicative ((<$>), (<*))
import           Control.Applicative (Applicative(..))
import           Control.Concurrent (threadDelay, forkIO, ThreadId)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join, unless, forever, replicateM_)
import           Control.Monad.Trans.State (execStateT)
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
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Graphics.UI.Bottle.WidgetsEnvT (runWidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
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
import           Lamdu.Eval.Results (EvalResults(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import           Paths_lamdu_ide (getDataFileName)
import qualified System.Directory as Directory
import           System.Environment (getArgs)
import           System.FilePath ((</>))

data ParsedOpts = ParsedOpts
    { _poShouldDeleteDB :: Bool
    , _poUndoCount :: Int
    , _poMFontPath :: Maybe FilePath
    }
poShouldDeleteDB :: Lens' ParsedOpts Bool
poShouldDeleteDB f ParsedOpts{..} = f _poShouldDeleteDB <&> \_poShouldDeleteDB -> ParsedOpts{..}
poMFontPath :: Lens' ParsedOpts (Maybe FilePath)
poMFontPath f ParsedOpts{..} = f _poMFontPath <&> \_poMFontPath -> ParsedOpts{..}
poUndoCount :: Lens' ParsedOpts Int
poUndoCount f ParsedOpts{..} = f _poUndoCount <&> \_poUndoCount -> ParsedOpts{..}

parseArgs :: [String] -> Either String ParsedOpts
parseArgs =
    (`execStateT` ParsedOpts False 0 Nothing) . go
    where
        go [] = return ()
        go ("-deletedb" : args) = poShouldDeleteDB .= True >> go args
        go ["-font"] = failUsage "-font must be followed by a font name"
        go ("-font" : fn : args) = poMFontPath %= setPath >> go args
            where
                setPath Nothing = Just fn
                setPath Just {} = failUsage "Duplicate -font arguments"
        go ["-undo"] = failUsage "-undo must be followed by an undo count"
        go ("-undo" : countStr : args) =
            case reads countStr of
                [(count, "")] -> poUndoCount += count >> go args
                _ -> failUsage $ "Invalid undo count: " ++ countStr
        go (arg : _) = failUsage $ "Unexpected arg: " ++ show arg
        failUsage msg = fail $ unlines [ msg, usage ]
        usage = "Usage: lamdu [-deletedb] [-font <filename>] [-undo <N>]"

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
main = do
    setNumCapabilities =<< getNumProcessors
    args <- getArgs
    home <- Directory.getHomeDirectory
    let lamduDir = home </> ".lamdu"
    ParsedOpts{..} <- either fail return $ parseArgs args
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
loadConfig configPath = do
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
sampler sample = do
    ref <- newMVar . (,) 0 =<< E.evaluate =<< sample
    let updateMVar new =
            modifyMVar_ ref $ \(ver, old) -> return $
            if old == new
            then (ver, old)
            else (ver+1, new)
    tid <-
        forkIO . forever $ do
            threadDelay 200000
            (updateMVar =<< sample) `E.catch` \E.SomeException {} -> return ()
    return (tid, readMVar ref)

runEditor :: FilePath -> Maybe FilePath -> IO ()
runEditor lamduDir mFontPath = do
    -- GLFW changes the directory from start directory, at least on macs.
    startDir <- Directory.getCurrentDirectory

    -- Load config as early as possible, before we open any windows/etc
    (_, getConfig) <- sampler $ accessDataFile startDir loadConfig "config.json"

    GLFWUtils.withGLFW $ do
        Vector2 displayWidth displayHeight <- GLFWUtils.getVideoModeSize
        win <- GLFWUtils.createWindow displayWidth displayHeight "Lamdu"
        -- Fonts must be loaded after the GL context is created..
        let getFont path = do
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
mainLoopDebugMode win shouldRefresh getConfig iteration = do
    debugModeRef <- newIORef False
    lastVersionNumRef <- newIORef 0
    let getAnimHalfLife = do
            isDebugMode <- readIORef debugModeRef
            if isDebugMode
                then return $ AnimConfig 6.64 0.01
                else do
                    (_, config) <- getConfig
                    return $
                        AnimConfig
                        (realToFrac (Config.animationTimePeriodSec config))
                        (realToFrac (Config.animationRemainInPeriod config))
        addDebugMode config widget = do
            isDebugMode <- readIORef debugModeRef
            let doc = EventMap.Doc $ "Debug Mode" : if isDebugMode then ["Disable"] else ["Enable"]
                set = writeIORef debugModeRef (not isDebugMode)
            return $
                Widget.strongerEvents
                (Widget.keysEventMap (Config.debugModeKeys config) doc set)
                widget
        makeDebugModeWidget size = do
            (_, config) <- getConfig
            let (makeWidget, addHelp) = iteration config size
            addHelp =<< addDebugMode config =<< makeWidget
        tickHandler = do
            (curVersionNum, _) <- getConfig
            configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                (curVersionNum, lastVersionNum /= curVersionNum)
            if configChanged
                then return True
                else shouldRefresh
    mainLoopWidget win tickHandler makeDebugModeWidget getAnimHalfLife

cacheMakeWidget :: Eq a => (a -> IO (Widget IO)) -> IO (IO (), a -> IO (Widget IO))
cacheMakeWidget mkWidget = do
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
makeFlyNav = do
    flyNavState <- newIORef FlyNav.initState
    return $ \widget -> do
        fnState <- readIORef flyNavState
        return $
            FlyNav.make flyNavConfig WidgetIds.flyNav fnState (writeIORef flyNavState) widget

getDisplayScale :: GLFW.Window -> IO Widget.R
getDisplayScale window =
    do
        (fbWidth, _) <- GLFW.getFramebufferSize window
        (winWidth, _) <- GLFW.getWindowSize window
        return $ fromIntegral fbWidth / fromIntegral winWidth

makeScaleFactor ::
    GLFW.Window -> IO (IORef (Vector2 Widget.R), Config.Zoom -> Widget.EventHandlers IO)
makeScaleFactor window =
    do
        factor <- newIORef . realToFrac =<< getDisplayScale window
        let eventMap Config.Zoom{..} = mconcat
                [ Widget.keysEventMap enlargeKeys
                    (EventMap.Doc ["View", "Zoom", "Enlarge"]) $
                    modifyIORef factor (* realToFrac enlargeFactor)
                , Widget.keysEventMap shrinkKeys
                    (EventMap.Doc ["View", "Zoom", "Shrink"]) $
                    modifyIORef factor (/ realToFrac shrinkFactor)
                ]
        return (factor, eventMap)

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
runDb win getConfig font db = do
    (sizeFactorRef, sizeFactorEvents) <- makeScaleFactor win
    addHelpWithStyle <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
    settingsRef <- newIORef Settings
        { _sInfoMode = Settings.defaultInfoMode
        }
    wrapFlyNav <- makeFlyNav
    evaluatorsRef <- newIORef []
    invalidateCacheRef <- newIORef (return ())
    let invalidateCache = join (readIORef invalidateCacheRef)
    let makeWidget (config, size) = do
            cursor <-
                DbLayout.cursor DbLayout.revisionProps
                & Transaction.getP
                & DbLayout.runDbTransaction db
            sizeFactor <- readIORef sizeFactorRef
            globalEventMap <- mkGlobalEventMap config settingsRef
            let eventMap = globalEventMap `mappend` sizeFactorEvents (Config.zoom config)
            evalResults <-
                    readIORef evaluatorsRef
                    >>= mapM (EvalBG.getResults . snd)
                    <&> mconcat
            widget <-
                mkWidgetWithFallback evalResults config settingsRef
                (baseStyle config font)
                (runTransactionReevaluate db evaluatorsRef invalidateCache)
                (size / sizeFactor, cursor)
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

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x
    | x == maxBound = minBound
    | otherwise = succ x

nextInfoMode :: Settings.InfoMode -> Settings.InfoMode
nextInfoMode = cyclicSucc

mkGlobalEventMap :: Config -> IORef Settings -> IO (Widget.EventHandlers IO)
mkGlobalEventMap config settingsRef = do
    settings <- readIORef settingsRef
    let curInfoMode = settings ^. Settings.sInfoMode
        next = nextInfoMode curInfoMode
        nextDoc = EventMap.Doc ["View", "Subtext", "Show " ++ show next]
    return .
        Widget.keysEventMap (Config.nextInfoModeKeys config) nextDoc .
        modifyIORef settingsRef $ Settings.sInfoMode .~ next

mkWidgetWithFallback ::
    EvalResults (ExprIRef.ValI DbLayout.ViewM) ->
    Config -> IORef Settings -> TextEdit.Style ->
    (forall a. Transaction DbLayout.DbM a -> IO a) ->
    (Widget.Size, Widget.Id) ->
    IO (Widget IO)
mkWidgetWithFallback evalMap config settingsRef style dbToIO (size, cursor) = do
    settings <- readIORef settingsRef
    (isValid, widget) <-
        dbToIO $ do
            candidateWidget <- fromCursor settings cursor
            (isValid, widget) <-
                if candidateWidget ^. Widget.isFocused
                then return (True, candidateWidget)
                else do
                    finalWidget <- fromCursor settings rootCursor
                    Transaction.setP (DbLayout.cursor DbLayout.revisionProps) rootCursor
                    return (False, finalWidget)
            unless (widget ^. Widget.isFocused) $
                fail "Root cursor did not match"
            return (isValid, widget)
    unless isValid $ putStrLn $ "Invalid cursor: " ++ show cursor
    widget
        & Widget.backgroundColor (Config.layerMax (Config.layers config))
            ["background"] (bgColor isValid config)
        & return
    where
        bgColor False = Config.invalidCursorBGColor
        bgColor True = Config.backgroundColor
        fromCursor settings = makeRootWidget evalMap config settings style dbToIO size
        rootCursor = WidgetIds.fromGuid rootGuid

rootGuid :: Guid
rootGuid = IRef.guid $ DbLayout.panes DbLayout.codeIRefs

makeRootWidget ::
    EvalResults (ExprIRef.ValI DbLayout.ViewM) ->
    Config -> Settings -> TextEdit.Style ->
    (forall a. Transaction DbLayout.DbM a -> IO a) ->
    Widget.Size -> Widget.Id ->
    Transaction DbLayout.DbM (Widget IO)
makeRootWidget evalMap config settings style dbToIO fullSize cursor = do
    actions <- VersionControl.makeActions
    let widgetEnv = WE.Env
            { _envCursor = cursor
            , _envTextStyle = style
            , backgroundCursorId = WidgetIds.backgroundCursorId
            , cursorBGColor = Config.cursorBGColor config
            , layerCursor = Config.layerCursor $ Config.layers config
            , layerInterval = Config.layerInterval $ Config.layers config
            , verticalSpacing = Config.verticalSpacing config
            , stdSpaceWidth = Config.spaceWidth config
            }
    runWidgetEnvT widgetEnv $ do
        branchGui <-
            VersionControlGUI.make (Config.versionControl config)
            (Config.layerChoiceBG (Config.layers config))
            id actions $
            \branchSelector ->
                do
                    let hoverPadding = Spacer.makeWidget (Vector2 0 (Config.paneHoverPadding (Config.pane config)))
                    let nonCodeHeight =
                                hoverPadding   ^. Widget.height +
                                branchSelector ^. Widget.height
                    let codeSize = fullSize - Vector2 0 nonCodeHeight
                    codeEdit <-
                        CodeEdit.make (env codeSize) rootGuid
                        & WE.mapWidgetEnvT VersionControl.runAction
                        <&> Widget.events %~ VersionControl.runEvent cursor
                        <&> Widget.padToSizeAlign codeSize 0
                    Box.vbox [(0.5, hoverPadding), (0.5, codeEdit), (0.5, branchSelector)]
                        & return
        let quitEventMap =
                Widget.keysEventMap (Config.quitKeys config) (EventMap.Doc ["Quit"]) (error "Quit")
        branchGui
            & Widget.strongerEvents quitEventMap
            & Widget.events %~ dbToIO . (attachCursor =<<)
            & return
    where
        env size = CodeEdit.Env
            { CodeEdit.codeProps = DbLayout.codeProps
            , CodeEdit.evalMap = evalMap
            , CodeEdit.totalSize = size
            , CodeEdit.config = config
            , CodeEdit.settings = settings
            }
        attachCursor eventResult = do
            maybe (return ()) (Transaction.setP (DbLayout.cursor DbLayout.revisionProps)) .
                Monoid.getLast $ eventResult ^. Widget.eCursor
            return eventResult
