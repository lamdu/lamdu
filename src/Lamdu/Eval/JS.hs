{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TemplateHaskell, TupleSections, DerivingVia #-}
-- | Run a process that evaluates given compiled
module Lamdu.Eval.JS
    ( module Lamdu.Eval.JS.Types
    , Evaluator
    , Actions(..), aLoadGlobal, aReportUpdatesAvailable
    , start, stop, executeReplIOProcess
    , Dependencies(..), whilePaused
    , getResults

      -- | Export for test purposes:
    , nodeRepl
    ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.Extended (forkIO, killThread, withForkedIO)
import           Control.Concurrent.MVar
import qualified Control.Lens as Lens
import           Control.Monad (msum)
import           Control.Monad.Cont (ContT(..))
import           Control.Monad.Trans.State (State, runState)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Extended as BS
import           Data.Either (fromRight)
import           Data.IORef
import           Data.IntMap (IntMap)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.String (IsString(..))
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import qualified Data.Vector as Vec
import           Data.Word (Word8)
import           Hyper
import           Hyper.Syntax.Row (RowExtend(..))
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Identifier (Identifier(..), identHex)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Tag(..))
import           Lamdu.Data.Anchors (anonTag)
import           Lamdu.Data.Definition (Definition)
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.JS.Compiler as Compiler
import           Lamdu.Eval.JS.Types
import           Lamdu.Eval.Results (ScopeId(..), EvalResults(..))
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.Paths as Paths
import qualified Language.JavaScript.Inline.Core as NodeJS
import           Numeric (readHex)
import           System.Environment (getEnvironment)
import           System.FilePath (splitFileName)
import           System.IO (IOMode(..), Handle, hIsEOF, hPutStrLn, hFlush, withFile)
import           System.IO.Temp (withSystemTempFile)
import qualified System.Process as Proc
import           System.Process.Utils (withProcess)

import           Lamdu.Prelude

data Actions = Actions
    { _aLoadGlobal :: V.Var -> IO (Definition (Annotated UUID # V.Term) ())
    , _aReportUpdatesAvailable :: IO ()
    , _aJSDebugPaths :: JSDebugPaths FilePath
    }
Lens.makeLenses ''Actions

data Dependencies = Dependencies
    { subExprDeps :: Set UUID
    , globalDeps :: Set V.Var
    } deriving stock Generic
    deriving (Semigroup, Monoid) via Generically Dependencies

data Evaluator = Evaluator
    { stop :: IO ()
    , executeReplIOProcess :: IO ()
    , eDeps :: MVar Dependencies
    , eResultsRef :: IORef EvalResults
    }

type Parse = State (IntMap (ER.Val ()))

getNodePath :: IO FilePath
getNodePath =
    -- prefer the relative-path bin/node.exe
    Paths.getDataFileNameMaybe "bin/node.exe"
    <&> fromMaybe (NodeJS.nodePath NodeJS.defaultConfig)

nodeRepl :: IO Proc.CreateProcess
nodeRepl =
    do
        rtsPath <- Paths.getDataFileName "js/rts.js" <&> fst . splitFileName
        nodeExePath <- getNodePath
        env <- getEnvironment
        pure (Proc.proc nodeExePath ["--interactive"])
            { Proc.std_in = Proc.CreatePipe
            , Proc.std_out = Proc.CreatePipe
            , Proc.env = Just (("NODE_PATH", rtsPath):env)
            }

parseHexBs :: Text -> ByteString
parseHexBs =
    BS.pack . map (fst . sHead . readHex . Text.unpack) . Text.chunksOf 2
    where
        sHead [] = error "parseHexBs got bad input"
        sHead (x:_) = x

parseHexNameBs :: Text -> ByteString
parseHexNameBs t =
    case Text.uncons t of
    Just ('_', n) -> parseHexBs n
    _ -> parseHexBs t

parseUUID :: Text -> UUID
parseUUID = UUIDUtils.fromSBS16 . parseHexNameBs

parseRecord :: Json.Object -> Parse (ER.Val ())
parseRecord =
    Lens.ifoldlM step (Ann (Const ()) ER.RRecEmpty)
    where
        step "cacheId" r _ = pure r -- TODO: Explain/fix this
        step k r v =
            parseResult v
            <&> \pv ->
            ER.RRecExtend RowExtend
            { _eKey = parseHexNameBs k & Identifier & Tag
            , _eVal = pv
            , _eRest = r
            } & Ann (Const ())

parseWord8 :: Json.Value -> Word8
parseWord8 (Json.Number x)
    | x == fromIntegral i = i
    where
        i = truncate x
parseWord8 x = "Expected word8, given: " ++ show x & error

parseBytes :: Json.Value -> ER.Val ()
parseBytes (Json.Array vals) =
    Vec.toList vals
    <&> parseWord8
    & BS.pack & PrimVal.Bytes & PrimVal.fromKnown & ER.RPrimVal & Ann (Const ())
parseBytes _ = error "Bytes with non-array data"

parseInject :: Text -> Either e Json.Value -> Parse (ER.Val ())
parseInject tag mData =
    case mData of
    Left{} -> Ann (Const ()) ER.RRecEmpty & pure
    Right v -> parseResult v
    <&> \iv ->
    ER.RInject ER.Inject
    { ER._injectTag = parseHexNameBs tag & Identifier & Tag
    , ER._injectVal = iv
    } & Ann (Const ())

(.:) :: Json.FromJSON a => Json.Object -> Text -> Either String a
obj .: tag = Json.parseEither (Json..: tag) obj

(.:?) :: Json.FromJSON a => Json.Object -> Text -> Either String (Maybe a)
obj .:? tag = Json.parseEither (Json..:? tag) obj

parseObj :: Json.Object -> Parse (ER.Val ())
parseObj obj =
    msum
    [ obj .: "array"
      <&> \(Json.Array arr) ->
            Vec.toList arr & Lens.traversed %%~ parseResult <&> ER.RArray <&> Ann (Const ())
    , obj .: "bytes" <&> parseBytes <&> pure
    , obj .: "number" <&> read <&> fromDouble <&> pure
    , obj .: "tag" <&> (`parseInject` (obj .: "data"))
    , obj .: "func" <&> (\(Json.Number x) -> round x & ER.RFunc & Ann (Const ()) & pure)
    ] & fromRight (parseRecord obj)

parseResult :: Json.Value -> Parse (ER.Val ())
parseResult (Json.Number x) = realToFrac x & fromDouble & pure
parseResult (Json.Object obj) =
    case obj .: "cachedVal" of
    Right cacheId -> Lens.use (Lens.singular (Lens.ix cacheId))
    Left{} ->
        do
            x <- parseObj obj
            case obj .: "cacheId" <|> obj .: "func" of
                Left{} -> pure ()
                Right cacheId -> Lens.at cacheId ?= x
            pure x
parseResult x = "Unsupported encoded JS output: " ++ show x & error

fromDouble :: Double -> ER.Val ()
fromDouble = Ann (Const ()) . ER.RPrimVal . PrimVal.fromKnown . PrimVal.Float

addVal ::
    Json.Object ->
    Parse
    ( Map UUID (Map ScopeId (ER.Val ())) ->
      Map UUID (Map ScopeId (ER.Val ()))
    )
addVal obj =
    case obj .: "result" of
    Left{} -> pure id
    Right result ->
        parseResult result
        <&> \pr ->
        Map.alter
        (<> Just (Map.singleton (ScopeId scope) pr))
        (parseUUID exprId)
    where
        Right scope = obj .: "scope"
        Right exprId = obj .: "exprId"

newScope ::
    Json.Object ->
    Parse
    ( Map UUID (Map ScopeId [(ScopeId, ER.Val ())]) ->
      Map UUID (Map ScopeId [(ScopeId, ER.Val ())])
    )
newScope obj =
    do
        arg <-
            case obj .: "arg" of
            Left{} -> error "Scope report missing arg"
            Right x -> parseResult x
        let apply = Map.singleton (ScopeId parentScope) [(ScopeId scope, arg)]
        let addApply Nothing = Just apply
            addApply (Just x) = Just (Map.unionWith (++) x apply)
        Map.alter addApply (parseUUID lamId) & pure
    where
        Right parentScope = obj .: "parentScope"
        Right scope = obj .: "scope"
        Right lamId = obj .: "lamId"

completionSuccess :: Json.Object -> Parse (ER.Val ())
completionSuccess obj =
    case obj .: "result" of
    Left{} -> error "Completion success report missing result"
    Right x -> parseResult x

completionError ::
    Monad m => Json.Object -> m (ER.EvalException UUID)
completionError obj =
    case obj .: "err" of
    Left{} -> "Completion error report missing valid err: " ++ show obj & error
    Right x ->
        ER.EvalException
        <$> do
                errTypeStr <- x .: "error"
                exceptionMStr <- x .:? "exception"
                ER.decodeJsErrorException errTypeStr exceptionMStr
        <*> (
            case (,) <$> (x .: "globalId") <*> (x .: "exprId") of
            Left{} -> pure Nothing
            Right (g, e) ->
                (,)
                <$> ER.decodeWhichGlobal g
                ?? parseUUID e
                <&> Just
        )
        & either error pure

processEvent ::
    IORef EvalResults -> Actions ->
    Json.Object -> IO ()
processEvent resultsRef actions obj =
    case event of
    "Result" ->
        runParse (addVal obj) (ER.erExprValues %~)
    "NewScope" ->
        runParse (newScope obj) (ER.erAppliesOfLam %~)
    "CompletionSuccess" ->
        runParse (completionSuccess obj) (\res -> ER.erCompleted ?~ Right res)
    "CompletionError" ->
        runParse (completionError obj) (\exc -> ER.erCompleted ?~ Left exc)
    _ -> "Unknown event " ++ event & putStrLn
    where
        runParse act postProcess =
            do
                atomicModifyIORef' resultsRef $
                    \oldEvalResults ->
                    let (res, newCache) = runState act (oldEvalResults ^. ER.erCache)
                    in  oldEvalResults
                        & ER.erCache .~ newCache
                        & postProcess res
                        & (, ())
                actions ^. aReportUpdatesAvailable
        Right event = obj .: "event"

withJSDebugHandles :: Traversable t => t FilePath -> (t Handle -> IO a) -> IO a
withJSDebugHandles paths =
    traverse withPath paths & runContT
    where
        withPath path = withFile path WriteMode & ContT

compilerActions ::
    MVar Dependencies -> Actions -> (String -> IO ()) ->
    Compiler.Actions IO
compilerActions depsMVar actions output =
    Compiler.Actions
    { Compiler.readAssocName = pure . fromString . identHex . tagName
    , Compiler.readAssocTag = pure anonTag & const
    , Compiler.readGlobal =
        readGlobal $
        \def ->
        ( Dependencies
            { subExprDeps =
                def ^.. Def.defBody . Lens.folded . hflipped
                >>= hfoldMap (const (^.. Lens._Wrapped))
                & Set.fromList
            , globalDeps = mempty
            }
        , def & Def.defBody . Lens.mapped . hflipped %~ hmap (const (Lens._Wrapped %~ Compiler.ValId))
        )
    , Compiler.readGlobalType = readGlobal ((^. Def.defType) <&> (,) mempty)
    , Compiler.output = output
    , Compiler.loggingMode = Compiler.loggingEnabled
    }
    where
        readGlobal f globalId =
            modifyMVar depsMVar $ \oldDeps ->
            globalId & actions ^. aLoadGlobal
            <&> f
            <&> _1 %~ \deps ->
            -- This happens inside the modifyMVar so
            -- loads are under "lock" and not racy
            oldDeps <> deps <>
            Dependencies
            { subExprDeps = mempty
            , globalDeps = Set.singleton globalId
            }

stripInteractive :: ByteString -> ByteString
stripInteractive line
    | "> " `BS.isPrefixOf` line = stripInteractive (BS.drop 2 line)
    | otherwise = BS.dropWhile (`elem` irrelevant) line
    where
        irrelevant = BS.unpack ". "

processNodeOutput :: Maybe Handle -> (Json.Object -> IO ()) -> Handle -> IO ()
processNodeOutput copyNodeOutput handleEvent stdout =
    do
        isEof <- hIsEOF stdout
        when isEof $ fail "EOF"
        rawLine <- BS.hGetLine stdout
        traverse_ (flushedLine rawLine) copyNodeOutput
        let line = stripInteractive rawLine
        case Aeson.decode (BS.lazify line) of
            Nothing
                | line `elem` ["'use strict'", "undefined", "Type \".help\" for more information.", ""]
                || "Welcome to Node.js " `BS.isPrefixOf` line -> pure ()
                | otherwise -> "Failed to decode: " ++ show line & fail
            Just obj -> handleEvent obj
    & forever
    where
        flushedLine line handle =
            do
                BS.hPutStr handle line
                BS.hPutStr handle "\n"
                hFlush handle

asyncStart ::
    MVar Dependencies -> MVar () -> IORef EvalResults ->
    Def.Expr (Val UUID) -> Actions ->
    IO ()
asyncStart depsMVar executeReplMVar resultsRef replVal actions =
    withSystemTempFile "lamdu-output.js" $
    \lamduOutputPath lamduOutputHandle ->
    do
        procParams <- nodeRepl
        withProcess procParams $
            \(Just stdin, Just stdout, Nothing, _handle) ->
            withJSDebugHandles (actions ^. aJSDebugPaths) $ \jsHandles ->
            do
                let handlesJS = lamduOutputHandle : jsHandles ^.. jsDebugCodePath . Lens._Just
                let outputJS line = traverse_ (`hPutStrLn` line) handlesJS
                let flushJS = traverse_ hFlush handlesJS
                let handleEvent = processEvent resultsRef actions
                let nodeOutputHandle = jsHandles ^. jsDebugNodeOutputPath
                let processOutput = processNodeOutput nodeOutputHandle handleEvent stdout
                withForkedIO processOutput $
                    do
                        replVal
                            <&> hflipped %~ hmap (const (Lens._Wrapped %~ Compiler.ValId))
                            & Compiler.compileRepl (compilerActions depsMVar actions outputJS)
                        flushJS
                        let flushedOutput handle msg =
                                do
                                    hPutStrLn handle msg
                                    hFlush handle
                        let outputInteractive msg =
                                do
                                    flushedOutput stdin msg
                                    traverse_ (`flushedOutput` msg)
                                        (jsHandles ^. jsDebugInteractivePath)
                        "'use strict';\n" ++
                            "var repl = require(" ++ show lamduOutputPath ++ ");"
                            & outputInteractive
                        do
                            takeMVar executeReplMVar
                            outputInteractive "repl(x => undefined);"
                            & forever

-- | Pause the evaluator, yielding all dependencies of evaluation so
-- far. If any dependency changed, this evaluation is stale.
--
-- Pause must be called for a started/resumed evaluator, and if given
-- an already paused evaluator, will wait for its resumption.
whilePaused :: Evaluator -> (Dependencies -> IO a) -> IO a
whilePaused = withMVar . eDeps

start ::
    Actions -> Def.Expr (Val UUID) -> IO Evaluator
start actions replExpr =
    do
        depsMVar <-
            newMVar Dependencies
            { globalDeps = Set.empty
            , subExprDeps =
                replExpr ^.. Lens.folded . hflipped
                >>= hfoldMap (const (^.. Lens._Wrapped))
                & Set.fromList
            }
        resultsRef <- newIORef ER.empty
        executeReplMVar <- newEmptyMVar
        tid <- asyncStart depsMVar executeReplMVar resultsRef replExpr actions & forkIO
        pure Evaluator
            { stop = killThread tid
            , executeReplIOProcess = putMVar executeReplMVar ()
            , eDeps = depsMVar
            , eResultsRef = resultsRef
            }

getResults :: Evaluator -> IO EvalResults
getResults = readIORef . eResultsRef
