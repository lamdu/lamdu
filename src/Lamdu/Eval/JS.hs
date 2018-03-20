{-# LANGUAGE TemplateHaskell #-}
-- | Run a process that evaluates given compiled
module Lamdu.Eval.JS
    ( Evaluator
    , Actions(..), aLoadGlobal, aReportUpdatesAvailable, aCompleted
    , start, stop, executeReplIOProcess
    , Dependencies(..), whilePaused
    , getResults
    ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Monad (foldM, msum)
import           Control.Monad.Trans.State (State, runState)
import qualified Data.Aeson as JsonStr
import           Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as SBS
import           Data.ByteString.Utils (lazifyBS)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Identifier (Identifier(..), identHex)
import           Lamdu.Calc.Type (Tag(..))
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Data.Anchors (anonTag)
import           Lamdu.Data.Definition (Definition)
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Eval.JS.Compiler as Compiler
import           Lamdu.Eval.Results (ScopeId(..), EvalResults(..))
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.Paths as Paths
import           Numeric (readHex)
import           System.FilePath (splitFileName)
import           System.IO (IOMode(..), Handle, hClose, hIsEOF, hPutStrLn, hFlush, withFile)
import qualified System.NodeJS.Path as NodeJS
import qualified System.Process as Proc

import           Lamdu.Prelude

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

data Actions srcId = Actions
    { _aLoadGlobal :: V.Var -> IO (Definition (Val srcId) ())
    , _aReportUpdatesAvailable :: IO ()
    , _aCompleted :: Either E.SomeException (ER.Val srcId) -> IO ()
    , _aCopyJSOutputPath :: Maybe FilePath
    }
Lens.makeLenses ''Actions

data Dependencies srcId = Dependencies
    { subExprDeps :: Set srcId
    , globalDeps :: Set V.Var
    }
instance Ord srcId => Semigroup (Dependencies srcId) where
    Dependencies x0 y0 <> Dependencies x1 y1 = Dependencies (x0 <> x1) (y0 <> y1)
instance Ord srcId => Monoid (Dependencies srcId) where
    mempty = Dependencies mempty mempty
    mappend = (<>)

data Evaluator srcId = Evaluator
    { stop :: IO ()
    , executeReplIOProcess :: IO ()
    , eDeps :: MVar (Dependencies srcId)
    , eResultsRef :: IORef (EvalResults srcId)
    }

type Parse = State (IntMap (ER.Val ()))

nodeRepl :: FilePath -> FilePath -> Proc.CreateProcess
nodeRepl nodeExePath rtsPath =
    (Proc.proc nodeExePath ["--interactive", "--harmony-tailcalls"])
    { Proc.cwd = Just rtsPath
    }

withProcess ::
    Proc.CreateProcess ->
    ((Maybe Handle, Maybe Handle, Maybe Handle, Proc.ProcessHandle) -> IO a) ->
    IO a
withProcess createProc =
    E.bracket
    (Proc.createProcess createProc
     { Proc.std_in = Proc.CreatePipe
     , Proc.std_out = Proc.CreatePipe
     })
    close
    where
      close (_, mStdout, mStderr, handle) =
          do
              _ <- [mStdout, mStderr] & Lens.traverse . Lens.traverse %%~ hClose
              Proc.terminateProcess handle

parseHexBs :: Text -> ByteString
parseHexBs =
    SBS.pack . map (fst . sHead . readHex . Text.unpack) . Text.chunksOf 2
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

parseRecord :: HashMap Text Json.Value -> Parse (ER.Val ())
parseRecord obj =
    HashMap.toList obj & foldM step (ER.Val () ER.RRecEmpty)
    where
        step r ("cacheId", _) = pure r
        step r (k, v) =
            parseResult v
            <&> \pv ->
            ER.RRecExtend V.RecExtend
            { V._recTag = parseHexNameBs k & Identifier & Tag
            , V._recFieldVal = pv
            , V._recRest = r
            } & ER.Val ()

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
    & SBS.pack & PrimVal.Bytes & PrimVal.fromKnown & ER.RPrimVal & ER.Val ()
parseBytes _ = error "Bytes with non-array data"

parseInject :: Text -> Maybe Json.Value -> Parse (ER.Val ())
parseInject tag mData =
    case mData of
    Nothing -> ER.Val () ER.RRecEmpty & pure
    Just v -> parseResult v
    <&> \iv ->
    ER.RInject V.Inject
    { V._injectTag = parseHexNameBs tag & Identifier & Tag
    , V._injectVal = iv
    } & ER.Val ()

(.?) :: Json.FromJSON a => Json.Object -> Text -> Maybe a
obj .? tag = Json.parseMaybe (.: tag) obj

parseObj :: Json.Object -> Parse (ER.Val ())
parseObj obj =
    msum
    [ obj .? "array"
      <&> \(Json.Array arr) ->
            Vec.toList arr & Lens.traversed %%~ parseResult <&> ER.RArray <&> ER.Val ()
    , obj .? "bytes" <&> parseBytes <&> pure
    , obj .? "number" <&> read <&> fromDouble <&> pure
    , obj .? "tag" <&> (`parseInject` (obj .? "data"))
    , obj .? "func" <&> (\(Json.Number x) -> round x & ER.RFunc & ER.Val () & pure)
    ] & fromMaybe (parseRecord obj)

parseResult :: Json.Value -> Parse (ER.Val ())
parseResult (Json.Number x) = realToFrac x & fromDouble & pure
parseResult (Json.Object obj) =
    case obj .? "cachedVal" of
    Just cacheId -> Lens.use (Lens.singular (Lens.ix cacheId))
    Nothing ->
        do
            val <- parseObj obj
            case obj .? "cacheId" <|> obj .? "func" of
                Nothing -> pure ()
                Just cacheId -> Lens.at cacheId ?= val
            pure val
parseResult x = "Unsupported encoded JS output: " ++ show x & fail

fromDouble :: Double -> ER.Val ()
fromDouble = ER.Val () . ER.RPrimVal . PrimVal.fromKnown . PrimVal.Float

addVal ::
    Ord srcId =>
    (UUID -> srcId) -> Json.Object ->
    Parse
    ( Map srcId (Map ScopeId (ER.Val ())) ->
      Map srcId (Map ScopeId (ER.Val ()))
    )
addVal fromUUID obj =
    case obj .? "result" of
    Nothing -> pure id
    Just result ->
        parseResult result
        <&> \pr ->
        Map.alter
        (<> Just (Map.singleton (ScopeId scope) pr))
        (fromUUID (parseUUID exprId))
    where
        Just scope = obj .? "scope"
        Just exprId = obj .? "exprId"

newScope ::
    Ord srcId =>
    (UUID -> srcId) -> Json.Object ->
    Parse
    ( Map srcId (Map ScopeId [(ScopeId, ER.Val ())]) ->
      Map srcId (Map ScopeId [(ScopeId, ER.Val ())])
    )
newScope fromUUID obj =
    do
        arg <-
            case obj .? "arg" of
            Nothing -> fail "Scope report missing arg"
            Just x -> parseResult x
        let apply = Map.singleton (ScopeId parentScope) [(ScopeId scope, arg)]
        let addApply Nothing = Just apply
            addApply (Just x) = Just (Map.unionWith (++) x apply)
        Map.alter addApply (fromUUID (parseUUID lamId)) & pure
    where
        Just parentScope = obj .? "parentScope"
        Just scope = obj .? "scope"
        Just lamId = obj .? "lamId"

atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref (flip (,) () . f)

processEvent ::
    Ord srcId => (UUID -> srcId) -> IORef (EvalResults srcId) -> Json.Object -> IO ()
processEvent fromUUID resultsRef obj =
    case event of
    "Result" ->
        runParse (addVal fromUUID obj) (ER.erExprValues %~)
    "NewScope" ->
        runParse (newScope fromUUID obj) (ER.erAppliesOfLam %~)
    _ -> "Unknown event " ++ event & putStrLn
    where
        runParse act postProcess =
            atomicModifyIORef'_ resultsRef $
            \oldEvalResults ->
            let (res, newCache) = runState act (oldEvalResults ^. ER.erCache)
            in  oldEvalResults
                & ER.erCache .~ newCache
                & postProcess res
        Just event = obj .? "event"

withCopyJSOutputTo :: Maybe FilePath -> ((String -> IO ()) -> IO a) -> IO a
withCopyJSOutputTo Nothing f = f $ \_js -> pure ()
withCopyJSOutputTo (Just path) f =
    withFile path WriteMode $ \outputFile -> f (hPutStrLn outputFile)

compilerActions ::
    Ord a =>
    (a -> UUID) -> MVar (Dependencies a) -> Actions a -> (String -> IO ()) ->
    Compiler.Actions IO
compilerActions toUUID depsMVar actions output =
    Compiler.Actions
    { Compiler.readAssocName = pure . fromString . identHex . tagName
    , Compiler.readAssocTag = pure anonTag & const
    , Compiler.readGlobal =
        readGlobal $
        \def ->
        ( Dependencies
          { subExprDeps = def ^.. Def.defBody . Lens.folded . Lens.folded & Set.fromList
          , globalDeps = mempty
          }
        , def & Def.defBody . Lens.mapped . Lens.mapped %~ Compiler.ValId . toUUID
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
    | "> " `SBS.isPrefixOf` line = stripInteractive (SBS.drop 2 line)
    | otherwise = SBS.dropWhile (`elem` irrelevant) line
    where
        irrelevant = SBS.unpack ". "

asyncStart ::
    Ord srcId =>
    (srcId -> UUID) -> (UUID -> srcId) ->
    MVar (Dependencies srcId) -> MVar () -> IORef (EvalResults srcId) ->
    Def.Expr (Val srcId) -> Actions srcId ->
    IO ()
asyncStart toUUID fromUUID depsMVar executeReplMVar resultsRef val actions =
    do
        nodeExePath <- NodeJS.path
        rtsPath <- Paths.getDataFileName "js/rts.js" <&> fst . splitFileName
        withProcess (nodeRepl nodeExePath rtsPath) $
            \(Just stdin, Just stdout, Nothing, _handle) ->
            withCopyJSOutputTo (actions ^. aCopyJSOutputPath) $ \copyJSOutput ->
            do
                let output line =
                        do
                            copyJSOutput line
                            hPutStrLn stdin line
                let
                    processLines =
                        do
                            isEof <- hIsEOF stdout
                            unless isEof $
                                do
                                    line <- SBS.hGetLine stdout <&> stripInteractive
                                    case JsonStr.decode (lazifyBS line) of
                                        Nothing
                                            | line `elem` ["'use strict'", "undefined", ""] -> processLines
                                            | otherwise -> "Failed to decode: " ++ show line & error >> processLines
                                        Just obj ->
                                            do
                                                processEvent fromUUID resultsRef obj
                                                actions ^. aReportUpdatesAvailable
                                                processLines
                _ <- forkIO processLines
                val <&> Lens.mapped %~ Compiler.ValId . toUUID
                    & Compiler.compile
                        (compilerActions toUUID depsMVar actions output)
                hFlush stdin
                forever (takeMVar executeReplMVar >> output "repl(x => undefined);" >> hFlush stdin)

-- | Pause the evaluator, yielding all dependencies of evaluation so
-- far. If any dependency changed, this evaluation is stale.
--
-- Pause must be called for a started/resumed evaluator, and if given
-- an already paused evaluator, will wait for its resumption.
whilePaused :: Evaluator srcId -> (Dependencies srcId -> IO a) -> IO a
whilePaused = withMVar . eDeps

start ::
    Ord srcId => (srcId -> UUID) -> (UUID -> srcId) ->
    Actions srcId -> Def.Expr (Val srcId) -> IO (Evaluator srcId)
start toUUID fromUUID actions defExpr =
    do
        depsMVar <-
            newMVar Dependencies
            { globalDeps = Set.empty
            , subExprDeps = defExpr ^.. Lens.folded . Lens.folded & Set.fromList
            }
        resultsRef <- newIORef ER.empty
        executeReplMVar <- newEmptyMVar
        tid <- asyncStart toUUID fromUUID depsMVar executeReplMVar resultsRef defExpr actions & forkIO
        pure Evaluator
            { stop = killThread tid
            , executeReplIOProcess = putMVar executeReplMVar ()
            , eDeps = depsMVar
            , eResultsRef = resultsRef
            }

getResults :: Evaluator srcId -> IO (EvalResults srcId)
getResults = readIORef . eResultsRef
