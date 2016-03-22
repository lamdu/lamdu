{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}
-- | Run a process that evaluates given compiled
module Lamdu.Eval.JS
    ( Evaluator
    , Actions(..), aLoadGlobal, aReadAssocName, aReportUpdatesAvailable, aCompleted
    , start, stop
    , Dependencies(..), whilePaused
    , getResults
    ) where

import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (unless)
import qualified Data.Aeson as JsonStr
import           Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.List.Split (chunksOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import           Data.Word (Word8)
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.DataFile as DataFile
import qualified Lamdu.Eval.JS.Compiler as Compiler
import           Lamdu.Eval.Results (ScopeId(..), EvalResults(..))
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.Identifier (Identifier(..))
import           Lamdu.Expr.Type (Tag(..))
import qualified Lamdu.Expr.Val as V
import           Numeric (readHex)
import           System.IO (IOMode(..), Handle, hClose, hIsEOF, hPutStrLn, withFile)
import qualified System.Process as Proc

import           Prelude.Compat

data Actions srcId = Actions
    { _aLoadGlobal :: V.Var -> IO (Def.Body (V.Val srcId))
    , -- TODO: This is currently not in use but remains here because
      -- it *should* be used for readable JS output
      _aReadAssocName :: Guid -> IO String
    , _aReportUpdatesAvailable :: IO ()
    , _aCompleted :: Either E.SomeException (ER.Val srcId) -> IO ()
    }
Lens.makeLenses ''Actions

data Dependencies srcId = Dependencies
    { subExprDeps :: Set srcId
    , globalDeps :: Set V.Var
    }
instance Ord srcId => Monoid (Dependencies srcId) where
    mempty = Dependencies mempty mempty
    mappend (Dependencies x0 y0) (Dependencies x1 y1) =
        Dependencies (x0 <> x1) (y0 <> y1)

data Evaluator srcId = Evaluator
    { stop :: IO ()
    , eDeps :: MVar (Dependencies srcId)
    , eResultsRef :: IORef (EvalResults srcId)
    }

nodeRepl :: FilePath -> Proc.CreateProcess
nodeRepl nodeExePath = Proc.proc nodeExePath ["--harmony-tailcalls"]

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

parseHexBs :: String -> SBS.ByteString
parseHexBs =
    SBS.pack . map (fst . sHead . readHex) . chunksOf 2
    where
        sHead [] = error "parseHexBs got bad input"
        sHead (x:_) = x

parseHexNameBs :: String -> SBS.ByteString
parseHexNameBs ('_':n) = parseHexBs n
parseHexNameBs n = parseHexBs n

parseGuid :: String -> Guid
parseGuid = Guid.make . parseHexNameBs

parseRecord :: HashMap Text Json.Value -> ER.Val ()
parseRecord obj =
    HashMap.toList obj & foldl step (ER.Val () ER.RRecEmpty)
    where
        step r (k, v) =
            ER.RRecExtend V.RecExtend
            { V._recTag = Text.unpack k & parseHexNameBs & Identifier & Tag
            , V._recFieldVal = parseResult v
            , V._recRest = r
            } & ER.Val ()

parseWord8 :: Json.Value -> Word8
parseWord8 (Json.Number x)
    | x == fromIntegral i = i
    where
        i = truncate x
parseWord8 x = error $ "Expected word8, given: " ++ show x

parseBytes :: Json.Value -> ER.Val ()
parseBytes (Json.Array vals) =
    Vec.toList vals
    <&> parseWord8
    & SBS.pack & PrimVal.Bytes & PrimVal.fromKnown & ER.RPrimVal & ER.Val ()
parseBytes _ = error "Bytes with non-array data"

parseInject :: String -> Maybe Json.Value -> ER.Val ()
parseInject tag mData =
    ER.RInject V.Inject
    { V._injectTag = parseHexNameBs tag & Identifier & Tag
    , V._injectVal =
        case mData of
        Nothing -> ER.Val () ER.RRecEmpty
        Just v -> parseResult v
    } & ER.Val ()

parseResult :: Json.Value -> ER.Val ()
parseResult (Json.Number x) =
    realToFrac x & PrimVal.Float & PrimVal.fromKnown & ER.RPrimVal & ER.Val ()
parseResult (Json.Object obj) =
    case Json.parseMaybe (.: "tag") obj of
    Nothing -> parseRecord obj
    Just tag
        | tag == "bytes" ->
          fromMaybe (error "bytes with no data?!") dataField & parseBytes
        | tag == "function" -> ER.Val () ER.RFunc
        | otherwise -> parseInject tag dataField
        where
            dataField = Json.parseMaybe (.: "data") obj
parseResult x = error $ "Unsupported encoded JS output: " ++ show x

addVal ::
    Ord srcId =>
    (Guid -> srcId) -> Json.Object ->
    Map srcId (Map ScopeId (ER.Val ())) ->
    Map srcId (Map ScopeId (ER.Val ()))
addVal fromGuid obj =
    case Json.parseMaybe (.: "result") obj of
    Nothing -> id
    Just result ->
        Map.alter
        (<> Just (Map.singleton (ScopeId scope) (parseResult result)))
        (fromGuid (parseGuid exprId))
    where
        Just scope = Json.parseMaybe (.: "scope") obj
        Just exprId = Json.parseMaybe (.: "exprId") obj

newScope ::
    Ord srcId =>
    (Guid -> srcId) -> Json.Object ->
    Map srcId (Map ScopeId [(ScopeId, ER.Val ())]) ->
    Map srcId (Map ScopeId [(ScopeId, ER.Val ())])
newScope fromGuid obj =
    Map.alter addApply (fromGuid (parseGuid lamId))
    where
        addApply Nothing = Just apply
        addApply (Just x) = Just (Map.unionWith (++) x apply)
        apply = Map.singleton (ScopeId parentScope) [(ScopeId scope, arg)]
        Just parentScope = Json.parseMaybe (.: "parentScope") obj
        Just scope = Json.parseMaybe (.: "scope") obj
        Just lamId = Json.parseMaybe (.: "lamId") obj
        arg =
            case Json.parseMaybe (.: "arg") obj of
            Nothing -> error "Scope report missing arg"
            Just x -> parseResult x

processEvent ::
    Ord srcId => (Guid -> srcId) -> IORef (EvalResults srcId) -> Json.Object -> IO ()
processEvent fromGuid resultsRef obj =
    case event of
    "Result" ->
        atomicModifyIORef' resultsRef
        (flip (,) () . (ER.erExprValues %~ addVal fromGuid obj))
    "NewScope" ->
        atomicModifyIORef' resultsRef
        (flip (,) () . (ER.erAppliesOfLam %~ newScope fromGuid obj))
    _ -> "Unknown event " ++ event & putStrLn
    where
        Just event = Json.parseMaybe (.: "event") obj

asyncStart ::
    Ord srcId =>
    (srcId -> Guid) -> (Guid -> srcId) ->
    MVar (Dependencies srcId) -> IORef (EvalResults srcId) ->
    V.Val srcId -> Actions srcId ->
    IO ()
asyncStart toGuid fromGuid depsMVar resultsRef val actions =
    do
        nodeExePath <- DataFile.getPath "submodules/node/node"
        rtsPath <- DataFile.getPath "js/rts.js"
        withProcess (nodeRepl nodeExePath) $ \(Just stdin, Just stdout, Nothing, handle) ->
            withFile "output.js" WriteMode $ \outputFile ->
            do
                val
                    <&> valId
                    & Compiler.compile Compiler.Actions
                    { Compiler.readAssocName = return . Guid.asHex
                    , Compiler.readGlobal =
                      \globalId ->
                      modifyMVar depsMVar $ \oldDeps ->
                      do
                          -- This happens inside the modifyMVar so
                          -- loads are under "lock" and not racy
                          defBody <- globalId & actions ^. aLoadGlobal
                          return
                              ( oldDeps <> Dependencies
                                { subExprDeps = defBody ^.. Lens.folded . Lens.folded & Set.fromList
                                , globalDeps = Set.singleton globalId
                                }
                              , defBody <&> fmap valId
                              )
                    , Compiler.output =
                      \line ->
                      do
                          hPutStrLn outputFile line
                          hPutStrLn stdin line
                    , Compiler.jsRtsPath = rtsPath
                    }
                hClose stdin
                let
                    processLines =
                        do
                            isEof <- hIsEOF stdout
                            unless isEof $
                                do
                                    line <- SBS.hGetLine stdout
                                    let Just obj = JsonStr.decode (LBS.fromChunks [line])
                                    processEvent fromGuid resultsRef obj
                                    actions ^. aReportUpdatesAvailable
                                    processLines
                processLines
                _ <- Proc.waitForProcess handle
                return ()
    where
        valId = Compiler.ValId . toGuid

-- | Pause the evaluator, yielding all dependencies of evaluation so
-- far. If any dependency changed, this evaluation is stale.
--
-- Pause must be called for a started/resumed evaluator, and if given
-- an already paused evaluator, will wait for its resumption.
whilePaused :: Evaluator srcId -> (Dependencies srcId -> IO a) -> IO a
whilePaused = withMVar . eDeps

start ::
    Ord srcId => (srcId -> Guid) -> (Guid -> srcId) ->
    Actions srcId -> V.Val srcId -> IO (Evaluator srcId)
start toGuid fromGuid actions val =
    do
        depsMVar <-
            newMVar Dependencies
            { globalDeps = Set.empty
            , subExprDeps = val ^.. Lens.folded & Set.fromList
            }
        resultsRef <- newIORef ER.empty
        tid <- asyncStart toGuid fromGuid depsMVar resultsRef val actions & forkIO
        return Evaluator
            { stop = killThread tid
            , eDeps = depsMVar
            , eResultsRef = resultsRef
            }

getResults :: Evaluator srcId -> IO (EvalResults srcId)
getResults = readIORef . eResultsRef
