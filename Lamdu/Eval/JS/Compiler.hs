{-# LANGUAGE NoImplicitPrelude, LambdaCase, GeneralizedNewtypeDeriving, TemplateHaskell, QuasiQuotes, OverloadedStrings, PolymorphicComponents #-}
-- | Compile Lamdu vals to Javascript

module Lamdu.Eval.JS.Compiler
    ( Actions(..)
    , ValId(..)
    , compile, Mode(..), loggingEnabled
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS.Strict (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as RWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Hex as Hex
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Char as Char
import           Data.Default () -- instances
import           Data.List (isPrefixOf)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Compiler.Flatten as Flatten
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.Identifier (identHex)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Language.ECMAScript3.PrettyPrint as JSPP
import qualified Language.ECMAScript3.Syntax as JSS
import qualified Language.ECMAScript3.Syntax.CodeGen as JS
import           Language.ECMAScript3.Syntax.QuasiQuote (jsstmt)
import qualified Text.PrettyPrint.Leijen as Pretty

import           Prelude.Compat

newtype ValId = ValId UUID

data Mode = FastSilent | SlowLogging LoggingInfo
    deriving Show

data Actions m = Actions
    { readAssocName :: UUID -> m String
    , readGlobal :: V.Var -> m (Definition.Body (Val ValId))
    , output :: String -> m ()
    , loggingMode :: Mode
    }

type LocalVarName = JSS.Id ()
type GlobalVarName = JSS.Id ()

newtype LoggingInfo = LoggingInfo
    { _liScopeDepth :: Int
    } deriving Show
Lens.makeLenses ''LoggingInfo

data Env m = Env
    { _envActions :: Actions m
    , _envLocals :: Map V.Var LocalVarName
    , _envMode :: Mode
    }
Lens.makeLenses ''Env

data State = State
    { _freshId :: Int
    , _names :: Map String (Map UUID String)
    , _compiled :: Map V.Var GlobalVarName
    }
Lens.makeLenses ''State

data LogUsed
    = LogUnused
    | LogUsed
    deriving (Eq, Ord, Show)
instance Monoid LogUsed where
    mempty = LogUnused
    mappend LogUsed _ = LogUsed
    mappend _ LogUsed = LogUsed
    mappend _ _ = LogUnused

newtype M m a = M { unM :: RWST (Env m) LogUsed State m a }
    deriving (Functor, Applicative, Monad)

infixl 4 $.
($.) :: JSS.Expression () -> JSS.Id () -> JSS.Expression ()
($.) = JS.dot

infixl 3 $$
($$) :: JSS.Expression () -> JSS.Expression () -> JSS.Expression ()
f $$ x = f `JS.call` [x]

pp :: JSS.Statement () -> String
pp = (`Pretty.displayS`"") . Pretty.renderPretty 1.0 90 . JSPP.prettyPrint

performAction :: Monad m => (Actions m -> m a) -> M m a
performAction f = RWS.asks (f . _envActions) >>= lift & M

ppOut :: Monad m => JSS.Statement () -> M m ()
ppOut stmt = performAction (`output` pp stmt)

-- Multiple vars using a single "var" is badly formatted and generally
-- less readable than a vardecl for each:
varinit :: JSS.Id () -> JSS.Expression () -> JSS.Statement ()
varinit ident expr = JS.vardecls [JS.varinit ident expr]

scopeIdent :: Int -> JSS.Id ()
scopeIdent depth = "scopeId_" ++ show depth & JS.ident

declLog :: Int -> JSS.Statement ()
declLog depth =
    varinit "log" $
    JS.lambda ["exprId", "result"]
    [ (JS.var "rts" $. "logResult") `JS.call`
      [ JS.var (scopeIdent depth)
      , JS.var "exprId"
      , JS.var "result"
      ] & JS.returns
    ]

isReservedName :: String -> Bool
isReservedName name =
    name `elem`
    [ "x", "repl"
    , "Object", "console", "repl"
    , "log", "scopeCounter", "rts"
    , "while", "if", "switch", "case", "default"
    , "var", "return", "function", "throw"
    ]
    || any (`isPrefixOf` name)
    [ "global_"
    , "local_"
    , "scopeId_"
    ]

topLevelDecls :: [JSS.Statement ()]
topLevelDecls =
    ( [ [jsstmt|"use strict";|]
      , [jsstmt|var scopeId_0 = 0;|]
      , [jsstmt|var scopeCounter = 1;|]
      , [jsstmt|var rts = require('rts.js');|]
      ] <&> void
    ) ++
    [ declLog 0
    ]

loggingEnabled :: Mode
loggingEnabled = SlowLogging LoggingInfo { _liScopeDepth = 0 }

compile :: Monad m => Actions m -> Val ValId -> m ()
compile actions val = compileVal val & run actions

run :: Monad m => Actions m -> M m CodeGen -> m ()
run actions act =
    runRWST
    (traverse ppOut topLevelDecls
     >> act
     <&> codeGenExpression
     <&> JS.call (JS.var "rts" $. "logRepl") . (:[])
     <&> JS.expr
     >>= ppOut & unM)
    Env
    { _envActions = actions
    , _envLocals = mempty
    , _envMode = loggingMode actions
    }
    State
    { _freshId = 0
    , _names = mempty
    , _compiled = mempty
    }
    <&> (^. _1)

-- | Reset reader/writer components of RWS for a new global compilation context
resetRW :: Monad m => M m a -> M m a
resetRW (M act) =
    act
    & RWS.censor (const LogUnused)
    & RWS.local (envLocals .~ mempty)
    & RWS.local (\x -> x & envMode .~ loggingMode (x ^. envActions))
    & M

freshName :: Monad m => String -> M m String
freshName prefix =
    do
        newId <- freshId <+= 1
        prefix ++ show newId & return
    & M

avoidReservedNames :: String -> String
avoidReservedNames name
    | isReservedName name = "_" ++ name
    | otherwise = name

escapeName :: String -> String
escapeName (d:xs)
    | Char.isDigit d = '_' : d : replaceSpecialChars xs
escapeName xs = replaceSpecialChars xs

replaceSpecialChars :: String -> String
replaceSpecialChars = concatMap replaceSpecial
    where
        replaceSpecial x
            | Char.isAlphaNum x = [x]
            | x == '_' = "__"
            | otherwise = '_' : (Hex.showHexByte . fromIntegral . Char.ord) x

readName :: (UniqueId.ToUUID a, Monad m) => a -> M m String -> M m String
readName g act =
    do
        name <-
            performAction (`readAssocName` uuid)
            <&> avoidReservedNames
            <&> escapeName
            >>= \case
                "" -> act
                name -> return name
        names . Lens.at name %%= \case
            Nothing -> (name, Just (Map.singleton uuid name))
            Just uuidMap ->
                uuidMap
                & Lens.at uuid %%~
                \case
                Nothing -> (newName, Just newName)
                    where
                        newName = name ++ show (Map.size uuidMap)
                Just oldName -> (oldName, Just oldName)
                <&> Just
            & M
    where
        uuid = UniqueId.toUUID g

freshStoredName :: (Monad m, UniqueId.ToUUID a) => a -> String -> M m String
freshStoredName g prefix = readName g (freshName prefix)

tagString :: Monad m => T.Tag -> M m String
tagString tag@(T.Tag ident) = readName tag ("tag" ++ identHex ident & return)

tagIdent :: Monad m => T.Tag -> M m (JSS.Id ())
tagIdent = fmap JS.ident . tagString

local :: Monad m => (Env m -> Env m) -> M m a -> M m a
local f (M act) = M (RWS.local f act)

withLocalVar :: Monad m => V.Var -> M m a -> M m (LocalVarName, a)
withLocalVar v act =
    do
        varName <- freshStoredName v "local_" <&> JS.ident
        res <- local (envLocals . Lens.at v ?~ varName) act
        return (varName, res)

compileGlobal :: Monad m => V.Var -> M m (JSS.Expression ())
compileGlobal globalId =
    performAction (`readGlobal` globalId) >>=
    \case
    Definition.BodyBuiltin (Definition.Builtin ffiName _scheme) ->
        ffiCompile ffiName & return
    Definition.BodyExpr (Definition.Expr val _type _usedDefs) ->
        compileVal val <&> codeGenExpression
    & resetRW

compileGlobalVar :: Monad m => V.Var -> M m CodeGen
compileGlobalVar var =
    Lens.use (compiled . Lens.at var) & M
    >>= maybe newGlobal return
    <&> JS.var
    <&> codeGenFromExpr
    where
        newGlobal =
            do
                varName <- freshStoredName var "global_" <&> JS.ident
                compiled . Lens.at var ?= varName & M
                compileGlobal var
                    <&> varinit varName
                    >>= ppOut
                return varName

compileLocalVar :: JSS.Id () -> CodeGen
compileLocalVar = codeGenFromExpr . JS.var

compileVar :: Monad m => V.Var -> M m CodeGen
compileVar v =
    Lens.view (envLocals . Lens.at v) & M
    >>= maybe (compileGlobalVar v) (return . compileLocalVar)

data CodeGen = CodeGen
    { codeGenLamStmts :: [JSS.Statement ()]
    , codeGenExpression :: JSS.Expression ()
    }

unitRedex :: [JSS.Statement ()] -> JSS.Expression ()
unitRedex stmts = JS.lambda [] stmts `JS.call` []

throwStr :: String -> CodeGen
throwStr str =
    go [JS.throw (JS.string str)]
    where
        go stmts =
            CodeGen
            { codeGenLamStmts = stmts
            , codeGenExpression = unitRedex stmts
            }

codeGenFromLamStmts :: [JSS.Statement ()] -> CodeGen
codeGenFromLamStmts stmts =
    CodeGen
    { codeGenLamStmts = stmts
    , codeGenExpression = JS.lambda [] stmts `JS.call` []
    }

codeGenFromExpr :: JSS.Expression () -> CodeGen
codeGenFromExpr expr =
    CodeGen
    { codeGenLamStmts = [JS.returns expr]
    , codeGenExpression = expr
    }

lam ::
    Monad m => String ->
    (JSS.Expression () -> M m [JSS.Statement ()]) ->
    M m (JSS.Expression ())
lam prefix code =
    do
        var <- freshName prefix <&> JS.ident
        code (JS.var var) <&> JS.lambda [var]

inject :: JSS.Expression () -> JSS.Expression () -> JSS.Expression ()
inject tagStr dat' =
    JS.object
    [ (JS.propId "tag", tagStr)
    , (JS.propId "data", dat')
    ]

ffiCompile :: Definition.FFIName -> JSS.Expression ()
ffiCompile (Definition.FFIName modul funcName) =
    foldl ($.) (JS.var "rts" $. "builtins") (modul <&> JS.ident)
    `JS.brack` JS.string funcName

compileLiteral :: V.PrimVal -> CodeGen
compileLiteral literal =
    case PrimVal.toKnown literal of
    PrimVal.Bytes bytes ->
        JS.var "rts" $. "bytes" $$ JS.array ints & codeGenFromExpr
        where
            ints = [JS.int (fromIntegral byte) | byte <- BS.unpack bytes]
    PrimVal.Float num -> JS.number num & codeGenFromExpr

compileRecExtend :: Monad m => V.RecExtend (Val ValId) -> M m CodeGen
compileRecExtend x =
    do
        Flatten.Composite tags mRest <- Flatten.recExtend x & Lens.traverse compileVal
        strTags <-
            Map.toList tags
            <&> _2 %~ codeGenExpression
            & Lens.traversed . _1 %%~ tagString
        case mRest of
            Nothing ->
                strTags <&> _1 %~ JS.propId . JS.ident
                & JS.object & codeGenFromExpr
            Just rest ->
                CodeGen
                { codeGenLamStmts = stmts
                , codeGenExpression = unitRedex stmts
                }
                where
                    stmts =
                        varinit "rest"
                        (JS.var "Object" $. "create" $$ codeGenExpression rest)
                        : ( strTags
                            <&> _1 %~ JS.ldot (JS.var "rest")
                            <&> uncurry JS.assign
                            <&> JS.expr )
                        ++ [JS.var "rest" & JS.returns]
            & return

compileInject :: Monad m => V.Inject (Val ValId) -> M m CodeGen
compileInject (V.Inject tag dat) =
    do
        tagStr <- tagString tag <&> JS.string
        dat' <- compileVal dat
        inject tagStr (codeGenExpression dat') & codeGenFromExpr & return

compileCase :: Monad m => V.Case (Val ValId) -> M m CodeGen
compileCase = fmap codeGenFromExpr . lam "x" . compileCaseOnVar

compileCaseOnVar ::
    Monad m => V.Case (Val ValId) -> JSS.Expression () -> M m [JSS.Statement ()]
compileCaseOnVar x scrutineeVar =
    do
        tagsStr <- Map.toList tags & Lens.traverse . _1 %%~ tagString
        cases <- traverse makeCase tagsStr
        defaultCase <-
            case mRestHandler of
            Nothing ->
                return [JS.throw (JS.string "Unhandled case? This is a type error!")]
            Just restHandler ->
                compileAppliedFunc restHandler scrutineeVar
                <&> codeGenLamStmts
            <&> JS.defaultc
        return [JS.switch (scrutineeVar $. "tag") (cases ++ [defaultCase])]
    where
        Flatten.Composite tags mRestHandler = Flatten.case_ x
        makeCase (tagStr, handler) =
            compileAppliedFunc handler (scrutineeVar $. "data")
            <&> codeGenLamStmts
            <&> JS.casee (JS.string tagStr)

compileGetField :: Monad m => V.GetField (Val ValId) -> M m CodeGen
compileGetField (V.GetField record tag) =
    do
        tagId <- tagIdent tag
        compileVal record
            <&> codeGenExpression <&> (`JS.dot` tagId)
            <&> codeGenFromExpr

declMyScopeDepth :: Int -> JSS.Statement ()
declMyScopeDepth depth =
    varinit (scopeIdent depth) $
    JS.uassign JSS.PostfixInc "scopeCounter"

jsValId :: ValId -> JSS.Expression ()
jsValId (ValId uuid) = (JS.string . Hex.showHexBytes . UUIDUtils.toSBS16) uuid

callLogNewScope :: Int -> Int -> ValId -> JSS.Expression () -> JSS.Statement ()
callLogNewScope parentDepth myDepth lamValId argVal =
    (JS.var "rts" $. "logNewScope") `JS.call`
    [ JS.var (scopeIdent parentDepth)
    , JS.var (scopeIdent myDepth)
    , jsValId lamValId
    , argVal
    ] & JS.expr

slowLoggingLambdaPrefix ::
    LogUsed -> Int -> ValId -> JSS.Expression () -> [JSS.Statement ()]
slowLoggingLambdaPrefix logUsed parentScopeDepth lamValId argVal =
    [ declMyScopeDepth myScopeDepth
    , callLogNewScope parentScopeDepth myScopeDepth lamValId argVal
    ] ++
    [ declLog myScopeDepth | LogUsed <- [logUsed] ]
    where
        myScopeDepth = parentScopeDepth + 1

listenNoTellLogUsed :: Monad m => M m a -> M m (a, LogUsed)
listenNoTellLogUsed act =
    act & unM & RWS.listen & RWS.censor (const LogUnused) & M

compileLambda :: Monad m => V.Lam (Val ValId) -> ValId -> M m CodeGen
compileLambda (V.Lam v res) valId =
    Lens.view envMode & M
    >>= \case
        FastSilent -> compileRes <&> mkLambda
        SlowLogging loggingInfo ->
            do
                ((varName, lamStmts), logUsed) <-
                    compileRes
                    & local
                      (envMode .~ SlowLogging (loggingInfo & liScopeDepth .~ 1 + parentScopeDepth))
                    & listenNoTellLogUsed
                let stmts =
                        slowLoggingLambdaPrefix logUsed parentScopeDepth valId
                        (JS.var varName)
                fastLam <- compileRes & local (envMode .~ FastSilent) <&> mkLambda
                (JS.var "rts" $. "wrap") `JS.call`
                    [fastLam, JS.lambda [varName] (stmts ++ lamStmts)] & return
            where
                parentScopeDepth = loggingInfo ^. liScopeDepth
    <&> codeGenFromExpr
    where
        mkLambda (varId, lamStmts) = JS.lambda [varId] lamStmts
        compileRes = compileVal res <&> codeGenLamStmts & withLocalVar v

compileApply :: Monad m => V.Apply (Val ValId) -> M m CodeGen
compileApply (V.Apply func arg) =
    do
        arg' <- compileVal arg <&> codeGenExpression
        compileAppliedFunc func arg'

maybeLogSubexprResult :: Monad m => ValId -> CodeGen -> M m CodeGen
maybeLogSubexprResult valId codeGen =
    Lens.view envMode & M
    >>= \case
    FastSilent -> return codeGen
    SlowLogging _ -> logSubexprResult valId codeGen

logSubexprResult :: Monad m => ValId -> CodeGen -> M m CodeGen
logSubexprResult valId codeGen =
    do
        RWS.tell LogUsed & M
        JS.var "log" `JS.call` [jsValId valId, codeGenExpression codeGen]
            & codeGenFromExpr
            & return

compileAppliedFunc :: Monad m => Val ValId -> JSS.Expression () -> M m CodeGen
compileAppliedFunc func arg' =
    do
        mode <- Lens.view envMode & M
        case (func ^. V.body, mode) of
            (V.BCase case_, FastSilent) ->
                compileCaseOnVar case_ (JS.var "x")
                <&> (varinit "x" arg' :)
                <&> codeGenFromLamStmts
            (V.BAbs (V.Lam v res), FastSilent) ->
                do
                    (vId, lamStmts) <- compileVal res <&> codeGenLamStmts & withLocalVar v
                    return CodeGen
                        { codeGenLamStmts = varinit vId arg' : lamStmts
                        , codeGenExpression =
                            -- Can't really optimize a redex in expr
                            -- context, as at least 1 redex must be paid
                            JS.lambda [vId] lamStmts $$ arg'
                        }
            _ ->
                do
                    func' <- compileVal func <&> codeGenExpression
                    func' $$ arg' & codeGenFromExpr & return

compileLeaf :: Monad m => V.Leaf -> ValId -> M m CodeGen
compileLeaf leaf valId =
    case leaf of
    V.LHole -> throwStr "Reached hole!" & return
    V.LRecEmpty -> JS.object [] & codeGenFromExpr & return
    V.LAbsurd -> throwStr "Reached absurd!" & return
    V.LVar var -> compileVar var >>= maybeLogSubexprResult valId
    V.LLiteral literal -> compileLiteral literal & return

compileToNom :: Monad m => V.Nom (Val ValId) -> ValId -> M m CodeGen
compileToNom (V.Nom tId val) valId =
    case val ^? ExprLens.valLiteral <&> PrimVal.toKnown of
    Just (PrimVal.Bytes bytes) | tId == Builtins.textTid ->
        JS.var "rts" $. "bytesFromString" $$ JS.string (UTF8.toString bytes)
        & codeGenFromExpr & return
    _ -> compileVal val >>= maybeLogSubexprResult valId

compileVal :: Monad m => Val ValId -> M m CodeGen
compileVal (Val valId body) =
    case body of
    V.BLeaf leaf                -> compileLeaf leaf valId
    V.BApp x                    -> compileApply x    >>= maybeLog
    V.BGetField x               -> compileGetField x >>= maybeLog
    V.BAbs x                    -> compileLambda x valId
    V.BInject x                 -> compileInject x   >>= maybeLog
    V.BRecExtend x              -> compileRecExtend x
    V.BCase x                   -> compileCase x
    V.BFromNom (V.Nom _tId val) -> compileVal val    >>= maybeLog
    V.BToNom x                  -> compileToNom x valId
    where
        maybeLog = maybeLogSubexprResult valId
