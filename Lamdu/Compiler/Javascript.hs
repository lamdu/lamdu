{-# LANGUAGE NoImplicitPrelude, LambdaCase, GeneralizedNewtypeDeriving, TemplateHaskell, QuasiQuotes, OverloadedStrings, PolymorphicComponents #-}
-- | Compile Lamdu vals to Javascript

module Lamdu.Compiler.Javascript
    ( Actions(..), M, run
    , CodeGen
    , compileVal
    , ValId(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS.Strict (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as RWS
import qualified Data.ByteString as BS
import           Data.ByteString.Hex (showHexBytes)
import qualified Data.Char as Char
import           Data.Default () -- instances
import           Data.List (intercalate, isPrefixOf)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Store.Guid (Guid)
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Builtins.Literal (Lit(..))
import qualified Lamdu.Builtins.Literal as BuiltinLiteral
import qualified Lamdu.Compiler.Flatten as Flatten
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.Identifier (Identifier(..))
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

newtype ValId = ValId Int

data Actions m = Actions
    { readAssocName :: Guid -> m String
    , readGlobal :: V.Var -> m (Definition.Body (Val ValId))
    , output :: String -> m ()
    }

type LocalVarName = JSS.Id ()
type GlobalVarName = JSS.Id ()

newtype LoggingInfo = LoggingInfo
    { _liScopeDepth :: Int
    } deriving Show
Lens.makeLenses ''LoggingInfo

data Mode = FastSilent | SlowLogging LoggingInfo
    deriving Show

data Env m = Env
    { envActions :: Actions m
    , _envLocals :: Map V.Var LocalVarName
    , _envMode :: Mode
    }
Lens.makeLenses ''Env

data State = State
    { _freshId :: Int
    , _names :: Map String (Map Guid String)
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

pp :: JSS.Statement () -> String
pp = (`Pretty.displayS`"") . Pretty.renderPretty 1.0 90 . JSPP.prettyPrint

performAction :: Monad m => (Actions m -> m a) -> M m a
performAction f = RWS.asks (f . envActions) >>= lift & M

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
    [ JS.var "logResult" `JS.call`
      [ JS.var (scopeIdent depth)
      , JS.var "exprId"
      , JS.var "result"
      ] & JS.returns
    ]

isReservedName :: String -> Bool
isReservedName name =
    name `elem`
    [ "x", "o", "repl", "logobj"
    , "Object", "console", "repl"
    , "logNewScope", "log", "scopeCounter", "logResult", "wrap"
    ]
    || any (`isPrefixOf` name)
    [ "global_"
    , "local_"
    , "scopeId_"
    , "multiply"
    , "plus"
    , "minus"
    , "equals"
    , "greater"
    , "lesser"
    , "percent"
    , "pipe"
    , "dot"
    ]

topLevelDecls :: [JSS.Statement ()]
topLevelDecls =
    ( [ [jsstmt|var o = Object.freeze;|]
      , [jsstmt|var logResult = function (scope, exprId, result) {
                    console.log("Result", scope, exprId, result);
                    return result;
                };|]
      , [jsstmt|var logNewScope = function (parentScope, childScope, lamId, argVal) {
                    console.log("LambdaApplied", parentScope, childScope, lamId, argVal);
                };|]
      , [jsstmt|var scopeId_0 = 0;|]
      , [jsstmt|var scopeCounter = 1;|]
      , [jsstmt|var logobj = function (obj) {
                    for (var key in obj) console.log(key + " = " + obj[key]);
                }|]
      , [jsstmt|var wrap = function (fast, slow) {
                    var count = 0;
                    var callee = function() {
                        count += 1;
                        if (count > 10) {
                            callee = fast;
                            return fast.apply(this, arguments);
                        }
                        return slow.apply(this, arguments);
                    };
                    return function () {
                        return callee.apply(this, arguments);
                    }
                }|]
      ] <&> void
    ) ++
    [ declLog 0 ]

loggingEnabled :: Mode
loggingEnabled = SlowLogging LoggingInfo { _liScopeDepth = 0 }

run :: Monad m => Actions m -> M m CodeGen -> m ()
run actions act =
    runRWST
    (traverse ppOut topLevelDecls
     >> act <&> wrap >>= mapM_ ppOut & unM)
    Env
    { envActions = actions
    , _envLocals = mempty
    , _envMode = loggingEnabled
    }
    State
    { _freshId = 0
    , _names = mempty
    , _compiled = mempty
    }
    <&> (^. _1)
    where
        wrap replExpr =
            [ varinit "repl" $ codeGenExpression replExpr
            , JS.var "logobj" `JS.call` [JS.var "repl"] & JS.expr
            , (JS.var "console" $. "log") `JS.call` [JS.var "repl"] & JS.expr
            ]

-- | Reset reader/writer components of RWS for a new global compilation context
resetRW :: Monad m => M m a -> M m a
resetRW (M act) =
    act
    & RWS.censor (const LogUnused)
    & RWS.local (envLocals .~ mempty)
    & RWS.local (envMode .~ loggingEnabled)
    & M

freshName :: Monad m => String -> M m String
freshName prefix =
    do
        newId <- freshId <+= 1
        prefix ++ show newId & return
    & M

ops :: Map Char String
ops =
    mconcat
    [ '*' ==> "multiply"
    , '+' ==> "plus"
    , '-' ==> "minus"
    , '=' ==> "equals"
    , '>' ==> "greater"
    , '<' ==> "lesser"
    , '%' ==> "percent"
    , '|' ==> "pipe"
    , '.' ==> "dot"
    ]
    where
        (==>) = Map.singleton

avoidReservedNames :: String -> String
avoidReservedNames name
    | isReservedName name = "__" ++ name
    | otherwise = name

escapeName :: String -> String
escapeName (d:xs)
    | Char.isDigit d = '_' : d : replaceSpecialChars xs
escapeName xs = replaceSpecialChars xs

replaceSpecialChars :: String -> String
replaceSpecialChars = concatMap replaceSpecial
    where
        replaceSpecial x = Map.lookup x ops & fromMaybe [x]

readName :: (UniqueId.ToGuid a, Monad m) => a -> M m String -> M m String
readName g act =
    do
        name <-
            performAction (`readAssocName` guid)
            <&> avoidReservedNames
            <&> escapeName
            >>= \case
                "" -> act
                name -> return name
        names . Lens.at name %%= \case
            Nothing -> (name, Just (Map.singleton guid name))
            Just guidMap ->
                guidMap
                & Lens.at guid %%~
                \case
                Nothing -> (newName, Just newName)
                    where
                        newName = name ++ show (Map.size guidMap)
                Just oldName -> (oldName, Just oldName)
                <&> Just
            & M
    where
        guid = UniqueId.toGuid g

freshStoredName :: (Monad m, UniqueId.ToGuid a) => a -> String -> M m String
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

identHex :: Identifier -> String
identHex (Identifier bs) = showHexBytes bs

compileGlobal :: Monad m => V.Var -> M m (JSS.Expression ())
compileGlobal globalId =
    performAction (`readGlobal` globalId) >>=
    \case
    Definition.BodyBuiltin (Definition.Builtin ffiName _scheme) ->
        ffiCompile ffiName
    Definition.BodyExpr (Definition.Expr val _type _usedDefs) ->
        compileVal val <&> codeGenExpression
    & resetRW

getGlobalVar :: Monad m => V.Var -> M m GlobalVarName
getGlobalVar var =
    Lens.use (compiled . Lens.at var) & M
    >>= maybe newGlobal return
    where
        newGlobal =
            do
                varName <- freshStoredName var "global_" <&> JS.ident
                compiled . Lens.at var ?= varName & M
                compileGlobal var
                    <&> varinit varName
                    >>= ppOut
                return varName

getVar :: Monad m => V.Var -> M m (JSS.Id ())
getVar v =
    Lens.view (envLocals . Lens.at v) & M
    >>= maybe (getGlobalVar v) return

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

infixFunc ::
    Monad m =>
    (JSS.Expression () ->
     JSS.Expression () ->
     M m (JSS.Expression ())) ->
    M m (JSS.Expression ())
infixFunc f =
    do
        lStr <- tagIdent Builtins.infixlTag
        rStr <- tagIdent Builtins.infixrTag
        lam "i" $ \args ->
            (args $. lStr) `f` (args $. rStr)
            <&> JS.returns
            <&> (: [])

object :: [(JSS.Prop (), JSS.Expression ())] -> JSS.Expression ()
object =
    freeze . JS.object
    where
        freeze expr = JS.var "o" `JS.call` [expr]

nullaryInject :: JSS.Expression () -> JSS.Expression ()
nullaryInject tagStr = object [(JS.propId "tag", tagStr)]

inject :: JSS.Expression () -> JSS.Expression () -> JSS.Expression ()
inject tagStr dat' =
    object
    [ (JS.propId "tag", tagStr)
    , (JS.propId "data", dat')
    ]

jsBoolToSum :: String -> String -> JSS.Expression () -> JSS.Expression ()
jsBoolToSum trueTag falseTag bool =
    nullaryInject
    -- TODO: Use the true tag ids for False/True, not assume
    -- they're named False/True
    (JS.cond bool (JS.string trueTag) (JS.string falseTag))

unknownFfiFunc :: Definition.FFIName -> JSS.Expression ()
unknownFfiFunc (Definition.FFIName modulePath name) =
    JS.lambda []
    [ "Unsupported ffi function " ++ intercalate "." (modulePath ++ [name])
        & JS.string & JS.throw
    ]

ffiCompile :: Monad m => Definition.FFIName -> M m (JSS.Expression ())
ffiCompile ffiName@(Definition.FFIName ["Prelude"] opStr) =
    do
        trueTag <- tagString Builtins.trueTag
        falseTag <- tagString Builtins.falseTag
        let infixBool f = opFunc (\x y -> f x y & jsBoolToSum trueTag falseTag)
        case opStr of
            "*" -> opFunc JS.mul
            "+" -> opFunc JS.add
            "-" -> opFunc JS.sub
            "mod" -> opFunc JS.mod
            "==" -> infixBool JS.steq
            ">=" -> infixBool JS.ge
            ">" -> infixBool JS.gt
            "<=" -> infixBool JS.le
            "<" -> infixBool JS.lt
            _ -> unknownFfiFunc ffiName & return
    where
        opFunc op = infixFunc (\x y -> return (op x y))
ffiCompile ffiName = unknownFfiFunc ffiName & return

compileLiteral :: V.Literal -> CodeGen
compileLiteral literal =
    case BuiltinLiteral.toLit literal of
    LitBytes bytes ->
        CodeGen
        { codeGenLamStmts = stmts
        , codeGenExpression = unitRedex stmts
        }
        where
            stmts =
                [ varinit "arr" $
                  JS.new (JS.var "Uint8Array") [JS.int (BS.length bytes)]
                , JS.call (JS.var "arr" $. "set") [JS.array ints] & JS.expr
                , JS.var "arr" & JS.returns
                ]
            ints = [JS.int (fromIntegral byte) | byte <- BS.unpack bytes]
    LitFloat num -> JS.number num & codeGenFromExpr

compileRecExtend :: Monad m => V.RecExtend (Val ValId) -> M m CodeGen
compileRecExtend x =
    do
        Flatten.Composite tags mRest <- Flatten.recExtend x & Lens.traverse compileVal
        strTags <-
            Map.toList tags
            <&> _2 %~ codeGenExpression
            & Lens.traversed . _1 %%~ tagString
        let obj = strTags <&> _1 %~ JS.propId . JS.ident & object
        case mRest of
            Nothing -> codeGenFromExpr obj
            Just rest ->
                CodeGen
                { codeGenLamStmts = stmts
                , codeGenExpression = unitRedex stmts
                }
                where
                    stmts =
                        varinit "rest"
                        ((JS.var "Object" $. "create") `JS.call` [codeGenExpression rest])
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

compileLeaf :: Monad m => V.Leaf -> M m CodeGen
compileLeaf leaf =
    case leaf of
    V.LHole -> throwStr "Reached hole!" & return
    V.LRecEmpty -> object [] & codeGenFromExpr & return
    V.LAbsurd -> throwStr "Reached absurd!" & return
    V.LVar var -> getVar var <&> JS.var <&> codeGenFromExpr
    V.LLiteral literal -> compileLiteral literal & return

declMyScopeDepth :: Int -> JSS.Statement ()
declMyScopeDepth depth =
    varinit (scopeIdent depth) $
    JS.uassign JSS.PostfixInc "scopeCounter"

callLogNewScope :: Int -> Int -> ValId -> JSS.Expression () -> JSS.Statement ()
callLogNewScope parentDepth myDepth (ValId lamValId) argVal =
    JS.var "logNewScope" `JS.call`
    [ JS.var (scopeIdent parentDepth)
    , JS.var (scopeIdent myDepth)
    , JS.int lamValId
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
                JS.var "wrap" `JS.call`
                    [fastLam, JS.lambda [varName] (stmts ++ lamStmts)] & return
            where
                parentScopeDepth = loggingInfo ^. liScopeDepth
    <&> codeGenFromExpr
    where
        mkLambda (varId, lamStmts) = JS.lambda [varId] lamStmts
        compileRes = compileVal res <&> codeGenLamStmts & withLocalVar v

compileApply :: Monad m => V.Apply (Val ValId) -> ValId -> M m CodeGen
compileApply (V.Apply func arg) valId =
    do
        arg' <- compileVal arg <&> codeGenExpression
        compileAppliedFunc func arg'
    >>= maybeLogSubexprResult valId

maybeLogSubexprResult :: Monad m => ValId -> CodeGen -> M m CodeGen
maybeLogSubexprResult valId codeGen =
    Lens.view envMode & M
    >>= \case
    FastSilent -> return codeGen
    SlowLogging _ -> logSubexprResult valId codeGen

logSubexprResult :: Monad m => ValId -> CodeGen -> M m CodeGen
logSubexprResult (ValId valId) codeGen =
    do
        RWS.tell LogUsed & M
        JS.var "log" `JS.call` [JS.int valId, codeGenExpression codeGen]
            & codeGenFromExpr
            & return

compileAppliedFunc :: Monad m => Val ValId -> JSS.Expression () -> M m CodeGen
compileAppliedFunc func arg' =
    case func ^. V.body of
    V.BCase case_ ->
        compileCaseOnVar case_ (JS.var "x")
        <&> (varinit "x" arg' :)
        <&> codeGenFromLamStmts
    V.BAbs (V.Lam v res) ->
        do
            (vId, lamStmts) <- compileVal res <&> codeGenLamStmts & withLocalVar v
            return CodeGen
                { codeGenLamStmts = varinit vId arg' : lamStmts
                , codeGenExpression =
                    -- Can't really optimize a redex in expr
                    -- context, as at least 1 redex must be paid
                    JS.lambda [vId] lamStmts `JS.call` [arg']
                }
    _ ->
        do
            func' <- compileVal func <&> codeGenExpression
            func' `JS.call` [arg'] & codeGenFromExpr & return

compileVal :: Monad m => Val ValId -> M m CodeGen
compileVal (Val valId body) =
    case body of
    V.BLeaf leaf                -> compileLeaf leaf
    V.BApp x                    -> compileApply x valId
    V.BGetField x               -> compileGetField x
    V.BAbs x                    -> compileLambda x valId
    V.BInject x                 -> compileInject x
    V.BRecExtend x              -> compileRecExtend x
    V.BCase x                   -> compileCase x
    V.BFromNom (V.Nom _tId val) -> compileVal val
    V.BToNom (V.Nom _tId val)   -> compileVal val
