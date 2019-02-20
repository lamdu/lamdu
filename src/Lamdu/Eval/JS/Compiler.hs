{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, QuasiQuotes, PolymorphicComponents #-}
-- | Compile Lamdu vals to Javascript

module Lamdu.Eval.JS.Compiler
    ( Actions(..)
    , ValId(..)
    , compileRepl, Mode(..), loggingEnabled
    ) where

import           AST (Tree)
import           AST.Knot.Ann (Ann(..), val)
import           AST.Term.Nominal (ToNom(..))
import           AST.Term.Row (RowExtend(..))
import qualified Control.Lens as Lens
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.State (MonadState)
import           Control.Monad.Trans.FastRWS (RWST, runRWST)
import           Control.Monad.Writer (MonadWriter(..), censor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.Char as Char
import           Data.Default () -- instances
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Identifier (identHex)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Term.Utils as Flatten
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Scheme (Scheme)
import qualified Lamdu.Calc.Type.Scheme as Scheme
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (WhichGlobal(..), encodeWhichGlobal)
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import qualified Language.ECMAScript3.PrettyPrint as JSPP
import qualified Language.ECMAScript3.Syntax as JSS
import qualified Language.ECMAScript3.Syntax.CodeGen as JS
import           Language.ECMAScript3.Syntax.QuasiQuote (jsstmt)
import           Numeric.Lens (hex)
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import           Lamdu.Prelude

newtype ValId = ValId UUID

data Mode = FastSilent | SlowLogging LoggingInfo
    deriving Show

data Actions m = Actions
    { readAssocName :: T.Tag -> m Text
    , readAssocTag :: UUID -> m T.Tag
    , readGlobal :: V.Var -> m (Definition.Definition (Val ValId) ())
    , readGlobalType :: V.Var -> m Scheme
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
    , _envExpectedTypes :: Map V.Var Scheme
    , _envCurrentGlobal :: WhichGlobal
    }
Lens.makeLenses ''Env

data NameKind
    = VarName -- name of Javascript variable (never exposed outside)
    | TagName -- name of record field or inject-tag identifier
    deriving (Eq, Ord, Show)

data State = State
    { _freshId :: Int
    , _names :: Map (NameKind, Text) (Map UUID Text)
    , _globalVarNames :: Map V.Var GlobalVarName
    , _globalTypes :: Map V.Var Scheme
    }
Lens.makeLenses ''State

data LogUsed
    = LogUnused
    | LogUsed
    deriving (Eq, Ord, Show)
instance Semigroup LogUsed where
    LogUsed <> _ = LogUsed
    _ <> LogUsed = LogUsed
    _ <> _ = LogUnused
instance Monoid LogUsed where
    mempty = LogUnused
    mappend = (<>)

newtype M m a = M { unM :: RWST (Env m) LogUsed State m a }
    deriving
        ( Functor, Applicative, Monad
        , MonadReader (Env m)
        , MonadWriter LogUsed
        , MonadState State
        )

infixl 4 $.
($.) :: JSS.Expression () -> JSS.Id () -> JSS.Expression ()
($.) = JS.dot

infixl 3 $$
($$) :: JSS.Expression () -> JSS.Expression () -> JSS.Expression ()
f $$ x = f `JS.call` [x]

pp :: JSS.Statement () -> String
pp = (`Pretty.displayS`"") . Pretty.renderPretty 1.0 90 . JSPP.prettyPrint

performAction :: Monad m => (Actions m -> m a) -> M m a
performAction f = Lens.view envActions <&> f >>= lift & M

ppOut :: Monad m => JSS.Statement () -> M m ()
ppOut stmt = performAction (`output` pp stmt)

-- Multiple vars using a single "var" is badly formatted and generally
-- less readable than a vardecl for each:
varinit :: JSS.Id () -> JSS.Expression () -> JSS.Statement ()
varinit ident expr = JS.vardecls [JS.varinit ident expr]

scopeIdent :: Int -> JSS.Id ()
scopeIdent depth = "scopeId_" ++ show depth & JS.ident

rts :: JSS.Id () -> JSS.Expression ()
rts = (JS.var "rts" $.)

declLog :: Int -> JSS.Statement ()
declLog depth =
    varinit "log" $
    JS.lambda ["exprId", "result"]
    [ rts "logResult" `JS.call`
      [ JS.var (scopeIdent depth)
      , JS.var "exprId"
      , JS.var "result"
      ] & JS.returns
    ]

-- | Taken from http://www.ecma-international.org/ecma-262/6.0/#sec-keywords
jsReservedKeywords :: Set Text
jsReservedKeywords =
    Set.fromList
    [ "break"    , "do"        , "in"        , "typeof"
    , "case"     , "else"      , "instanceof", "var"
    , "catch"    , "export"    , "new"       , "void"
    , "class"    , "extends"   , "return"    , "while"
    , "const"    , "finally"   , "super"     , "with"
    , "continue" , "for"       , "switch"    , "yield"
    , "debugger" , "function"  , "this"      , "default"
    , "if"       , "throw"     , "delete"    , "import"
    , "try"      , "let"       , "static"    , "enum"
    , "await"    , "implements", "package"   , "protected"
    , "interface", "private"   , "public"
    ]

jsReservedNamespace :: Set Text
jsReservedNamespace =
    Set.fromList
    [ "x", "repl"
    , "Object", "console", "repl"
    , "log", "scopeCounter", "rts"
    , "tag", "data", "array", "bytes", "func", "cacheId", "number"
    ]

jsAllReserved :: Set Text
jsAllReserved = jsReservedNamespace <> jsReservedKeywords

isReservedName :: Text -> Bool
isReservedName name =
    jsAllReserved ^. Lens.contains name
    || any (`Text.isPrefixOf` name)
    [ "global_"
    , "local_"
    , "scopeId_"
    ]

topLevelDecls :: Mode -> [JSS.Statement ()]
topLevelDecls mode =
    ( [ [jsstmt|"use strict";|]
      , [jsstmt|var rts = require('rts.js');|]
      ] <&> void
    ) ++
    case mode of
    FastSilent -> []
    SlowLogging{} ->
        ( [ [jsstmt|var scopeId_0 = 0;|]
          , [jsstmt|var scopeCounter = 1;|]
          ] <&> void
        ) ++
        [ declLog 0
        ]

loggingEnabled :: Mode
loggingEnabled = SlowLogging LoggingInfo { _liScopeDepth = 0 }

compileRepl :: Monad m => Actions m -> Definition.Expr (Val ValId) -> m ()
compileRepl actions defExpr =
    runRWST
    ( traverse_ ppOut (topLevelDecls (loggingMode actions))
        >> compileDefExpr defExpr
        <&> codeGenExpression <&> scaffold >>= traverse_ ppOut & unM
    ) (initialEnv actions) initialState <&> (^. _1)

-- | Top-level wrapepr for the code (catch exceptions in repl
-- execution, log it, and export the module symbols)
scaffold :: JSS.Expression () -> [JSS.Statement ()]
scaffold replExpr =
    [ JS.trycatch
        ( JS.block
            [ varinit "repl" replExpr
            , void [jsstmt|rts.logRepl(repl);|]
            ]
        )
        ( JS.catch "err"
            (JS.block [ void [jsstmt|rts.logReplErr(err);|] ])
        )
        Nothing
    , -- This form avoids outputing repl's value in interactive mode
        void [jsstmt|(function() { module.exports = repl; })();|]
    ]

initialEnv :: Actions m -> Env m
initialEnv actions =
    Env
    { _envActions = actions
    , _envLocals = mempty
    , _envMode = loggingMode actions
    , _envExpectedTypes = mempty
    , _envCurrentGlobal = GlobalRepl
    }

initialState :: State
initialState =
    State
    { _freshId = 0
    , _names = mempty
    , _globalVarNames = mempty
    , _globalTypes = mempty
    }

-- | Reset reader/writer components of RWS for a new global compilation context
withGlobal :: Monad m => WhichGlobal -> M m a -> M m a
withGlobal whichGlobal act =
    act
    & censor (const LogUnused)
    & local (envLocals .~ mempty)
    & local (\x -> x & envMode .~ loggingMode (x ^. envActions))
    & local (envCurrentGlobal .~ whichGlobal)

freshName :: Monad m => Text -> M m Text
freshName prefix =
    freshId <+= 1
    <&> show
    <&> Text.pack
    <&> (prefix <>)
    & M

avoidReservedNames :: Text -> Text
avoidReservedNames name
    | isReservedName name = "_" <> name
    | otherwise = name

escapeName :: Text -> Text
escapeName name =
    case Text.unpack name of
    (d:xs) | Char.isDigit d -> '_' : d : replaceSpecialChars xs
    xs -> replaceSpecialChars xs
    & Text.pack

replaceSpecialChars :: String -> String
replaceSpecialChars = concatMap replaceSpecial
    where
        replaceSpecial x
            | Char.isAlphaNum x = [x]
            | x == '_' = "__"
            | otherwise = '_' : ((hex #) . Char.ord) x ++ "_"

readName :: (UniqueId.ToUUID a, Monad m) => a -> M m Text -> M m Text
readName g act =
    do
        tag <- performAction (`readAssocTag` uuid)
        (if tag == anonTag then act else readTagName tag act)
            >>= generatedName VarName uuid
    where
        uuid = UniqueId.toUUID g

generatedName :: Monad m => NameKind -> UUID -> Text -> M m Text
generatedName kind uuid name =
    names . Lens.at (kind, name) %%=
    \case
    Nothing -> (name, Just (Map.singleton uuid name))
    Just uuidMap ->
        uuidMap
        & Lens.at uuid %%~
        \case
        Nothing -> (newName, Just newName)
            where
                newName = name <> Text.pack (show (Map.size uuidMap))
        Just oldName -> (oldName, Just oldName)
        <&> Just

readTagName :: Monad m => T.Tag -> M m Text -> M m Text
readTagName tag act =
    performAction (`readAssocName` tag)
    <&> avoidReservedNames
    <&> escapeName
    >>=
    \case
    "" -> act
    name -> pure name

freshStoredName :: (Monad m, UniqueId.ToUUID a) => a -> Text -> M m Text
freshStoredName g prefix = readName g (freshName prefix)

tagString :: Monad m => T.Tag -> M m Text
tagString tag@(T.Tag ident) =
    "tag" ++ identHex ident & Text.pack & pure
    & readTagName tag
    >>= generatedName TagName (UniqueId.toUUID tag)

tagIdent :: Monad m => T.Tag -> M m (JSS.Id ())
tagIdent = fmap (JS.ident . Text.unpack) . tagString

withLocalVar :: Monad m => V.Var -> M m a -> M m (LocalVarName, a)
withLocalVar v act =
    do
        varName <- freshStoredName v "local_" <&> Text.unpack <&> JS.ident
        res <- local (envLocals . Lens.at v ?~ varName) act
        pure (varName, res)

compileDefExpr :: Monad m => Definition.Expr (Val ValId) -> M m CodeGen
compileDefExpr (Definition.Expr x frozenDeps) =
    compileVal x & local (envExpectedTypes .~ frozenDeps ^. Infer.depsGlobalTypes)

compileGlobal :: Monad m => V.Var -> M m CodeGen
compileGlobal globalId =
    do
        def <- performAction (`readGlobal` globalId)
        globalTypes . Lens.at globalId ?= def ^. Definition.defType
        case def ^. Definition.defBody of
            Definition.BodyBuiltin ffiName -> ffiCompile ffiName & codeGenFromExpr & pure
            Definition.BodyExpr defExpr -> compileDefExpr defExpr
    & withGlobal (GlobalDef globalId)

throwErr :: Monad m => ValId -> String -> String -> M m CodeGen
throwErr valId errName desc =
    Lens.view envCurrentGlobal
    <&> \curGlobal ->
    [ (rts "exceptions" $. JS.ident errName)
        `JS.call`
        [ JS.string desc
        , JS.string (encodeWhichGlobal curGlobal)
        , jsValId valId
        ] & JS.throw
    ] & codeGenFromLamStmts

compileGlobalVar :: Monad m => ValId -> V.Var -> M m CodeGen
compileGlobalVar valId var =
    Lens.view (envExpectedTypes . Lens.at var)
    >>= maybe loadGlobal verifyType
    where
        loadGlobal =
            Lens.use (globalVarNames . Lens.at var)
            >>= maybe newGlobal pure
            <&> useGlobal
            <&> codeGenFromExpr
        useGlobal varName = JS.var varName `JS.call` []
        newGlobal =
            do
                varName <- freshStoredName var "global_" <&> Text.unpack <&> JS.ident
                globalVarNames . Lens.at var ?= varName
                compileGlobal var
                    <&> codeGenLamStmts
                    <&> JS.lambda []
                    <&> (: [])
                    <&> (rts "memo" `JS.call`)
                    <&> varinit varName
                    >>= ppOut
                pure varName
        verifyType expectedType =
            do
                scheme <-
                    Lens.use (globalTypes . Lens.at var)
                    >>= maybe newGlobalType pure
                if Scheme.alphaEq scheme expectedType
                    then loadGlobal
                    else throwErr valId "BrokenDef" "Dependency type needs update"
        newGlobalType =
            do
                scheme <- performAction (`readGlobalType` var)
                globalTypes . Lens.at var ?= scheme
                pure scheme

compileLocalVar :: JSS.Id () -> CodeGen
compileLocalVar = codeGenFromExpr . JS.var

compileVar :: Monad m => ValId -> V.Var -> M m CodeGen
compileVar valId v =
    Lens.view (envLocals . Lens.at v)
    >>= maybe (compileGlobalVar valId v) (pure . compileLocalVar)

data CodeGen = CodeGen
    { codeGenLamStmts :: [JSS.Statement ()]
    , codeGenExpression :: JSS.Expression ()
    }

unitRedex :: [JSS.Statement ()] -> JSS.Expression ()
unitRedex stmts = JS.lambda [] stmts `JS.call` []

codeGenFromLamStmts :: [JSS.Statement ()] -> CodeGen
codeGenFromLamStmts stmts =
    CodeGen
    { codeGenLamStmts = stmts
    , codeGenExpression = unitRedex stmts
    }

codeGenFromExpr :: JSS.Expression () -> CodeGen
codeGenFromExpr expr =
    CodeGen
    { codeGenLamStmts = [JS.returns expr]
    , codeGenExpression = expr
    }

lam ::
    Monad m => Text ->
    (JSS.Expression () -> M m [JSS.Statement ()]) ->
    M m (JSS.Expression ())
lam prefix code =
    do
        var <- freshName prefix <&> Text.unpack <&> JS.ident
        code (JS.var var) <&> JS.lambda [var]

inject :: JSS.Expression () -> JSS.Expression () -> JSS.Expression ()
inject tagStr dat' =
    JS.object
    [ (JS.propId "tag", tagStr)
    , (JS.propId "data", dat')
    ]

ffiCompile :: Definition.FFIName -> JSS.Expression ()
ffiCompile (Definition.FFIName modul funcName) =
    foldl ($.) (rts "builtins") (modul <&> Text.unpack <&> JS.ident)
    `JS.brack` JS.string (Text.unpack funcName)

compileLiteral :: V.PrimVal -> CodeGen
compileLiteral literal =
    case PrimVal.toKnown literal of
    PrimVal.Bytes bytes ->
        rts "bytes" $$ JS.array ints & codeGenFromExpr
        where
            ints = [JS.int (fromIntegral byte) | byte <- BS.unpack bytes]
    PrimVal.Float num -> JS.number num & codeGenFromExpr

compileRecExtend :: Monad m => Tree (RowExtend T.Tag V.Term V.Term) (Ann ValId) -> M m CodeGen
compileRecExtend x =
    do
        Flatten.Composite tags mRest <- Flatten.recExtend x & Lens.traverse compileVal
        extends <-
            Map.toList tags
            <&> _2 %~ codeGenExpression
            & Lens.traversed . _1 %%~ tagString
            <&> Lens.mapped . _1 %~ JS.propId . JS.ident . Text.unpack
            <&> JS.object
        case mRest of
            Nothing -> codeGenFromExpr extends
            Just rest ->
                codeGenFromLamStmts
                [ varinit "x"
                    ((JS.var "Object" $. "assign") `JS.call` [extends, codeGenExpression rest])
                , JS.expr (JS.delete (JS.var "x" $. "cacheId"))
                , JS.returns (JS.var "x")
                ]
            & pure

compileInject :: Monad m => V.Inject (Val ValId) -> M m CodeGen
compileInject (V.Inject tag dat) =
    do
        tagStr <- tagString tag <&> Text.unpack <&> JS.string
        dat' <- compileVal dat
        inject tagStr (codeGenExpression dat') & codeGenFromExpr & pure

compileCase :: Monad m => ValId -> Tree (RowExtend T.Tag V.Term V.Term) (Ann ValId) -> M m CodeGen
compileCase valId = fmap codeGenFromExpr . lam "x" . compileCaseOnVar valId

compileCaseOnVar ::
    Monad m => ValId -> Tree (RowExtend T.Tag V.Term V.Term) (Ann ValId) -> JSS.Expression () -> M m [JSS.Statement ()]
compileCaseOnVar valId x scrutineeVar =
    do
        tagsStr <- Map.toList tags & Lens.traverse . _1 %%~ tagString
        cases <- traverse makeCase tagsStr
        defaultCase <-
            case mRestHandler of
            Nothing -> throwErr valId "LamduBug" "Unhandled case"
            Just restHandler -> compileAppliedFunc valId restHandler scrutineeVar
            <&> codeGenLamStmts
            <&> JS.defaultc
        pure [JS.switch (scrutineeVar $. "tag") (cases ++ [defaultCase])]
    where
        Flatten.Composite tags mRestHandler = Flatten.case_ x
        makeCase (tagStr, handler) =
            compileAppliedFunc valId handler (scrutineeVar $. "data")
            <&> codeGenLamStmts
            <&> JS.casee (JS.string (Text.unpack tagStr))

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
jsValId (ValId uuid) = (JS.string . Text.unpack . decodeUtf8 . Hex.encode . UUIDUtils.toSBS16) uuid

callLogNewScope :: Int -> Int -> ValId -> JSS.Expression () -> JSS.Statement ()
callLogNewScope parentDepth myDepth lamValId argVal =
    rts "logNewScope" `JS.call`
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
listenNoTellLogUsed = censor (const LogUnused) . listen

compileLambda :: Monad m => Tree (V.Lam V.Var V.Term) (Ann ValId) -> ValId -> M m CodeGen
compileLambda (V.Lam v res) valId =
    Lens.view envMode
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
                rts "wrap" `JS.call`
                    [fastLam, JS.lambda [varName] (stmts ++ lamStmts)] & pure
            where
                parentScopeDepth = loggingInfo ^. liScopeDepth
    <&> optimizeExpr
    <&> codeGenFromExpr
    where
        mkLambda (varId, lamStmts) = JS.lambda [varId] lamStmts
        compileRes = compileVal res <&> codeGenLamStmts & withLocalVar v

compileApply ::
    Monad m => ValId -> Tree (V.Apply V.Term) (Ann ValId) -> M m CodeGen
compileApply valId (V.Apply func arg) =
    do
        arg' <- compileVal arg <&> codeGenExpression
        compileAppliedFunc valId func arg'

maybeLogSubexprResult :: Monad m => ValId -> CodeGen -> M m CodeGen
maybeLogSubexprResult valId codeGen =
    Lens.view envMode
    >>= \case
    FastSilent -> pure codeGen
    SlowLogging _ -> logSubexprResult valId codeGen

logSubexprResult :: Monad m => ValId -> CodeGen -> M m CodeGen
logSubexprResult valId codeGen =
    codeGenFromExpr
    (JS.var "log" `JS.call` [jsValId valId, codeGenExpression codeGen])
    <$ tell LogUsed

compileAppliedFunc :: Monad m => ValId -> Val ValId -> JSS.Expression () -> M m CodeGen
compileAppliedFunc valId func arg' =
    do
        mode <- Lens.view envMode
        case (func ^. val, mode) of
            (V.BCase case_, FastSilent) ->
                compileCaseOnVar valId case_ (JS.var "x")
                <&> (varinit "x" arg' :)
                <&> codeGenFromLamStmts
            (V.BLam (V.Lam v res), FastSilent) ->
                compileVal res <&> codeGenLamStmts & withLocalVar v
                <&> \(vId, lamStmts) ->
                CodeGen
                { codeGenLamStmts = varinit vId arg' : lamStmts
                , codeGenExpression =
                    -- Can't really optimize a redex in expr
                    -- context, as at least 1 redex must be paid
                    JS.lambda [vId] lamStmts $$ arg'
                }
            _ ->
                compileVal func
                <&> codeGenExpression
                <&> ($$ arg')
                <&> optimizeExpr
                <&> codeGenFromExpr

optimizeExpr :: JSS.Expression () -> JSS.Expression ()
optimizeExpr x@(JSS.CallExpr () func [arg])
    | func == def "toArray" =
        arrayLit arg
        & maybe x (JSS.ArrayLit ())
    | func == def "map" =
        -- Check mapping with "id" (when unwrapping nominals..)
        case arg of
        JSS.ObjectLit ()
            [ (_, str)
            , (k, JSS.FuncExpr () Nothing [lamParam] [JSS.ReturnStmt () (Just (JSS.VarRef () lamRet))])
            ] | k == key "mapping" && lamParam == lamRet
            -> str
        _ -> x
    where
        arrayLit (JSS.CallExpr () cons [JSS.ObjectLit ()
            [(k0, v0), (k1, JSS.FuncExpr () Nothing [_] [JSS.ReturnStmt () (Just v1)])]
            ])
            | cons == def "_3a__3a_" && k0 == key "infixl" && k1 == key "infixr" =
                arrayLit v1 <&> (v0 :)
            | otherwise = Nothing
        arrayLit (JSS.ObjectLit () [(k0, JSS.StringLit () "empty"), (k1, JSS.ObjectLit () [])])
            | k0 == key "tag" && k1 == key "data" =
                Just []
        arrayLit _ = Nothing
        def g = JSS.CallExpr () (JSS.VarRef () (JSS.Id () g)) []
        key n = JSS.PropId () (JSS.Id () n)
optimizeExpr (JSS.FuncExpr () Nothing [param] [JSS.ReturnStmt () (Just (JSS.CallExpr () func [JSS.VarRef () var]))])
    | param == var = func
optimizeExpr x = x

compileLeaf :: Monad m => V.Leaf -> ValId -> M m CodeGen
compileLeaf x valId =
    case x of
    V.LHole -> throwErr valId "ReachedHole" "Reached a hole"
    V.LRecEmpty -> JS.object [] & codeGenFromExpr & pure
    V.LAbsurd -> throwErr valId "LamduBug" "Reached absurd"
    V.LVar var -> compileVar valId var >>= maybeLogSubexprResult valId
    V.LLiteral literal -> compileLiteral literal & pure
    V.LFromNom {} ->
        lam "x" (pure . (:[]) . JSS.ReturnStmt () . Just) <&> codeGenFromExpr

compileToNom ::
    Monad m =>
    Tree (ToNom T.NominalId V.Term) (Ann ValId) -> ValId -> M m CodeGen
compileToNom (ToNom tId x) valId =
    case x ^? ExprLens.valLiteral <&> PrimVal.toKnown of
    Just (PrimVal.Bytes bytes)
        | tId == Builtins.textTid
        && all (< 128) (BS.unpack bytes) ->
            -- The JS is more readable with string constants
            rts "bytesFromAscii" $$ JS.string (Text.unpack (decodeUtf8 bytes))
            & codeGenFromExpr & pure
    _ -> compileVal x >>= maybeLogSubexprResult valId

compileVal :: Monad m => Val ValId -> M m CodeGen
compileVal (Ann valId body) =
    case body of
    V.BLeaf x                   -> compileLeaf x valId
    V.BApp x                    -> compileApply valId x    >>= maybeLog
    V.BGetField x               -> compileGetField x >>= maybeLog
    V.BLam x                    -> compileLambda x valId
    V.BInject x                 -> compileInject x   >>= maybeLog
    V.BRecExtend x              -> compileRecExtend x
    V.BCase x                   -> compileCase valId x
    V.BToNom x                  -> compileToNom x valId
    where
        maybeLog = maybeLogSubexprResult valId
