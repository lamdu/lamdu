{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, TemplateHaskell, QuasiQuotes, OverloadedStrings, PolymorphicComponents #-}
-- | Compile Lamdu vals to Javascript

module Lamdu.Compiler.Javascript
    {-( Actions(..), M, run
    , compile
    )-} where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS (RWST(..))
import qualified Control.Monad.Trans.RWS as RWS
import           Control.MonadA (MonadA)
import qualified Data.ByteString as BS
import           Data.ByteString.Hex (showHexBytes)
import qualified Data.Char as Char
import           Data.Default () -- instances
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Store.Guid as Guid
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Builtins.Literal (Lit(..))
import qualified Lamdu.Builtins.Literal as BuiltinLiteral
import qualified Lamdu.Compiler.Flatten as Flatten
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (ValI(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Identifier (Identifier(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Language.ECMAScript3.PrettyPrint as JSPP
import qualified Language.ECMAScript3.Syntax as JSS
import qualified Language.ECMAScript3.Syntax.CodeGen as JS
import           Language.ECMAScript3.Syntax.QuasiQuote (jsexpr)

import           Prelude.Compat

type T = Transaction

data Actions m = Actions
    { runTransaction :: forall a. T m a -> IO a
    , output :: String -> IO ()
    }

type LocalVarName = String
type GlobalVarName = String

data Env m = Env
    { envActions :: Actions m
    , _envLocals :: Map V.Var LocalVarName
    }
Lens.makeLenses ''Env

data State = State
    { _freshId :: Int
    , _compiled :: Map V.Var GlobalVarName
    }
Lens.makeLenses ''State

newtype M m a = M (RWST (Env m) () State IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

infixl 4 $.
($.) :: JSS.Expression () -> JSS.Id () -> JSS.Expression ()
($.) = JS.dot

pp :: JSS.Statement () -> String
pp = show . JSPP.prettyPrint

logObj :: JSS.Expression ()
logObj =
    [jsexpr|function (obj) {
                for (var key in obj) console.log(key + " = " + obj[key]);
            }|]
    & void

ppOut :: MonadIO m => Actions n -> JSS.Statement () -> m ()
ppOut actions = liftIO . output actions . pp

run :: Actions m -> M m (JSS.Expression ()) -> IO ()
run actions (M act) =
    do
        replExpr <- runRWST act (Env actions mempty) (State 0 mempty) <&> (^. _1)
        JS.block
            [ [ JS.varinit "repl" replExpr
              , JS.varinit "logobj" logObj
              ] & JS.vardecls
            , JS.var "logobj" `JS.call` [JS.var "repl"] & JS.expr
            , (JS.var "console" $. "log") `JS.call` [JS.var "repl"] & JS.expr
            ] & ppOut actions

trans :: T m b -> M m b
trans act =
    do
        runTrans <- RWS.asks (runTransaction . envActions)
        runTrans act & lift
    & M

getName :: String -> M m String
getName prefix =
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

escapeName :: String -> String
escapeName (d:xs)
    | Char.isDigit d = '_' : d : replaceSpecialChars xs
escapeName xs = replaceSpecialChars xs

replaceSpecialChars :: String -> String
replaceSpecialChars = concatMap replaceSpecial
    where
        replaceSpecial x = Map.lookup x ops & fromMaybe [x]

readName :: (UniqueId.ToGuid a, Monad m) => a -> M m String
readName g =
    Anchors.assocNameRef g & Transaction.getP & trans
    <&> escapeName
    <&> (++ "_" ++ hexDisamb)
    where
        guid = UniqueId.toGuid g
        hexDisamb = Guid.bs guid & showHexBytes & take 4

getStoredName :: (Monad m, UniqueId.ToGuid a) => a -> String -> M m String
getStoredName g prefix =
    do
        name <- readName g
        case name of
            "" -> getName prefix
            _ -> return name

tagString :: Monad m => T.Tag -> M m String
tagString tag@(T.Tag ident) =
    do
        name <- readName tag
        case name of
            "" -> "tag" ++ identHex ident & return
            _ -> return name

tagIdent :: Monad m => T.Tag -> M m (JSS.Id ())
tagIdent = fmap JS.ident . tagString

withLocalVar :: Monad m => V.Var -> M m a -> M m (LocalVarName, a)
withLocalVar v (M act) =
    do
        varName <- getStoredName v "local_"
        res <- RWS.local (envLocals . Lens.at v ?~ varName) act & M
        return (varName, res)

-- | Compile a given val and all the transitively used definitions
-- (FIXME: currently, compilation goes to stdout)
compileValI :: MonadA m => ValI m -> M m (JSS.Expression ())
compileValI valI = ExprIRef.readVal valI & trans >>= compileVal

identHex :: Identifier -> String
identHex (Identifier bs) = showHexBytes bs

compileGlobal :: Monad m => V.Var -> M m (JSS.Expression ())
compileGlobal v =
    do
        defBody <- Transaction.readIRef defI & trans
        case defBody of
            Definition.BodyBuiltin (Definition.Builtin ffiName _scheme) ->
                ffiCompile ffiName
            Definition.BodyExpr (Definition.Expr valI _type _usedDefs) ->
                compileValI valI
    where
        defI = ExprIRef.defI v

getGlobalVar :: Monad m => V.Var -> M m GlobalVarName
getGlobalVar var =
    Lens.use (compiled . Lens.at var) & M
    >>= maybe newGlobal return
    where
        newGlobal =
            do
                varName <- getStoredName var "global_"
                actions <- RWS.asks envActions & M
                compiled . Lens.at var ?= varName & M
                compileGlobal var
                    <&> JS.varinit (JS.ident varName)
                    <&> JS.vardecls . (:[])
                    >>= ppOut actions
                return varName

getVar :: Monad m => V.Var -> M m (JSS.Id ())
getVar v =
    Lens.view (envLocals . Lens.at v) & M
    >>= maybe (getGlobalVar v) return
    <&> JS.ident

-- | A statement as an expression that evaluates to undefined
stmtsExpressionUndefined :: [JSS.Statement ()] -> JSS.Expression ()
stmtsExpressionUndefined stmts =
    JS.call (JS.lambda [] stmts) []

throwStr :: String -> JSS.Expression ()
throwStr str = [JS.throw (JS.string str)] & stmtsExpressionUndefined

lam ::
    String -> (JSS.Expression () -> M m [JSS.Statement ()]) ->
    M m (JSS.Expression ())
lam prefix stmts =
    do
        var <- getName prefix <&> JS.ident
        stmts (JS.var var) <&> JS.lambda [var]

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

jsUndefined :: JSS.Expression ()
jsUndefined = JS.var "undefined"

inject :: JSS.Expression () -> JSS.Expression () -> JSS.Expression ()
inject tagStr dat' =
    JS.object
    [ (JS.propId "tag", tagStr)
    , (JS.propId "data", dat')
    ]

jsBoolToSum :: String -> String -> JSS.Expression () -> JSS.Expression ()
jsBoolToSum trueTag falseTag bool =
    inject
    -- TODO: Use the true tag ids for False/True, not assume
    -- they're named False/True
    (JS.cond bool (JS.string trueTag) (JS.string falseTag))
    jsUndefined

ffiCompile :: Monad m => Definition.FFIName -> M m (JSS.Expression ())
ffiCompile (Definition.FFIName ["Prelude"] opStr) =
    do
        trueTag <- tagString Builtins.trueTag
        falseTag <- tagString Builtins.falseTag
        let infixBool f x y = f x y & jsBoolToSum trueTag falseTag
        let op =
                case opStr of
                "*" -> JS.mul
                "+" -> JS.add
                "-" -> JS.sub
                "mod" -> JS.mod
                "==" -> infixBool JS.steq
                ">=" -> infixBool JS.ge
                ">" -> infixBool JS.gt
                "<=" -> infixBool JS.le
                "<" -> infixBool JS.lt
                _ -> "Unknown Prelude function " ++ opStr & error
        infixFunc (\x y -> return (op x y))

ffiCompile (Definition.FFIName modulePath name) =
    intercalate "." (modulePath ++ [name]) ++ " not implemented"
    & error

stmtsExpression :: [JSS.Statement ()] -> JSS.Expression () -> JSS.Expression ()
stmtsExpression stmts expr =
    JS.call (JS.lambda [] (stmts ++ [JS.returns expr])) []

compileLiteral :: V.Literal -> JSS.Expression ()
compileLiteral literal =
    case BuiltinLiteral.toLit literal of
    LitBytes bytes ->
        stmtsExpression
        [ JS.vardecls
          [ JS.varinit "arr"
            (JS.new (JS.var "Uint8Array") [JS.int (BS.length bytes)])
          ]
        , JS.call (JS.var "arr" $. "set") [JS.array ints] & JS.expr
        ] (JS.var "arr")
        where
            ints = [JS.int (fromIntegral byte) | byte <- BS.unpack bytes]
    LitFloat num -> JS.number num

freeze :: JSS.Expression () -> JSS.Expression ()
freeze expr = JS.call (JS.var "Object" $. "freeze") [expr]

compileFlatRecExtend :: Monad m => Flatten.Record (JSS.Expression ()) -> M m (JSS.Expression ())
compileFlatRecExtend (Flatten.Composite tags mRest) =
    do
        strTags <- Map.toList tags & Lens.traversed . _1 %%~ tagString
        let obj = strTags <&> _1 %~ JS.propId . JS.ident & JS.object
        case mRest of
            Nothing -> freeze obj
            Just rest ->
                stmtsExpression (restDecl : restAssignments) (freeze (JS.var "rest"))
                where
                    restDecl =
                        JS.vardecls
                        [ JS.varinit "rest"
                          ((JS.var "Object" $. "create") `JS.call` [rest])
                        ]
                    restAssignments =
                        strTags
                        <&> _1 %~ JS.ldot (JS.var "rest")
                        <&> uncurry JS.assign
                        <&> JS.expr
            & return
    where

compileInject :: Monad m => V.Inject (Val (ValI m)) -> M m (JSS.Expression ())
compileInject (V.Inject tag dat) =
    do
        tagStr <- tagString tag <&> JS.string
        dat' <- compileVal dat
        inject tagStr dat' & return

compileFlatCase ::
    Monad m =>
    Flatten.Case (JSS.Expression ()) -> JSS.Expression () ->
    M m (JSS.Statement ())
compileFlatCase (Flatten.Composite tags mRestHandler) scrutinee =
    do
        tagsStr <- Map.toList tags & Lens.traverse . _1 %%~ tagString
        [ JS.casee (JS.string tagStr)
              [ handler `JS.call` [scrutinee $. "data"] & JS.returns ]
            | (tagStr, handler) <- tagsStr
            ] ++
            [ JS.defaultc
              [ case mRestHandler of
                Nothing ->
                    JS.throw (JS.string "Unhandled case? This is a type error!")
                Just restHandler ->
                    restHandler `JS.call` [scrutinee $. "data"] & JS.returns
              ]
            ]
            & JS.switch (scrutinee $. "tag")
            & return

compileGetField :: Monad m => V.GetField (Val (ValI m)) -> M m (JSS.Expression ())
compileGetField (V.GetField record tag) =
    do
        tagId <- tagIdent tag
        compileVal record <&> (`JS.dot` tagId)

compileLeaf :: Monad m => V.Leaf -> M m (JSS.Expression ())
compileLeaf leaf =
    case leaf of
    V.LHole -> throwStr "Reached hole!" & return
    V.LRecEmpty -> JS.object [] & freeze & return
    V.LAbsurd -> throwStr "Reached absurd!" & return
    V.LVar var -> getVar var <&> JS.var
    V.LLiteral literal -> compileLiteral literal & return

compileLambda :: Monad m => V.Lam (Val (ValI m)) -> M m (JSS.Expression ())
compileLambda (V.Lam v res) =
    do
        (vId, res') <- compileVal res & withLocalVar v
        JS.lambda [JS.ident vId] [JS.returns res'] & return

compileApply :: Monad m => V.Apply (Val (ValI m)) -> M m (JSS.Expression ())
compileApply (V.Apply func arg) =
    JS.call <$> compileVal func <*> sequence [compileVal arg]

compileRecExtend :: Monad m => V.RecExtend (Val (ValI m)) -> M m (JSS.Expression ())
compileRecExtend x =
    Flatten.recExtend x & Lens.traverse compileVal >>= compileFlatRecExtend

compileCase :: Monad m => V.Case (Val (ValI m)) -> M m (JSS.Expression ())
compileCase x =
    do
        caseJs <- Flatten.case_ x & Lens.traverse compileVal
        compileFlatCase caseJs <&> fmap (:[]) & lam "x"

compileVal :: Monad m => Val (ValI m) -> M m (JSS.Expression ())
compileVal (Val _ body) =
    case body of
    V.BLeaf leaf -> compileLeaf leaf
    V.BApp x -> compileApply x
    V.BGetField x -> compileGetField x
    V.BAbs x -> compileLambda x
    V.BInject x -> compileInject x
    V.BRecExtend x -> compileRecExtend x
    V.BCase x -> compileCase x
    V.BFromNom (V.Nom _tId val) -> compileVal val
    V.BToNom (V.Nom _tId val) -> compileVal val
