{-# LANGUAGE NoImplicitPrelude, LambdaCase, GeneralizedNewtypeDeriving, TemplateHaskell, QuasiQuotes, OverloadedStrings, PolymorphicComponents #-}
-- | Compile Lamdu vals to Javascript

module Lamdu.Compiler.Javascript
    ( Actions(..), M, run
    , CodeGen
    , compileVal
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS (RWST(..))
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.ByteString as BS
import           Data.ByteString.Hex (showHexBytes)
import qualified Data.Char as Char
import           Data.Default () -- instances
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid
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
import           Language.ECMAScript3.Syntax.QuasiQuote (jsexpr)
import qualified Text.PrettyPrint.Leijen as Pretty

import           Prelude.Compat

data Actions m pl = Actions
    { readAssocName :: Guid -> m String
    , readGlobal :: V.Var -> m (Definition.Body (Val pl))
    , output :: String -> m ()
    }

type LocalVarName = JSS.Id ()
type GlobalVarName = JSS.Id ()

data Env m pl = Env
    { envActions :: Actions m pl
    , _envLocals :: Map V.Var LocalVarName
    }
Lens.makeLenses ''Env

data State = State
    { _freshId :: Int
    , _names :: Map String (Map Guid String)
    , _compiled :: Map V.Var GlobalVarName
    }
Lens.makeLenses ''State

newtype M m pl a = M { unM :: RWST (Env m pl) () State m a }
    deriving (Functor, Applicative, Monad)

infixl 4 $.
($.) :: JSS.Expression () -> JSS.Id () -> JSS.Expression ()
($.) = JS.dot

pp :: JSS.Statement () -> String
pp = (`Pretty.displayS`"") . Pretty.renderPretty 1.0 90 . JSPP.prettyPrint

logObj :: JSS.Expression ()
logObj =
    [jsexpr|function (obj) {
                for (var key in obj) console.log(key + " = " + obj[key]);
            }|]
    & void

performAction :: Monad m => (Actions m pl -> m a) -> M m pl a
performAction f = RWS.asks (f . envActions) >>= lift & M

ppOut :: Monad m => JSS.Statement () -> M m pl ()
ppOut stmt = performAction (`output` pp stmt)

-- Multiple vars using a single "var" is badly formatted and generally
-- less readable than a vardecl for each:
varinit :: JSS.Id () -> JSS.Expression () -> JSS.Statement ()
varinit ident expr = JS.vardecls [JS.varinit ident expr]

run :: Monad m => Actions m pl -> M m pl CodeGen -> m ()
run actions act =
    runRWST
    (prelude >> act <&> wrap >>= mapM_ ppOut & unM)
    Env
    { envActions = actions
    , _envLocals = mempty
    }
    State
    { _freshId = 0
    , _names = map (`Map.singleton` fakeGuidMap) ["o", "repl", "logobj"] & mconcat
    , _compiled = mempty
    }
    <&> (^. _1)
    where
        -- We use a fake guid in the Guid->String map just to mark the
        -- built-in names is considered a collision
        fakeGuidMap = Map.singleton (Guid.make "") ""
        prelude =
            JS.vardecls
            [ JS.varinit "o" (JS.var "Object" $. "freeze")
            ] & ppOut
        wrap replExpr =
            [ varinit "repl" $ codeGenExpression replExpr
            , varinit "logobj" logObj
            , JS.var "logobj" `JS.call` [JS.var "repl"] & JS.expr
            , (JS.var "console" $. "log") `JS.call` [JS.var "repl"] & JS.expr
            ]

freshName :: Monad m => String -> M m pl String
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

escapeName :: String -> String
escapeName (d:xs)
    | Char.isDigit d = '_' : d : replaceSpecialChars xs
escapeName xs = replaceSpecialChars xs

replaceSpecialChars :: String -> String
replaceSpecialChars = concatMap replaceSpecial
    where
        replaceSpecial x = Map.lookup x ops & fromMaybe [x]

readName :: (UniqueId.ToGuid a, Monad m) => a -> M m pl String -> M m pl String
readName g act =
    do
        name <-
            performAction (`readAssocName` guid)
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

freshStoredName :: (Monad m, UniqueId.ToGuid a) => a -> String -> M m pl String
freshStoredName g prefix = readName g (freshName prefix)

tagString :: Monad m => T.Tag -> M m pl String
tagString tag@(T.Tag ident) = readName tag ("tag" ++ identHex ident & return)

tagIdent :: Monad m => T.Tag -> M m pl (JSS.Id ())
tagIdent = fmap JS.ident . tagString

withLocalVar :: Monad m => V.Var -> M m pl a -> M m pl (LocalVarName, a)
withLocalVar v (M act) =
    do
        varName <- freshStoredName v "local_" <&> JS.ident
        res <- RWS.local (envLocals . Lens.at v ?~ varName) act & M
        return (varName, res)

identHex :: Identifier -> String
identHex (Identifier bs) = showHexBytes bs

compileGlobal :: Monad m => V.Var -> M m pl (JSS.Expression ())
compileGlobal globalId =
    performAction (`readGlobal` globalId) >>=
    \case
    Definition.BodyBuiltin (Definition.Builtin ffiName _scheme) ->
        ffiCompile ffiName
    Definition.BodyExpr (Definition.Expr val _type _usedDefs) ->
        compileVal val <&> codeGenExpression

getGlobalVar :: Monad m => V.Var -> M m pl GlobalVarName
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

getVar :: Monad m => V.Var -> M m pl (JSS.Id ())
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
    (JSS.Expression () -> M m pl [JSS.Statement ()]) ->
    M m pl (JSS.Expression ())
lam prefix code =
    do
        var <- freshName prefix <&> JS.ident
        code (JS.var var) <&> JS.lambda [var]

infixFunc ::
    Monad m =>
    (JSS.Expression () ->
     JSS.Expression () ->
     M m pl (JSS.Expression ())) ->
    M m pl (JSS.Expression ())
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

ffiCompile :: Monad m => Definition.FFIName -> M m pl (JSS.Expression ())
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

compileRecExtend :: Monad m => V.RecExtend (Val pl) -> M m pl CodeGen
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

compileInject :: Monad m => V.Inject (Val pl) -> M m pl CodeGen
compileInject (V.Inject tag dat) =
    do
        tagStr <- tagString tag <&> JS.string
        dat' <- compileVal dat
        inject tagStr (codeGenExpression dat') & codeGenFromExpr & return

compileCase :: Monad m => V.Case (Val pl) -> M m pl CodeGen
compileCase = fmap codeGenFromExpr . lam "x" . compileCaseOnVar

compileCaseOnVar ::
    Monad m => V.Case (Val pl) -> JSS.Expression () -> M m pl [JSS.Statement ()]
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

compileGetField :: Monad m => V.GetField (Val pl) -> M m pl CodeGen
compileGetField (V.GetField record tag) =
    do
        tagId <- tagIdent tag
        compileVal record
            <&> codeGenExpression <&> (`JS.dot` tagId)
            <&> codeGenFromExpr

compileLeaf :: Monad m => V.Leaf -> M m pl CodeGen
compileLeaf leaf =
    case leaf of
    V.LHole -> throwStr "Reached hole!" & return
    V.LRecEmpty -> object [] & codeGenFromExpr & return
    V.LAbsurd -> throwStr "Reached absurd!" & return
    V.LVar var -> getVar var <&> JS.var <&> codeGenFromExpr
    V.LLiteral literal -> compileLiteral literal & return

compileLambda :: Monad m => V.Lam (Val pl) -> M m pl CodeGen
compileLambda (V.Lam v res) =
    do
        (vId, lamStmts) <- compileVal res <&> codeGenLamStmts & withLocalVar v
        JS.lambda [vId] lamStmts & codeGenFromExpr & return

compileApply :: Monad m => V.Apply (Val pl) -> M m pl CodeGen
compileApply (V.Apply func arg) =
    do
        arg' <- compileVal arg <&> codeGenExpression
        compileAppliedFunc func arg'

compileAppliedFunc :: Monad m => Val pl -> JSS.Expression () -> M m pl CodeGen
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

compileVal :: Monad m => Val pl -> M m pl CodeGen
compileVal (Val _ body) =
    case body of
    V.BLeaf leaf                -> compileLeaf leaf
    V.BApp x                    -> compileApply x
    V.BGetField x               -> compileGetField x
    V.BAbs x                    -> compileLambda x
    V.BInject x                 -> compileInject x
    V.BRecExtend x              -> compileRecExtend x
    V.BCase x                   -> compileCase x
    V.BFromNom (V.Nom _tId val) -> compileVal val
    V.BToNom (V.Nom _tId val)   -> compileVal val
