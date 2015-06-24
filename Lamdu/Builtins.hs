{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Builtins
    ( eval
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens.Operators
import           Control.Monad (void, join)
import           Data.Foldable (Foldable(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Utils (matchKeys)
import           Data.Traversable (Traversable(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval (EvalT)
import qualified Lamdu.Eval as Eval
import           Lamdu.Eval.Val (ValBody(..), ValHead, ThunkId)
import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Val as V

forceRecExtend :: Monad m => V.RecExtend ThunkId -> EvalT pl m (Map Tag ThunkId)
forceRecExtend (V.RecExtend tag val rest) =
    whnfRecordShape rest
    <&> Map.insert tag val

whnfRecordShape :: Monad m => ThunkId -> EvalT pl m (Map Tag ThunkId)
whnfRecordShape thunkId =
    do
        recordHead <- Eval.whnfThunk thunkId
        case recordHead of
            HRecEmpty -> return Map.empty
            HRecExtend recExtend ->
                forceRecExtend recExtend
            _ -> error "Param record is not a record"

extractRecordParams ::
    (Monad m, Traversable t, Show (t Tag)) => t Tag -> ThunkId -> EvalT pl m (t ThunkId)
extractRecordParams expectedTags thunkId =
    do
        paramsMap <- whnfRecordShape thunkId
        case matchKeys expectedTags paramsMap of
            Nothing ->
                error $ concat
                [ "Param list mismatches expected params: "
                , show expectedTags, " vs. ", show (Map.keys paramsMap)
                ]
            Just paramThunks -> return paramThunks

data V2 a = V2 a a   deriving (Show, Functor, Foldable, Traversable)
data V3 a = V3 a a a deriving (Show, Functor, Foldable, Traversable)

extractInfixParams :: Monad m => ThunkId -> EvalT pl m (V2 ThunkId)
extractInfixParams = extractRecordParams (V2 Builtins.infixlTag Builtins.infixrTag)

type BuiltinRunner m pl = ThunkId -> EvalT pl m (ValHead pl)

class GuestType t where
    toGuest :: t -> ValHead pl
    fromGuest :: ValHead pl -> t

instance GuestType Integer where
    toGuest = HInteger
    fromGuest (HInteger x) = x
    fromGuest x = error $ "expected int, got " ++ show (void x)

instance GuestType Bool where
    toGuest = HBuiltin . Def.FFIName [] . show
    fromGuest (HBuiltin (Def.FFIName [] x)) = read x
    fromGuest x = error $ "expected bool, got " ++ show (void x)

builtinIf :: Monad m => BuiltinRunner m pl
builtinIf thunkId =
    do
        V3 obj then_ else_ <-
            extractRecordParams
            (V3 Builtins.objTag Builtins.thenTag Builtins.elseTag) thunkId
        condition <- Eval.whnfThunk obj
        ( if fromGuest condition
          then then_
          else else_
          ) & Eval.whnfThunk

builtin1 ::
    (Monad m, GuestType a, GuestType b) =>
    (a -> b) -> ThunkId -> EvalT pl m (ValHead pl)
builtin1 f thunkId =
    Eval.whnfThunk thunkId <&> fromGuest <&> f <&> toGuest

builtinOr :: Monad m => BuiltinRunner m pl
builtinOr thunkId =
    do
        V2 lThunk rThunk <- extractInfixParams thunkId
        l <- Eval.whnfThunk lThunk
        if fromGuest l
            then return $ toGuest True
            else Eval.whnfThunk rThunk

builtinAnd :: Monad m => BuiltinRunner m pl
builtinAnd thunkId =
    do
        V2 lThunk rThunk <- extractInfixParams thunkId
        l <- Eval.whnfThunk lThunk
        if fromGuest l
            then Eval.whnfThunk rThunk
            else return $ toGuest False

builtin2Infix ::
    ( Monad m
    , GuestType a
    , GuestType b
    , GuestType c ) =>
    (a -> b -> c) -> ThunkId -> EvalT pl m (ValHead pl)
builtin2Infix f thunkId =
    do
        V2 x y <- extractInfixParams thunkId >>= traverse Eval.whnfThunk
        f (fromGuest x) (fromGuest y) & toGuest & return

eqThunks :: Monad m => ThunkId -> ThunkId -> EvalT t m Bool
eqThunks aThunk bThunk =
    join $ eq <$> Eval.whnfThunk aThunk <*> Eval.whnfThunk bThunk

eq :: Monad m => ValBody ThunkId t -> ValBody ThunkId t -> EvalT pl m Bool
eq HFunc {} _ = error "Cannot compare functions"
eq HAbsurd {} _ = error "Cannot compare case analysis"
eq HCase {} _ = error "Cannot compare case analysis"
eq HBuiltin {} _ = error "Cannot compare builtins"
eq (HInteger x) (HInteger y) = return $ x == y
eq (HRecExtend x) (HRecExtend y) =
    do
        xm <- forceRecExtend x
        ym <- forceRecExtend y
        if Map.keys xm == Map.keys ym
            then Map.intersectionWith eqThunks xm ym & Map.elems & sequenceA <&> and
            else return False
eq HRecEmpty HRecEmpty = return True
eq (HInject (V.Inject xf xv)) (HInject (V.Inject yf yv))
    | xf == yf = eqThunks xv yv
    | otherwise = return False
eq _ _ = return False -- assume type checking ruled out errorenous equalities already

builtinEqH :: (Monad m, GuestType t) => (Bool -> t) -> ThunkId -> EvalT pl m (ValHead pl)
builtinEqH f thunkId =
    do
        V2 x y <- extractInfixParams thunkId >>= traverse Eval.whnfThunk
        eq x y <&> f <&> toGuest

builtinEq :: Monad m => ThunkId -> EvalT pl m (ValHead pl)
builtinEq = builtinEqH id

builtinNotEq :: Monad m => ThunkId -> EvalT pl m (ValHead pl)
builtinNotEq = builtinEqH not

intArg :: (Integer -> a) -> Integer -> a
intArg = id

eval :: Monad m => Def.FFIName -> ThunkId -> EvalT pl m (ValHead pl)
eval name =
    case name of
    Def.FFIName ["Prelude"] "if"     -> builtinIf
    Def.FFIName ["Prelude"] "||"     -> builtinOr
    Def.FFIName ["Prelude"] "&&"     -> builtinAnd

    Def.FFIName ["Prelude"] "=="     -> builtinEq
    Def.FFIName ["Prelude"] "/="     -> builtinNotEq
    Def.FFIName ["Prelude"] "<"      -> builtin2Infix $ intArg (<)
    Def.FFIName ["Prelude"] "<="     -> builtin2Infix $ intArg (<=)
    Def.FFIName ["Prelude"] ">"      -> builtin2Infix $ intArg (>)
    Def.FFIName ["Prelude"] ">="     -> builtin2Infix $ intArg (>=)
    Def.FFIName ["Prelude"] "*"      -> builtin2Infix $ intArg (*)
    Def.FFIName ["Prelude"] "+"      -> builtin2Infix $ intArg (+)
    Def.FFIName ["Prelude"] "-"      -> builtin2Infix $ intArg (-)
    Def.FFIName ["Prelude"] "div"    -> builtin2Infix $ intArg div
    Def.FFIName ["Prelude"] "mod"    -> builtin2Infix $ intArg mod
    Def.FFIName ["Prelude"] "negate" -> builtin1      $ intArg negate
    _ -> error $ show name ++ " not yet supported"
