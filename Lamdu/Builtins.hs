{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
import qualified Lamdu.Expr.Type as T
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
    toGuest :: Monad m => t -> EvalT pl m (ValHead pl)
    fromGuest :: Monad m => ValHead pl -> EvalT pl m t

instance GuestType Integer where
    toGuest = return . HInteger
    fromGuest (HInteger x) = return x
    fromGuest x = error $ "expected int, got " ++ show (void x)

instance GuestType Bool where
    toGuest b =
        do
            let tag True = Builtins.trueTag
                tag False = Builtins.falseTag
            record [] >>= Eval.asThunk <&> V.Inject (tag b) <&> HInject
    fromGuest v =
        do
            case v of
                HInject (V.Inject boolTag _)
                    | boolTag == Builtins.trueTag -> return True
                    | boolTag == Builtins.falseTag -> return False
                _ -> error $ "expected bool, got " ++ show (void v)

record :: Monad m => [(T.Tag, ThunkId)] -> EvalT pl m (ValHead pl)
record [] = return HRecEmpty
record ((tag, val) : xs) =
    recordThunk xs <&> V.RecExtend tag val <&> HRecExtend

recordThunk :: Monad m => [(Tag, ThunkId)] -> EvalT pl m ThunkId
recordThunk fields = record fields >>= Eval.asThunk

instance GuestType t => GuestType [t] where
    toGuest [] = record [] >>= Eval.asThunk <&> V.Inject "[]" <&> HInject
    toGuest (x:xs) =
        do
            xg <- toGuest x >>= Eval.asThunk
            xsg <- toGuest xs >>= Eval.asThunk
            recordThunk
                [ ("head", xg),
                  ("tail", xsg) ] <&> V.Inject "[]" <&> HInject
    fromGuest (HInject (V.Inject t val))
        | t == "[]" = return []
        | t == ":" =
              do
                  fields <- whnfRecordShape val
                  case (Map.lookup "head" fields, Map.lookup "tail" fields) of
                      (Just hd, Just tl) -> (:) <$> fromGuestThunk hd <*> fromGuestThunk tl
                      _ -> error ": constructor without head/tail in it?!"
    fromGuest x = error $ "Expected list: got " ++ show (void x)

fromGuestThunk :: (Monad m, GuestType t) => ThunkId -> EvalT pl m t
fromGuestThunk t = Eval.whnfThunk t >>= fromGuest

builtin1 ::
    (Monad m, GuestType a, GuestType b) =>
    (a -> b) -> ThunkId -> EvalT pl m (ValHead pl)
builtin1 f thunkId =
    Eval.whnfThunk thunkId >>= fromGuest <&> f >>= toGuest

builtinOr :: Monad m => BuiltinRunner m pl
builtinOr thunkId =
    do
        V2 lThunk rThunk <- extractInfixParams thunkId
        l <- fromGuestThunk lThunk
        if l
            then toGuest True
            else Eval.whnfThunk rThunk

builtinAnd :: Monad m => BuiltinRunner m pl
builtinAnd thunkId =
    do
        V2 lThunk rThunk <- extractInfixParams thunkId
        l <- fromGuestThunk lThunk
        if l
            then Eval.whnfThunk rThunk
            else toGuest False

builtin2Infix ::
    ( Monad m
    , GuestType a
    , GuestType b
    , GuestType c ) =>
    (a -> b -> c) -> ThunkId -> EvalT pl m (ValHead pl)
builtin2Infix f thunkId =
    do
        V2 x y <- extractInfixParams thunkId >>= traverse Eval.whnfThunk
        (f <$> fromGuest x <*> fromGuest y) >>= toGuest

eqThunks :: Monad m => ThunkId -> ThunkId -> EvalT t m Bool
eqThunks aThunk bThunk =
    join $ eq <$> Eval.whnfThunk aThunk <*> Eval.whnfThunk bThunk

eq :: Monad m => ValHead t -> ValHead t -> EvalT pl m Bool
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
        eq x y <&> f >>= toGuest

builtinEq :: Monad m => ThunkId -> EvalT pl m (ValHead pl)
builtinEq = builtinEqH id

builtinNotEq :: Monad m => ThunkId -> EvalT pl m (ValHead pl)
builtinNotEq = builtinEqH not

intArg :: (Integer -> a) -> Integer -> a
intArg = id

eval :: Monad m => Def.FFIName -> ThunkId -> EvalT pl m (ValHead pl)
eval name =
    case name of
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
    Def.FFIName ["Prelude"] "sqrt"   -> builtin1      $ intArg ((floor :: Double -> Integer) . sqrt . fromIntegral)
    _ -> error $ show name ++ " not yet supported"
