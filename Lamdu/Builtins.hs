{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Builtins
    ( eval
    ) where

import           Control.Lens.Operators
import           Control.Monad (join, void, when)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Utils (matchKeys)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (EvalResult, Val(..), EvalError(..))
import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

flatRecord :: EvalResult pl -> Either EvalError (Map Tag (EvalResult pl))
flatRecord (Left err) = Left err
flatRecord (Right HRecEmpty) = Right Map.empty
flatRecord (Right (HRecExtend (V.RecExtend t v rest))) =
    flatRecord rest <&> Map.insert t v
flatRecord _ = error "Param record is not a record"

extractRecordParams ::
    (Traversable t, Show (t Tag)) =>
    t Tag -> EvalResult pl -> Either EvalError (t (EvalResult pl))
extractRecordParams expectedTags val =
    do
        paramsMap <- flatRecord val
        case matchKeys expectedTags paramsMap of
            Nothing ->
                "Builtin expected params: " ++ show expectedTags ++ " got: " ++
                show (void val) & EvalTypeError & Left
            Just x -> Right x

data V2 a = V2 a a   deriving (Show, Functor, Foldable, Traversable)
data V3 a = V3 a a a deriving (Show, Functor, Foldable, Traversable)

extractInfixParams :: EvalResult pl -> Either EvalError (V2 (EvalResult pl))
extractInfixParams =
        extractRecordParams (V2 Builtins.infixlTag Builtins.infixrTag)

class GuestType t where
    toGuestVal :: t -> Val pl
    fromGuestVal :: Val pl -> Either EvalError t

toGuest :: GuestType t => t -> EvalResult pl
toGuest = Right . toGuestVal

fromGuest :: GuestType t => EvalResult pl -> Either EvalError t
fromGuest = (>>= fromGuestVal)

instance GuestType Integer where
    toGuestVal = HInteger
    fromGuestVal (HInteger x) = Right x
    fromGuestVal x = error $ "expected int, got " ++ show (void x)

instance GuestType Bool where
    toGuestVal b =
        record [] & V.Inject (tag b) & HInject
        where
            tag True = Builtins.trueTag
            tag False = Builtins.falseTag
    fromGuestVal v =
        case v of
        HInject (V.Inject boolTag _)
            | boolTag == Builtins.trueTag -> Right True
            | boolTag == Builtins.falseTag -> Right False
        _ -> "Expected bool, got: " ++ show (void v) & EvalTypeError & Left

record :: [(T.Tag, EvalResult pl)] -> EvalResult pl
record [] = Right HRecEmpty
record ((tag, val) : xs) =
    record xs & V.RecExtend tag val & HRecExtend & Right

builtin1 :: (GuestType a, GuestType b) => (a -> b) -> EvalResult pl -> EvalResult pl
builtin1 f val = fromGuest val <&> f >>= toGuest

builtin2Infix ::
    ( GuestType a
    , GuestType b
    , GuestType c ) =>
    (a -> b -> c) -> EvalResult pl -> EvalResult pl
builtin2Infix f thunkId =
    do
        V2 x y <- extractInfixParams thunkId
        f <$> fromGuest x <*> fromGuest y >>= toGuest

eq :: EvalResult t -> EvalResult t -> Either EvalError Bool
eq x y = eqVal <$> x <*> y & join

eqVal :: Val t -> Val t -> Either EvalError Bool
eqVal HFunc {} _    = EvalTodoError "Eq of func" & Left
eqVal HAbsurd {} _  = EvalTodoError "Eq of absurd" & Left
eqVal HCase {} _    = EvalTodoError "Eq of case" & Left
eqVal HBuiltin {} _ = EvalTodoError "Eq of builtin" & Left
eqVal (HInteger x) (HInteger y) = x == y & Right
eqVal (HRecExtend x) (HRecExtend y) =
    do
        fx <- HRecExtend x & Right & flatRecord
        fy <- HRecExtend y & Right & flatRecord
        when (Map.keysSet fx /= Map.keysSet fy) $
            "Comparing different record types: " ++
            show (Map.keys fx) ++ " vs. " ++
            show (Map.keys fy)
            & EvalTypeError & Left
        Map.intersectionWith eq fx fy
            & Map.elems & sequence <&> and
eqVal HRecEmpty HRecEmpty = Right True
eqVal (HInject (V.Inject xf xv)) (HInject (V.Inject yf yv))
    | xf == yf = eq xv yv
    | otherwise = Right False
eqVal _ _ = Right False -- assume type checking ruled out errorenous equalities already

builtinEqH :: GuestType t => (Bool -> t) -> EvalResult pl -> EvalResult pl
builtinEqH f val =
    do
        V2 x y <- extractInfixParams val
        eq x y <&> f >>= toGuest

builtinEq :: EvalResult pl -> EvalResult pl
builtinEq = builtinEqH id

builtinNotEq :: EvalResult pl -> EvalResult pl
builtinNotEq = builtinEqH not

intArg :: (Integer -> a) -> Integer -> a
intArg = id

eval :: Def.FFIName -> EvalResult pl -> EvalResult pl
eval name =
    case name of
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
