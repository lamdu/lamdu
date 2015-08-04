{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Builtins
    ( eval
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.Monad (void)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Utils (matchKeys)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (EvalResult(..))
import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

flatRecord :: EvalResult pl -> Map Tag (EvalResult pl)
flatRecord HRecEmpty = Map.empty
flatRecord (HRecExtend (V.RecExtend t v rest)) = flatRecord rest & Map.insert t v
flatRecord _ = error "Param record is not a record"

extractRecordParams ::
    (Traversable t, Show (t Tag)) => t Tag -> EvalResult pl -> t (EvalResult pl)
extractRecordParams expectedTags val =
    case matchKeys expectedTags paramsMap of
    Nothing ->
        error $ concat
        [ "Param list mismatches expected params: "
        , show expectedTags, " vs. ", show (Map.keys paramsMap)
        ]
    Just x -> x
    where
        paramsMap = flatRecord val

data V2 a = V2 a a   deriving (Show, Functor, Foldable, Traversable)
data V3 a = V3 a a a deriving (Show, Functor, Foldable, Traversable)

extractInfixParams :: EvalResult pl -> V2 (EvalResult pl)
extractInfixParams = extractRecordParams (V2 Builtins.infixlTag Builtins.infixrTag)

class GuestType t where
    toGuest :: t -> EvalResult pl
    fromGuest :: EvalResult pl -> t

instance GuestType Integer where
    toGuest = HInteger
    fromGuest (HInteger x) = x
    fromGuest x = error $ "expected int, got " ++ show (void x)

instance GuestType Bool where
    toGuest b =
        record [] & V.Inject (tag b) & HInject
        where
            tag True = Builtins.trueTag
            tag False = Builtins.falseTag
    fromGuest v =
        case v of
        HInject (V.Inject boolTag _)
            | boolTag == Builtins.trueTag -> True
            | boolTag == Builtins.falseTag -> False
        _ -> error $ "expected bool, got " ++ show (void v)

record :: [(T.Tag, EvalResult pl)] -> EvalResult pl
record [] = HRecEmpty
record ((tag, val) : xs) = record xs & V.RecExtend tag val & HRecExtend

instance GuestType t => GuestType [t] where
    toGuest [] = record [] & V.Inject "[]" & HInject
    toGuest (x:xs) =
        record
        [ ("head", toGuest x)
        , ("tail", toGuest xs)
        ] & V.Inject "[]" & HInject
    fromGuest (HInject (V.Inject t val))
        | t == "[]" = []
        | t == ":" =
            case (Map.lookup "head" fields, Map.lookup "tail" fields) of
            (Just hd, Just tl) -> fromGuest hd : fromGuest tl
            _ -> error ": constructor without head/tail in it?!"
        where
            fields = flatRecord val
    fromGuest x = error $ "Expected list: got " ++ show (void x)

builtin1 :: (GuestType a, GuestType b) => (a -> b) -> EvalResult pl -> EvalResult pl
builtin1 f val = fromGuest val & f & toGuest

builtin2Infix ::
    ( GuestType a
    , GuestType b
    , GuestType c ) =>
    (a -> b -> c) -> EvalResult pl -> EvalResult pl
builtin2Infix f thunkId =
    f (fromGuest x) (fromGuest y) & toGuest
    where
        V2 x y = extractInfixParams thunkId

eq :: EvalResult t -> EvalResult t -> Bool
eq HFunc {} _ = error "Cannot compare functions"
eq HAbsurd {} _ = error "Cannot compare case analysis"
eq HCase {} _ = error "Cannot compare case analysis"
eq HBuiltin {} _ = error "Cannot compare builtins"
eq (HInteger x) (HInteger y) = x == y
eq (HRecExtend x) (HRecExtend y) =
    Map.keys fx == Map.keys fy &&
    (Map.intersectionWith eq fx fy & Map.elems & and)
    where
        fx = flatRecord $ HRecExtend x
        fy = flatRecord $ HRecExtend y
eq HRecEmpty HRecEmpty = True
eq (HInject (V.Inject xf xv)) (HInject (V.Inject yf yv))
    | xf == yf = eq xv yv
    | otherwise = False
eq _ _ = False -- assume type checking ruled out errorenous equalities already

builtinEqH :: GuestType t => (Bool -> t) -> EvalResult pl -> EvalResult pl
builtinEqH f val =
    eq x y & f & toGuest
    where
        V2 x y = extractInfixParams val

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
