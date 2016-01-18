{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Builtins
    ( eval
    ) where

import           Control.Lens.Operators
import           Control.Monad (when)
import           Data.Binary.Utils (encodeS, decodeS)
import qualified Data.ByteString as SBS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Utils (matchKeys)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Eval.Val (Val(..), EvalError(..))
import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

flatRecord :: Val srcId -> Either EvalError (Map Tag (Val srcId))
flatRecord (HError err) = Left err
flatRecord HRecEmpty = Right Map.empty
flatRecord (HRecExtend (V.RecExtend t v rest)) =
    flatRecord rest <&> Map.insert t v
flatRecord _ = "Param record is not a record" & EvalTypeError & Left

extractRecordParams ::
    (Traversable t, Show (t Tag)) =>
    t Tag -> Val srcId -> Either EvalError (t (Val srcId))
extractRecordParams expectedTags val =
    do
        paramsMap <- flatRecord val
        case matchKeys expectedTags paramsMap of
            Nothing ->
                "Builtin expected params: " ++ show expectedTags ++ " got: " ++
                show val & EvalTypeError & Left
            Just x -> Right x

data V2 a = V2 a a   deriving (Show, Functor, Foldable, Traversable)
data V3 a = V3 a a a deriving (Show, Functor, Foldable, Traversable)

extractInfixParams :: Val srcId -> Either EvalError (V2 (Val srcId))
extractInfixParams =
    extractRecordParams (V2 Builtins.infixlTag Builtins.infixrTag)

class GuestType t where
    toGuest :: t -> Val srcId
    fromGuestVal :: Val srcId -> Either EvalError t

fromGuest :: GuestType t => Val srcId -> Either EvalError t
fromGuest (HError err) = Left err
fromGuest x = fromGuestVal x

instance GuestType SBS.ByteString where
    toGuest = HLiteral . V.Literal Builtins.bytesId
    fromGuestVal (HLiteral (V.Literal primId x))
        | primId == Builtins.bytesId = Right x
    fromGuestVal x = "expected bytes, got " ++ show x & EvalTypeError & Left

instance GuestType Double where
    toGuest = HLiteral . V.Literal Builtins.floatId . encodeS
    fromGuestVal (HLiteral (V.Literal primId x))
        | primId == Builtins.floatId = Right (decodeS x)
    fromGuestVal x = "expected num, got " ++ show x & EvalTypeError & Left

instance GuestType Bool where
    toGuest b =
        record [] & V.Inject (tag b) & HInject
        where
            tag True = Builtins.trueTag
            tag False = Builtins.falseTag
    fromGuestVal v =
        case v of
        HInject (V.Inject boolTag _)
            | boolTag == Builtins.trueTag -> Right True
            | boolTag == Builtins.falseTag -> Right False
        _ -> "Expected bool, got: " ++ show v & EvalTypeError & Left

record :: [(T.Tag, Val srcId)] -> Val srcId
record [] = HRecEmpty
record ((tag, val) : xs) = record xs & V.RecExtend tag val & HRecExtend

eitherToVal :: Either EvalError (Val srcId) -> Val srcId
eitherToVal (Left err) = HError err
eitherToVal (Right x) = x

valToEither :: Val srcId -> Either EvalError (Val srcId)
valToEither (HError err) = Left err
valToEither x = Right x

builtin1 :: (GuestType a, GuestType b) => (a -> b) -> Val srcId -> Val srcId
builtin1 f val = fromGuest val <&> f >>= valToEither . toGuest & eitherToVal

builtin2Infix ::
    ( GuestType a
    , GuestType b
    , GuestType c ) =>
    (a -> b -> c) -> Val srcId -> Val srcId
builtin2Infix f thunkId =
    do
        V2 x y <- extractInfixParams thunkId
        f <$> fromGuest x <*> fromGuest y >>= valToEither . toGuest
    & eitherToVal

eq :: Val t -> Val t -> Either EvalError Bool
eq HFunc {} _    = EvalTodoError "Eq of func" & Left
eq HAbsurd {} _  = EvalTodoError "Eq of absurd" & Left
eq HCase {} _    = EvalTodoError "Eq of case" & Left
eq HBuiltin {} _ = EvalTodoError "Eq of builtin" & Left
eq (HLiteral (V.Literal xTId x)) (HLiteral (V.Literal yTId y))
    | xTId == yTId = Right (x == y)
    | otherwise = EvalTypeError "Comparison of different literal types!" & Left
eq (HRecExtend x) (HRecExtend y) =
    do
        fx <- HRecExtend x & flatRecord
        fy <- HRecExtend y & flatRecord
        when (Map.keysSet fx /= Map.keysSet fy) $
            "Comparing different record types: " ++
            show (Map.keys fx) ++ " vs. " ++
            show (Map.keys fy)
            & EvalTypeError & Left
        Map.intersectionWith eq fx fy
            & Map.elems & sequence <&> and
eq HRecEmpty HRecEmpty = Right True
eq (HInject (V.Inject xf xv)) (HInject (V.Inject yf yv))
    | xf == yf = eq xv yv
    | otherwise = Right False
eq (HError err) _ = Left err
eq _ (HError err) = Left err
eq _ _ = Right False -- assume type checking ruled out errorenous equalities already

builtinEqH :: GuestType t => (Bool -> t) -> Val srcId -> Val srcId
builtinEqH f val =
    do
        V2 x y <- extractInfixParams val
        eq x y <&> f >>= valToEither . toGuest
    & eitherToVal

builtinEq :: Val srcId -> Val srcId
builtinEq = builtinEqH id

builtinNotEq :: Val srcId -> Val srcId
builtinNotEq = builtinEqH not

toIndex :: Int -> Val srcId -> Either EvalError Int
toIndex mx g =
    fromGuest g >>= f
    where
        f x
            | x /= fromIntegral i = "fractional index " ++ show x & err
            | i < 0 = "negative index " ++ show i & err
            | i > mx = "index too large " ++ show i ++ " >= " ++ show mx & err
            | otherwise = Right i
            where
                i = floatArg truncate x
        err = Left . EvalIndexError

builtinBytesSlice :: Val srcId -> Val srcId
builtinBytesSlice val =
    do
        V3 bytesG startG stopG <-
            extractRecordParams
            (V3 Builtins.objTag Builtins.startTag Builtins.stopTag) val
        bytes <- fromGuest bytesG
        V2 start stop <- V2 startG stopG & mapM (toIndex (SBS.length bytes))
        when (start > stop)
            (Left (EvalIndexError
            ("start index " ++ show start ++ " after stop index " ++ show stop)
            ))
        SBS.take stop bytes & SBS.drop start & toGuest & Right
    & eitherToVal

floatArg :: (Double -> a) -> Double -> a
floatArg = id

genericDiv :: (RealFrac a, Integral b) => a -> a -> b
genericDiv n d = n / d & floor

genericMod :: RealFrac a => a -> a -> a
genericMod n d = n - d * fromIntegral (genericDiv n d :: Int)

eval :: Def.FFIName -> Val srcId -> Val srcId
eval name =
    case name of
    Def.FFIName ["Prelude"] "=="     -> builtinEq
    Def.FFIName ["Prelude"] "/="     -> builtinNotEq
    Def.FFIName ["Prelude"] "<"      -> builtin2Infix $ floatArg (<)
    Def.FFIName ["Prelude"] "<="     -> builtin2Infix $ floatArg (<=)
    Def.FFIName ["Prelude"] ">"      -> builtin2Infix $ floatArg (>)
    Def.FFIName ["Prelude"] ">="     -> builtin2Infix $ floatArg (>=)
    Def.FFIName ["Prelude"] "*"      -> builtin2Infix $ floatArg (*)
    Def.FFIName ["Prelude"] "+"      -> builtin2Infix $ floatArg (+)
    Def.FFIName ["Prelude"] "-"      -> builtin2Infix $ floatArg (-)
    Def.FFIName ["Prelude"] "/"      -> builtin2Infix $ floatArg (/)
    Def.FFIName ["Prelude"] "div"    -> builtin2Infix $ ((fromIntegral :: Int -> Double) .) . floatArg genericDiv
    Def.FFIName ["Prelude"] "mod"    -> builtin2Infix $ floatArg genericMod
    Def.FFIName ["Prelude"] "negate" -> builtin1      $ floatArg negate
    Def.FFIName ["Prelude"] "sqrt"   -> builtin1      $ floatArg sqrt
    Def.FFIName ["Bytes"]   "slice"  -> builtinBytesSlice
    _ -> name & EvalMissingBuiltin & HError & const
