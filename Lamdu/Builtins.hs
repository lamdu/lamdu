{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Builtins
    ( eval
    ) where

import           Control.Lens.Operators
import           Control.Monad (void)
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

mapOfParamRecord :: Monad m => ThunkId -> EvalT pl m (Map Tag ThunkId)
mapOfParamRecord thunkId =
    do
        recordHead <- Eval.whnfThunk thunkId
        case recordHead of
            HRecEmpty -> return Map.empty
            HRecExtend (V.RecExtend tag val rest) ->
                mapOfParamRecord rest
                <&> Map.insert tag val
            _ -> error "Param record is not a record"

extractRecordParams ::
    (Monad m, Traversable t, Show (t Tag)) => t Tag -> ThunkId -> EvalT pl m (t ThunkId)
extractRecordParams expectedTags thunkId =
    do
        paramsMap <- mapOfParamRecord thunkId
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
    toGuest = HBuiltin . Def.FFIName ["Prelude"] . show
    fromGuest (HBuiltin (Def.FFIName ["Prelude"] x)) = read x
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

builtinNegate :: Monad m => BuiltinRunner m pl
builtinNegate thunkId =
    Eval.whnfThunk thunkId <&> fromGuest <&> intNegate <&> toGuest
    where
        intNegate :: Integer -> Integer
        intNegate = negate

builtinOr :: Monad m => BuiltinRunner m pl
builtinOr thunkId =
    do
        V2 lThunk rThunk <- extractInfixParams thunkId
        l <- Eval.whnfThunk lThunk
        if fromGuest l
            then return $ toGuest True
            else Eval.whnfThunk rThunk

intInfixFunc ::
    (Monad m, GuestType t) =>
    (Integer -> Integer -> t) -> BuiltinRunner m pl
intInfixFunc f thunkId =
    do
        V2 x y <-
            extractInfixParams thunkId
            >>= traverse (fmap fromGuest . Eval.whnfThunk)
        f x y & toGuest & return

eval :: Monad m => Def.FFIName -> ThunkId -> EvalT pl m (ValHead pl)
eval name =
    case name of
    Def.FFIName ["Prelude"] "if" -> builtinIf
    Def.FFIName ["Prelude"] "==" -> intInfixFunc (==)
    Def.FFIName ["Prelude"] "<" -> intInfixFunc (<)
    Def.FFIName ["Prelude"] "<=" -> intInfixFunc (<=)
    Def.FFIName ["Prelude"] ">" -> intInfixFunc (>)
    Def.FFIName ["Prelude"] ">=" -> intInfixFunc (>=)
    Def.FFIName ["Prelude"] "*" -> intInfixFunc (*)
    Def.FFIName ["Prelude"] "+" -> intInfixFunc (+)
    Def.FFIName ["Prelude"] "-" -> intInfixFunc (-)
    Def.FFIName ["Prelude"] "mod" -> intInfixFunc mod
    Def.FFIName ["Prelude"] "negate" -> builtinNegate
    Def.FFIName ["Prelude"] "||" -> builtinOr
    _ -> error $ show name ++ " not yet supported"
