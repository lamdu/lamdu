module Lamdu.Builtins
    ( eval
    ) where

--import qualified Data.Map as Map

import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Utils (matchKeys)
import           Data.Traversable (traverse)
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

extractRecordParams :: Monad m => [Tag] -> ThunkId -> EvalT pl m [ThunkId]
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

extractInfixParams :: Monad m => ThunkId -> EvalT pl m [ThunkId]
extractInfixParams =
    extractRecordParams [Builtins.infixlTag, Builtins.infixrTag]

type BuiltinRunner m pl = ThunkId -> EvalT pl m (ValHead pl)

class GuestType t where toGuest :: t -> ValHead pl
instance GuestType Integer where toGuest = HInteger
instance GuestType Bool where toGuest = HBuiltin . Def.FFIName ["Prelude"] . show

builtinIf :: Monad m => BuiltinRunner m pl
builtinIf thunkId =
    do
        [obj, then_, else_] <-
            extractRecordParams [ Builtins.objTag, Builtins.thenTag, Builtins.elseTag ] thunkId
        HBuiltin condition <- Eval.whnfThunk obj
        case condition of
            Def.FFIName ["Prelude"] "True" -> then_
            Def.FFIName ["Prelude"] "False" -> else_
            _ -> error "Unexpected condition parameter to if"
            & Eval.whnfThunk

builtinNegate :: Monad m => BuiltinRunner m pl
builtinNegate thunkId =
    do
        param <- Eval.whnfThunk thunkId
        case param of
            HInteger t -> negate t & HInteger & return
            _ -> error "negate expects integer"

intInfixFunc ::
    (Monad m, GuestType t) =>
    (Integer -> Integer -> t) -> ThunkId -> EvalT pl m (ValHead pl)
intInfixFunc f thunkId =
    do
        [l, r] <- extractInfixParams thunkId >>= traverse Eval.whnfThunk
        case (l, r) of
            (HInteger x, HInteger y) -> toGuest (f x y)
            _ -> error "== currently only supports Integers"
            & return

eval :: Monad m => Def.FFIName -> ThunkId -> EvalT pl m (ValHead pl)
eval name =
    case name of
    Def.FFIName ["Prelude"] "if" -> builtinIf
    Def.FFIName ["Prelude"] "==" -> intInfixFunc (==)
    Def.FFIName ["Prelude"] "*" -> intInfixFunc (*)
    Def.FFIName ["Prelude"] "+" -> intInfixFunc (+)
    Def.FFIName ["Prelude"] "-" -> intInfixFunc (-)
    Def.FFIName ["Prelude"] "negate" -> builtinNegate
    _ -> error $ show name ++ " not yet supported"
