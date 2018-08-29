{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Eval.Results
    ( Body(..), _RRecExtend, _RInject, _RFunc, _RRecEmpty, _RPrimVal, _RError, _RArray
    , Val(..), payload, body
    , ScopeId(..), topLevelScopeId
    , EvalTypeError(..)
    , WhichGlobal(..), encodeWhichGlobal, decodeWhichGlobal
    , ErrorType(..), _LamduBug, _BrokenDef, _ReachedHole
    , EvalException(..), errorType, errorDesc, errorPosition
    , EvalResults(..), erExprValues, erAppliesOfLam, erCache, erCompleted
    , empty
    , extractField
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List.Lens (prefixed)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Lamdu.Calc.Identifier (identHex, identFromHex)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

newtype ScopeId = ScopeId Int
    deriving (Show, Eq, Ord, Binary, Generic)

newtype EvalTypeError = EvalTypeError Text
    deriving (Show, Eq, Ord, Generic)

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0

data Body val
    = RRecExtend (V.RecExtend val)
    | RInject (V.Inject val)
    | RFunc Int -- Identifier for function instance
    | RRecEmpty
    | RPrimVal V.PrimVal
    | RArray [val]
    | RError EvalTypeError
    deriving (Show, Functor, Foldable, Traversable)

data Val pl = Val
    { _payload :: pl
    , _body :: Body (Val pl)
    } deriving (Show, Functor, Foldable, Traversable)

data ErrorType = LamduBug | BrokenDef | ReachedHole | RuntimeError
    deriving (Read, Show, Generic, Eq)
Lens.makePrisms ''ErrorType

data WhichGlobal = GlobalRepl | GlobalDef V.Var
    deriving Eq
Lens.makePrisms ''WhichGlobal

encodeWhichGlobal :: WhichGlobal -> String
encodeWhichGlobal GlobalRepl = "REPL"
encodeWhichGlobal (GlobalDef (V.Var var)) = "DEF_" ++ identHex var

decodeWhichGlobal :: String -> Either String WhichGlobal
decodeWhichGlobal "REPL" = Right GlobalRepl
decodeWhichGlobal x =
    x ^? prefixed "DEF_" & maybe (Left "Not REPL or DEF_*") Right
    >>= identFromHex <&> V.Var <&> GlobalDef

data EvalException srcId = EvalException
    { _errorType :: ErrorType
    , _errorDesc :: Text
    , _errorPosition :: Maybe (WhichGlobal, srcId)
    } deriving Eq
Lens.makeLenses ''EvalException

instance Show srcId => Show (EvalException srcId) where
    show (EvalException t d p) =
        "Eval exception: " ++ show t ++ " (" ++ Text.unpack d ++ ") at " ++
        case p of
        Nothing -> "N/A"
        Just (g, e) -> encodeWhichGlobal g ++ ":" ++ show e

extractField :: Show a => a -> T.Tag -> Val a -> Val a
extractField errPl tag (Val _ (RRecExtend (V.RecExtend vt vv vr)))
    | vt == tag = vv
    | otherwise = extractField errPl tag vr
extractField _ _ v@(Val _ RError {}) = v
extractField errPl tag x =
    "Expected record with tag: " ++ show tag ++ " got: " ++ show x
    & Text.pack & EvalTypeError & RError & Val errPl

data EvalResults srcId =
    EvalResults
    { _erExprValues :: Map srcId (Map ScopeId (Val ()))
    , _erAppliesOfLam :: Map srcId (Map ScopeId [(ScopeId, Val ())])
    , _erCache :: IntMap (Val ())
    , _erCompleted :: Maybe (Either (EvalException srcId) (Val ()))
    } deriving Show

empty :: EvalResults srcId
empty =
    EvalResults
    { _erExprValues = Map.empty
    , _erAppliesOfLam = Map.empty
    , _erCache = IntMap.empty
    , _erCompleted = Nothing
    }

Lens.makeLenses ''EvalResults
Lens.makeLenses ''Val
Lens.makePrisms ''Body
