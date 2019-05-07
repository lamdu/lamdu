{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, TypeFamilies, MultiParamTypeClasses #-}
module Lamdu.Eval.Results
    ( Body(..), _RRecExtend, _RInject, _RFunc, _RRecEmpty, _RPrimVal, _RError, _RArray
    , Val
    , ScopeId(..), topLevelScopeId
    , EvalTypeError(..)
    , WhichGlobal(..), encodeWhichGlobal, decodeWhichGlobal
    , CompiledErrorType(..), encodeCompiledError, decodeJsErrorException
        , _DependencyTypeOutOfDate, _ReachedHole, _UnhandledCase
    , Error(..), _CompiledError, _RuntimeError
    , EvalException(..), error, errorPosition
    , EvalResults(..), erExprValues, erAppliesOfLam, erCache, erCompleted
    , empty
    , extractField
    ) where

import qualified AST
import           AST (Ann(..), Tie, Tree)
import           AST.Term.Row (RowExtend(..))
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
import           Text.Read (readMaybe)

import           Lamdu.Prelude hiding (error)

newtype ScopeId = ScopeId Int
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Binary)

newtype EvalTypeError = EvalTypeError Text
    deriving (Show, Eq, Ord, Generic)

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0

data Body f
    = RRecExtend (RowExtend T.Tag Body Body f)
    | RInject (V.Inject (Tie f Body))
    | RFunc Int -- Identifier for function instance
    | RRecEmpty
    | RPrimVal V.PrimVal
    | RArray [Tie f Body]
    | RError EvalTypeError

AST.makeChildren ''Body

deriving instance Show (Tie f Body) => Show (Body f)

type Val pl = Tree (Ann pl) Body

data CompiledErrorType
    = ReachedHole -- ^ Reached a hole
    | UnhandledCase -- ^ Reached absurd (lamdu bug)
    | DependencyTypeOutOfDate -- ^ Reached a definition with stale type
    deriving (Read, Show, Generic, Eq)
Lens.makePrisms ''CompiledErrorType

encodeCompiledError :: CompiledErrorType -> String
encodeCompiledError = show

data Error
    = CompiledError CompiledErrorType
    | RuntimeError Text -- ^ JS exception
    deriving (Eq, Generic)
Lens.makePrisms ''Error

-- | JS errors are either a "RuntimeError" with an exception string,
-- or a name of a compiled error with Nothing more
decodeJsErrorException :: String -> Maybe Text -> Either String Error
decodeJsErrorException "RuntimeError" Nothing = Left "RuntimeError must have 'exception' attribute"
decodeJsErrorException "RuntimeError" (Just exc) = Right (RuntimeError exc)
decodeJsErrorException other (Just exc) =
    "Only RuntimeError should have exception attribute, not " ++
    show other ++ " (attr = " ++ show exc ++ ")"
    & Left
decodeJsErrorException other Nothing =
    case readMaybe other of
    Nothing -> "Invalid error type: " ++ show other & Left
    Just err -> Right (CompiledError err)

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
    { _error :: Error
    , _errorPosition :: Maybe (WhichGlobal, srcId)
    } deriving Eq
Lens.makeLenses ''EvalException

extractField :: Show a => a -> T.Tag -> Val a -> Val a
extractField errPl tag (Ann _ (RRecExtend (RowExtend vt vv vr)))
    | vt == tag = vv
    | otherwise = extractField errPl tag vr
extractField _ _ v@(Ann _ RError{}) = v
extractField errPl tag x =
    "Expected record with tag: " ++ show tag ++ " got: " ++ show x
    & Text.pack & EvalTypeError & RError & Ann errPl

data EvalResults srcId =
    EvalResults
    { _erExprValues :: Map srcId (Map ScopeId (Val ()))
    , _erAppliesOfLam :: Map srcId (Map ScopeId [(ScopeId, Val ())])
    , _erCache :: IntMap (Val ())
    , _erCompleted :: Maybe (Either (EvalException srcId) (Val ()))
    }

empty :: EvalResults srcId
empty =
    EvalResults
    { _erExprValues = Map.empty
    , _erAppliesOfLam = Map.empty
    , _erCache = IntMap.empty
    , _erCompleted = Nothing
    }

Lens.makeLenses ''EvalResults
Lens.makePrisms ''Body
