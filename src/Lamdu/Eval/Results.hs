{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, TypeFamilies, MultiParamTypeClasses, GADTs #-}
module Lamdu.Eval.Results
    ( Body(..), _RRecExtend, _RInject, _RFunc, _RRecEmpty, _RPrimVal, _RError, _RArray
    , Inject(..), injectTag, injectVal
    , Val
    , ScopeId(..), topLevelScopeId
    , EvalTypeError(..)
    , CompiledErrorType(..), encodeCompiledError, decodeJsErrorException
        , _DependencyTypeOutOfDate, _ReachedHole, _UnhandledCase
    , Error(..), _CompiledError, _RuntimeError
    , EvalException(..), error, errorPosition
    , EvalResults(..), erExprValues, erAppliesOfLam, erCache, erErrors
    , empty
    , extractField
    ) where

import qualified Control.Lens as Lens
import           Data.IntMap (IntMap)
import qualified Data.Text as Text
import           Data.UUID (UUID)
import qualified Hyper
import           Hyper.Syntax.Row (RowExtend(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Text.Read (readMaybe)

import           Lamdu.Prelude hiding (error)

newtype ScopeId = ScopeId Int
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Binary)

newtype EvalTypeError = EvalTypeError Text
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord)

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0

-- Todo: make a shared inject type in hypertypes?
data Inject k = Inject
    { _injectTag :: T.Tag
    , _injectVal :: k :# Body
    }

data Body k
    = RRecExtend (RowExtend T.Tag Body Body k)
    | RInject (Inject k)
    | RFunc Int -- Identifier for function instance
    | RRecEmpty
    | RPrimVal V.PrimVal
    | RArray [k :# Body]
    | RError EvalTypeError

Hyper.makeHTraversableAndBases ''Inject
Hyper.makeHTraversableAndBases ''Body

deriving instance Show (k :# Body) => Show (Body k)
deriving instance Show (k :# Body) => Show (Inject k)

type Val pl = Annotated pl # Body

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
    deriving (Eq, Generic, Show)
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

data EvalException srcId = EvalException
    { _error :: Error
    , _errorPosition :: Maybe (V.Var, srcId)
    } deriving Eq
Lens.makeLenses ''EvalException

extractField :: Show a => a -> T.Tag -> Val a -> Val a
extractField errPl tag (Ann _ (RRecExtend (RowExtend vt vv vr)))
    | vt == tag = vv
    | otherwise = extractField errPl tag vr
extractField _ _ v@(Ann _ RError{}) = v
extractField errPl tag x =
    "Expected record with tag: " ++ show tag ++ " got: " ++ show x
    & Text.pack & EvalTypeError & RError & Ann (Const errPl)

data EvalResults =
    EvalResults
    { _erExprValues :: Map UUID (Map ScopeId (Val ()))
    , _erAppliesOfLam :: Map UUID (Map ScopeId [(ScopeId, Val ())])
    , _erCache :: IntMap (Val ())
    , _erErrors :: Map V.Var (EvalException UUID)
    }

empty :: EvalResults
empty =
    EvalResults
    { _erExprValues = mempty
    , _erAppliesOfLam = mempty
    , _erCache = mempty
    , _erErrors = mempty
    }

Lens.makeLenses ''EvalResults
Lens.makeLenses ''Inject
Lens.makePrisms ''Body
