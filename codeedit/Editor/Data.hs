{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Editor.Data
  ( Definition(..), DefinitionBody(..)
  , FFIName(..)
  , VariableRef(..)
  , Lambda(..), lambdaParamId, lambdaParamType, lambdaBody
  , Apply(..), applyFunc, applyArg
  , Builtin(..)
  , Leaf(..)
  , ExpressionBody(..)
  , ExpressionBodyExpr
  , makeApply, makePi, makeLambda
  , hole, pureHole
  , set, pureSet
  , makeParameterRef, makeDefinitionRef, makeLiteralInteger
  , Expression(..), eValue, ePayload
  , pureExpression
  , randomizeExpr
  , canonizeParamIds, randomizeParamIds
  , matchExpression
  , subExpressions
  , isDependentPi
  -- Traversals
  , bitraverseExpressionBody
  , bitraverseExpression
  , expressionBodyDef
  , expressionDef
  ) where

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Lens ((^.))
import Control.Monad (liftM, liftM2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified System.Random as Random

data Lambda expr = Lambda
  { _lambdaParamId :: Guid
  , _lambdaParamType :: expr
  , _lambdaBody :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Apply expr = Apply
  { _applyFunc :: expr
  , _applyArg :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
instance Applicative Apply where
  pure x = Apply x x
  Apply f0 a0 <*> Apply f1 a1 = Apply (f0 f1) (a0 a1)

data VariableRef def
  = ParameterRef {-# UNPACK #-} !Guid -- of the lambda/pi
  | DefinitionRef def
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Leaf def
  = GetVariable !(VariableRef def)
  | LiteralInteger !Integer
  | Set
  | IntegerType
  | Hole
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ExpressionBody def expr
  = ExpressionLambda {-# UNPACK #-} !(Lambda expr)
  | ExpressionPi {-# UNPACK #-} !(Lambda expr)
  | ExpressionApply {-# UNPACK #-} !(Apply expr)
  | ExpressionLeaf !(Leaf def)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

bitraverseExpressionBody ::
  Applicative f => (defa -> f defb) -> (expra -> f exprb) ->
  ExpressionBody defa expra ->
  f (ExpressionBody defb exprb)
bitraverseExpressionBody onDef onExpr body =
  case body of
  ExpressionLambda x -> ExpressionLambda <$> traverse onExpr x
  ExpressionPi x -> ExpressionPi <$> traverse onExpr x
  ExpressionApply x -> ExpressionApply <$> traverse onExpr x
  ExpressionLeaf leaf -> ExpressionLeaf <$> traverse onDef leaf

expressionBodyDef :: Lens.Traversal (ExpressionBody a expr) (ExpressionBody b expr) a b
expressionBodyDef = (`bitraverseExpressionBody` pure)

type ExpressionBodyExpr def a = ExpressionBody def (Expression def a)

hole :: ExpressionBody def expr
hole = ExpressionLeaf Hole

set :: ExpressionBody def expr
set = ExpressionLeaf Set

makeApply :: expr -> expr -> ExpressionBody def expr
makeApply func arg = ExpressionApply $ Apply func arg

makePi :: Guid -> expr -> expr -> ExpressionBody def expr
makePi argId argType resultType =
  ExpressionPi $ Lambda argId argType resultType

makeLambda :: Guid -> expr -> expr -> ExpressionBody def expr
makeLambda argId argType body =
  ExpressionLambda $ Lambda argId argType body

makeParameterRef :: Guid -> ExpressionBody def a
makeParameterRef = ExpressionLeaf . GetVariable . ParameterRef

makeDefinitionRef :: def -> ExpressionBody def a
makeDefinitionRef = ExpressionLeaf . GetVariable . DefinitionRef

makeLiteralInteger :: Integer -> ExpressionBody def a
makeLiteralInteger = ExpressionLeaf . LiteralInteger

instance (Show expr, Show def) => Show (ExpressionBody def expr) where
  show (ExpressionLambda (Lambda paramId paramType body)) =
    concat ["\\", show paramId, ":", showP paramType, "==>", showP body]
  show (ExpressionPi (Lambda paramId paramType body)) =
    concat ["(", show paramId, ":", showP paramType, ")->", showP body]
  show (ExpressionApply (Apply func arg)) = unwords [showP func, showP arg]
  show (ExpressionLeaf (GetVariable (ParameterRef guid))) = "par:" ++ show guid
  show (ExpressionLeaf (GetVariable (DefinitionRef defI))) = "def:" ++ show defI
  show (ExpressionLeaf (LiteralInteger int)) = show int
  show (ExpressionLeaf x) = show x

showP :: Show a => a -> String
showP = parenify . show

parenify :: String -> String
parenify x = concat ["(", x, ")"]

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Eq, Ord)

instance Show FFIName where
  show (FFIName path name) = concatMap (++".") path ++ name

data Builtin = Builtin
  { bName :: FFIName
  } deriving (Eq, Ord, Show)

data DefinitionBody expr
  = DefinitionExpression expr
  | DefinitionBuiltin Builtin
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Definition expr = Definition
  { defBody :: DefinitionBody expr
  , defType :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Typeable)

data Expression def a = Expression
  { _eValue :: ExpressionBody def (Expression def a)
  , _ePayload :: a
  } deriving (Functor, Eq, Ord, Foldable, Traversable, Typeable)
LensTH.makeLenses ''Expression

instance (Show a, Show def) => Show (Expression def a) where
  show (Expression body payload) = show body ++ "{" ++ show payload ++ "}"

bitraverseExpression ::
  Applicative f =>
  (defa -> f defb) -> (pla -> f plb) ->
  Expression defa pla -> f (Expression defb plb)
bitraverseExpression onDef onPl = f
  where
    f (Expression body payload) =
      Expression <$> bitraverseExpressionBody onDef f body <*> onPl payload

expressionDef :: Lens.Traversal (Expression a pl) (Expression b pl) a b
expressionDef = (`bitraverseExpression` pure)

pureExpression :: ExpressionBody def (Expression def ()) -> Expression def ()
pureExpression = (`Expression` ())

pureHole :: Expression def ()
pureHole = pureExpression hole

pureSet :: Expression def ()
pureSet = pureExpression set

randomizeExpr :: (RandomGen g, Random r) => g -> Expression def (r -> a) -> Expression def a
randomizeExpr gen = (`evalState` gen) . Traversable.mapM randomize
  where
    randomize f = f <$> state random

canonizeParamIds :: Expression def a -> Expression def a
canonizeParamIds = randomizeParamIds $ Random.mkStdGen 0

randomizeParamIds :: RandomGen g => g -> Expression def a -> Expression def a
randomizeParamIds gen =
  (`evalState` gen) . (`runReaderT` Map.empty) . go
  where
    onLambda (Lambda oldParamId paramType body) = do
      newParamId <- lift $ state random
      liftM2 (Lambda newParamId) (go paramType) .
        Reader.local (Map.insert oldParamId newParamId) $ go body
    go (Expression v s) = liftM (`Expression` s) $
      case v of
      ExpressionLambda lambda -> liftM ExpressionLambda $ onLambda lambda
      ExpressionPi lambda -> liftM ExpressionPi $ onLambda lambda
      ExpressionApply (Apply func arg) -> liftM2 makeApply (go func) (go arg)
      gv@(ExpressionLeaf (GetVariable (ParameterRef guid))) ->
        Reader.asks $
        maybe gv makeParameterRef .
        Map.lookup guid
      _ -> return v

-- TODO: Generalize to defa/defb/defc with hof's to handle matching
-- them?  The returned expression gets the same guids as the left
-- expression
matchExpression ::
  (Eq def, Applicative f) =>
  (a -> b -> f c) ->
  (Expression def a -> Expression def b -> f (Expression def c)) ->
  Expression def a -> Expression def b -> f (Expression def c)
matchExpression onMatch onMismatch =
  go Map.empty
  where
    go scope e0@(Expression body0 pl0) e1@(Expression body1 pl1) =
      case (body0, body1) of
      (ExpressionLambda l0, ExpressionLambda l1) ->
        mkExpression $ ExpressionLambda <$> onLambda l0 l1
      (ExpressionPi l0, ExpressionPi l1) ->
        mkExpression $ ExpressionPi <$> onLambda l0 l1
      (ExpressionApply (Apply f0 a0), ExpressionApply (Apply f1 a1)) ->
        mkExpression $ ExpressionApply <$> liftA2 Apply (go scope f0 f1) (go scope a0 a1)
      (ExpressionLeaf gv@(GetVariable (ParameterRef p0)),
       ExpressionLeaf (GetVariable (ParameterRef p1)))
        | p0 == lookupGuid p1 ->
          mkExpression . pure $ ExpressionLeaf gv
      (ExpressionLeaf x, ExpressionLeaf y)
        | x == y -> mkExpression . pure $ ExpressionLeaf x
      _ -> onMismatch e0 $ onGetParamGuids lookupGuid e1
      where
        lookupGuid guid = fromMaybe guid $ Map.lookup guid scope
        mkExpression body = Expression <$> body <*> onMatch pl0 pl1
        onLambda (Lambda p0 pt0 r0) (Lambda p1 pt1 r1) =
          liftA2 (Lambda p0) (go scope pt0 pt1) $
          go (Map.insert p1 p0 scope) r0 r1

onGetParamGuids :: (Guid -> Guid) -> Expression def a -> Expression def a
onGetParamGuids f (Expression body payload) =
  flip Expression payload $
  case body of
  ExpressionLeaf (GetVariable (ParameterRef getParGuid)) ->
    makeParameterRef $ f getParGuid
  _ -> onGetParamGuids f <$> body

subExpressions :: Expression def a -> [Expression def a]
subExpressions x =
  x : Foldable.concatMap subExpressions (x ^. eValue)

isDependentPi :: Expression def a -> Bool
isDependentPi (Expression (ExpressionPi (Lambda g _ resultType)) _) =
  any (isGet . Lens.view eValue) $ subExpressions resultType
  where
    isGet (ExpressionLeaf (GetVariable (ParameterRef p))) = p == g
    isGet _ = False
isDependentPi _ = False

derive makeBinary ''FFIName
derive makeBinary ''VariableRef
derive makeBinary ''Lambda
derive makeBinary ''Apply
derive makeBinary ''Leaf
derive makeBinary ''ExpressionBody
derive makeBinary ''Expression
derive makeBinary ''Builtin
derive makeBinary ''DefinitionBody
derive makeBinary ''Definition
LensTH.makeLenses ''Lambda
LensTH.makeLenses ''Apply
