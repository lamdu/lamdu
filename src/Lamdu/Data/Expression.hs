{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expression
  ( VariableRef(..), parameterRef, definitionRef
  , Lambda(..), lambdaParamId, lambdaParamType, lambdaBody
  , Apply(..), applyFunc, applyArg
  , Leaf(..), getVariable, literalInteger, hole, set, integerType
  , Body(..), bodyLambda, bodyPi, bodyApply, bodyLeaf
  , BodyExpr
  , makeApply, pureApply
  , makePi, makeLambda
  , pureHole
  , pureSet
  , makeParameterRef, makeDefinitionRef
  , makeLiteralInteger, pureLiteralInteger
  , pureIntegerType
  , Expression(..), eValue, ePayload
  , pureExpression
  , randomizeExpr
  , canonizeParamIds, randomizeParamIds
  , matchExpression
  , subExpressions
  , isDependentPi
  , funcArguments
  -- Traversals
  , bitraverseBody
  , bitraverseExpression
  , expressionBodyDef
  , expressionDef

  -- Each expression gets a ptr to itself
  , addSubexpressionSetters
  , addBodyContexts

  , LambdaWrapper(..), lambdaWrapperPrism
  ) where

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Lens (Simple, Prism, (^.), (??))
import Control.Lens.Utils (contextSetter, result)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Function (on)
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
import qualified System.Random as Random

data Lambda expr = Lambda
  { _lambdaParamId :: Guid
  , _lambdaParamType :: expr
  -- TODO: Rename to _lambdaResult (for Pi it is not a body)
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
LensTH.makePrisms ''VariableRef

data Leaf def
  = GetVariable !(VariableRef def)
  | LiteralInteger !Integer
  | Set
  | IntegerType
  | Hole
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
LensTH.makePrisms ''Leaf

data Body def expr
  = BodyLambda {-# UNPACK #-} !(Lambda expr)
  | BodyPi {-# UNPACK #-} !(Lambda expr)
  | BodyApply {-# UNPACK #-} !(Apply expr)
  | BodyLeaf !(Leaf def)
  deriving (Eq, Ord, Functor, Foldable, Traversable)
LensTH.makePrisms ''Body

type BodyExpr def a = Body def (Expression def a)

bitraverseBody ::
  Applicative f => (defa -> f defb) -> (expra -> f exprb) ->
  Body defa expra ->
  f (Body defb exprb)
bitraverseBody onDef onExpr body =
  case body of
  BodyLambda x -> BodyLambda <$> traverse onExpr x
  BodyPi x -> BodyPi <$> traverse onExpr x
  BodyApply x -> BodyApply <$> traverse onExpr x
  BodyLeaf leaf -> BodyLeaf <$> traverse onDef leaf

expressionBodyDef :: Lens.Traversal (Body a expr) (Body b expr) a b
expressionBodyDef = (`bitraverseBody` pure)

makeApply :: expr -> expr -> Body def expr
makeApply func arg = BodyApply $ Apply func arg

makePi :: Guid -> expr -> expr -> Body def expr
makePi argId argType resultType =
  BodyPi $ Lambda argId argType resultType

makeLambda :: Guid -> expr -> expr -> Body def expr
makeLambda argId argType body =
  BodyLambda $ Lambda argId argType body

makeParameterRef :: Guid -> Body def a
makeParameterRef = BodyLeaf . GetVariable . ParameterRef

makeDefinitionRef :: def -> Body def a
makeDefinitionRef = BodyLeaf . GetVariable . DefinitionRef

makeLiteralInteger :: Integer -> Body def a
makeLiteralInteger = BodyLeaf . LiteralInteger

instance (Show expr, Show def) => Show (Body def expr) where
  show (BodyLambda (Lambda paramId paramType body)) =
    concat ["\\", show paramId, ":", showP paramType, "==>", showP body]
  show (BodyPi (Lambda paramId paramType body)) =
    concat ["(", show paramId, ":", showP paramType, ")->", showP body]
  show (BodyApply (Apply func arg)) = unwords [showP func, showP arg]
  show (BodyLeaf (GetVariable (ParameterRef guid))) = "par:" ++ show guid
  show (BodyLeaf (GetVariable (DefinitionRef defI))) = "def:" ++ show defI
  show (BodyLeaf (LiteralInteger int)) = show int
  show (BodyLeaf x) = show x

showP :: Show a => a -> String
showP = parenify . show

parenify :: String -> String
parenify x = concat ["(", x, ")"]

-- TODO: Expression = Cofree, do we want to use that?
data Expression def a = Expression
  { _eValue :: Body def (Expression def a)
  , _ePayload :: a
  } deriving (Functor, Eq, Ord, Foldable, Traversable, Typeable)

LensTH.makeLenses ''Expression
derive makeBinary ''VariableRef
derive makeBinary ''Lambda
derive makeBinary ''Apply
derive makeBinary ''Leaf
derive makeBinary ''Body
derive makeBinary ''Expression
LensTH.makeLenses ''Lambda
LensTH.makeLenses ''Apply

instance (Show a, Show def) => Show (Expression def a) where
  show (Expression body payload) =
    show body ++ showPayload
    where
      showPayload =
        case show payload of
        "()" -> ""
        x -> "{" ++ x ++ "}"

bitraverseExpression ::
  Applicative f =>
  (defa -> f defb) -> (pla -> f plb) ->
  Expression defa pla -> f (Expression defb plb)
bitraverseExpression onDef onPl = f
  where
    f (Expression body payload) =
      Expression <$> bitraverseBody onDef f body <*> onPl payload

expressionDef :: Lens.Traversal (Expression a pl) (Expression b pl) a b
expressionDef = (`bitraverseExpression` pure)

addBodyContexts ::
  (a -> b) ->
  Lens.Context (Body def a) (Body def b) container ->
  Body def (Lens.Context a b container)
addBodyContexts tob (Lens.Context intoContainer body) =
  Lens.over afterSetter intoContainer $
  case body of
  BodyLambda lam -> onLambda makeLambda BodyLambda lam
  BodyPi lam -> onLambda makePi BodyPi lam
  BodyApply app ->
    Lens.over afterSetter BodyApply $
    (makeApply `on` mkContext app)
    (applyFunc, applyFunc)
    (applyArg, applyArg)
  BodyLeaf leaf -> BodyLeaf leaf
  where
    afterSetter = traverse . contextSetter . result
    tobs = Lens.over traverse tob
    mkContext orig (lensa, lensb) =
      Lens.Context (Lens.set lensb ?? tobs orig) $
      orig ^. lensa
    onLambda consa consb lam@(Lambda param _ _) =
      Lens.over afterSetter consb $
      (consa param `on` mkContext lam)
      (lambdaParamType, lambdaParamType)
      (lambdaBody, lambdaBody)

addSubexpressionSetters ::
  (a -> b) -> Lens.Context (Expression def a) (Expression def b) container ->
  Expression def (Expression def b -> container, a)
addSubexpressionSetters atob (Lens.Context intoContainer (Expression body a)) =
  Expression newBody (intoContainer, a)
  where
    newBody =
      Lens.over traverse (addSubexpressionSetters atob) $
      addBodyContexts (fmap atob) bodyPtr
    bodyPtr =
      Lens.Context (intoContainer . (`Expression` atob a)) body

pureExpression :: Body def (Expression def ()) -> Expression def ()
pureExpression = (`Expression` ())

pureIntegerType :: Expression def ()
pureIntegerType = pureExpression $ BodyLeaf IntegerType

pureLiteralInteger :: Integer -> Expression def ()
pureLiteralInteger = pureExpression . makeLiteralInteger

pureApply :: Expression def () -> Expression def () -> Expression def ()
pureApply f x = pureExpression $ makeApply f x

pureHole :: Expression def ()
pureHole = pureExpression $ BodyLeaf Hole

pureSet :: Expression def ()
pureSet = pureExpression $ BodyLeaf Set

randomizeExpr :: (RandomGen g, Random r) => g -> Expression def (r -> a) -> Expression def a
randomizeExpr gen = (`evalState` gen) . traverse randomize
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
      liftA2 (Lambda newParamId) (go paramType) .
        Reader.local (Map.insert oldParamId newParamId) $ go body
    go (Expression v s) = fmap (`Expression` s) $
      case v of
      BodyLambda lambda -> fmap BodyLambda $ onLambda lambda
      BodyPi lambda -> fmap BodyPi $ onLambda lambda
      BodyApply (Apply func arg) -> liftA2 makeApply (go func) (go arg)
      gv@(BodyLeaf (GetVariable (ParameterRef guid))) ->
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
      (BodyLambda l0, BodyLambda l1) ->
        mkExpression $ BodyLambda <$> onLambda l0 l1
      (BodyPi l0, BodyPi l1) ->
        mkExpression $ BodyPi <$> onLambda l0 l1
      (BodyApply (Apply f0 a0), BodyApply (Apply f1 a1)) ->
        mkExpression $ BodyApply <$> liftA2 Apply (go scope f0 f1) (go scope a0 a1)
      (BodyLeaf gv@(GetVariable (ParameterRef p0)),
       BodyLeaf (GetVariable (ParameterRef p1)))
        | p0 == lookupGuid p1 ->
          mkExpression . pure $ BodyLeaf gv
      (BodyLeaf x, BodyLeaf y)
        | x == y -> mkExpression . pure $ BodyLeaf x
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
  BodyLeaf (GetVariable (ParameterRef getParGuid)) ->
    makeParameterRef $ f getParGuid
  _ -> onGetParamGuids f <$> body

subExpressions :: Expression def a -> [Expression def a]
subExpressions x =
  x : Foldable.concatMap subExpressions (x ^. eValue)

hasGetVar :: Guid -> Expression def a -> Bool
hasGetVar =
  Lens.anyOf
  ( Lens.folding subExpressions . eValue
  . bodyLeaf . getVariable . parameterRef
  ) . (==)

isDependentPi :: Expression def a -> Bool
isDependentPi =
  Lens.anyOf (eValue . bodyPi) f
  where
    f (Lambda g _ resultType) = hasGetVar g resultType

funcArguments :: Expression def a -> [Expression def a]
funcArguments =
  Lens.toListOf (eValue . bodyLambda . Lens.folding f)
  where
    f (Lambda _ paramType body) =
      paramType : funcArguments body

data LambdaWrapper = LambdaWrapperLambda | LambdaWrapperPi
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''LambdaWrapper

lambdaWrapperPrism :: LambdaWrapper -> Simple Prism (Body def expr) (Lambda expr)
lambdaWrapperPrism LambdaWrapperLambda = bodyLambda
lambdaWrapperPrism LambdaWrapperPi = bodyPi
