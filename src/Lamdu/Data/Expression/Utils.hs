{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Lamdu.Data.Expression.Utils
  ( makeApply, pureApply
  , makePi, makeLambda
  , pureHole
  , pureSet
  , bodyParameterRef, bodyDefinitionRef
  , bodyLiteralInteger, pureLiteralInteger
  , pureIntegerType
  , pureExpression
  , randomizeExpr
  , canonizeParamIds, randomizeParamIds
  , matchExpression
  , subExpressions
  , isDependentPi
  , funcArguments
  , LambdaWrapper(..), lambdaWrapperPrism
  , applyForms
  -- Traversals
  , bitraverseExpression
  , bitraverseBody
  , expressionBodyDef
  , expressionDef
  ) where

import Lamdu.Data.Expression

-- TODO in future: Use Prelude.(.) with future version of Lens.
import Prelude hiding ((.))
import Control.Category ((.))

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Lens (Simple, Prism, (^.), (+~))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified System.Random as Random

makeApply :: expr -> expr -> Body def expr
makeApply func arg = BodyApply $ Apply func arg

makePi :: Guid -> expr -> expr -> Body def expr
makePi argId argType resultType =
  BodyPi $ Lambda argId argType resultType

makeLambda :: Guid -> expr -> expr -> Body def expr
makeLambda argId argType body =
  BodyLambda $ Lambda argId argType body

bodyParameterRef :: Simple Prism (Body def expr) Guid
bodyParameterRef = bodyLeaf . getVariable . parameterRef

bodyDefinitionRef :: Prism (Body defa expr) (Body defb expr) defa defb
bodyDefinitionRef = bodyLeaf . getVariable . definitionRef

bodyLiteralInteger :: Simple Prism (Body def expr) Integer
bodyLiteralInteger = bodyLeaf . literalInteger

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

pureExpression :: Body def (Expression def ()) -> Expression def ()
pureExpression = (`Expression` ())

pureIntegerType :: Expression def ()
pureIntegerType = pureExpression $ BodyLeaf IntegerType

pureLiteralInteger :: Integer -> Expression def ()
pureLiteralInteger = pureExpression . Lens.review bodyLiteralInteger

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
        maybe gv (Lens.review bodyParameterRef) .
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
onGetParamGuids f =
  Lens.over (eBody . bodyParameterRef) f .
  Lens.over (eBody . Lens.mapped) (onGetParamGuids f)

subExpressions :: Expression def a -> [Expression def a]
subExpressions x =
  x : Foldable.concatMap subExpressions (x ^. eBody)

hasGetVar :: Guid -> Expression def a -> Bool
hasGetVar =
  Lens.anyOf
  ( Lens.folding subExpressions
  . eBody . bodyParameterRef
  ) . (==)

isDependentPi :: Expression def a -> Bool
isDependentPi =
  Lens.anyOf (eBody . bodyPi) f
  where
    f (Lambda g _ resultType) = hasGetVar g resultType

funcArguments :: Expression def a -> [Expression def a]
funcArguments =
  Lens.toListOf (eBody . bodyLambda . Lens.folding f)
  where
    f (Lambda _ paramType body) =
      paramType : funcArguments body

data LambdaWrapper = LambdaWrapperLambda | LambdaWrapperPi
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''LambdaWrapper

lambdaWrapperPrism :: LambdaWrapper -> Simple Prism (Body def expr) (Lambda expr)
lambdaWrapperPrism LambdaWrapperLambda = bodyLambda
lambdaWrapperPrism LambdaWrapperPi = bodyPi

countArrows :: Expression def () -> Int
countArrows Expression
  { _eBody =
    BodyPi (Lambda _ _ resultType)
  } = 1 + countArrows resultType
countArrows _ = 0

-- TODO: Return a record, not a tuple
countPis :: Expression def () -> (Int, Int)
countPis e@Expression
  { _eBody =
    BodyPi (Lambda _ _ resultType)
  }
  | isDependentPi e = Lens._1 +~ 1 $ countPis resultType
  | otherwise = (0, 1 + countArrows resultType)
countPis _ = (0, 0)

-- Transform expression to expression applied with holes,
-- with all different sensible levels of currying.
applyForms :: Expression def () -> Expression def () -> [Expression def ()]
applyForms _ e@Expression{ _eBody = BodyLambda {} } =
  [e]
applyForms exprType expr =
  reverse . take (1 + arrows) $ iterate addApply withDepPisApplied
  where
    withDepPisApplied = iterate addApply expr !! depPis
    (depPis, arrows) = countPis exprType
    addApply = pureExpression . (`makeApply` pureHole)
