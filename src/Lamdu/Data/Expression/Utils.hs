{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Lamdu.Data.Expression.Utils
  ( makeApply, pureApply
  , makePi, makeLambda, makeLam
  , pureHole
  , pureSet
  , bodyLambda, bodyPi
  , bodyParameterRef, bodyDefinitionRef
  , bodyLiteralInteger, pureLiteralInteger
  , pureIntegerType
  , pureExpression
  , randomizeExpr
  , canonizeParamIds, randomizeParamIds
  , matchBody, matchExpression
  , subExpressions
  , isDependentPi
  , funcArguments
  , applyForms, applyDependentPis
  -- Traversals
  , bitraverseExpression
  , bitraverseBody
  , expressionBodyDef
  , expressionDef
  , exprLeaves
  , bodyLeaves
  ) where

import Lamdu.Data.Expression

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Lens (Prism, Prism', (^.), (^?), (+~), (%~))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(..), sequenceA)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified System.Random as Random

makeApply :: expr -> expr -> Body def expr
makeApply func arg = BodyApply $ Apply func arg

makeLam :: LamKind -> Guid -> expr -> expr -> Body def expr
makeLam k argId argType resultType =
  BodyLam k $ Lambda argId argType resultType

-- TODO: Remove this? Take KindPi/KindLambda as arg
makePi :: Guid -> expr -> expr -> Body def expr
makePi = makeLam KindPi

makeLambda :: Guid -> expr -> expr -> Body def expr
makeLambda = makeLam KindLambda

bodyParameterRef :: Prism' (Body def expr) Guid
bodyParameterRef = _BodyLeaf . _GetVariable . _ParameterRef

bodyDefinitionRef :: Prism (Body defa expr) (Body defb expr) defa defb
bodyDefinitionRef = _BodyLeaf . _GetVariable . _DefinitionRef

bodyLiteralInteger :: Prism' (Body def expr) Integer
bodyLiteralInteger = _BodyLeaf . _LiteralInteger

bitraverseBody ::
  Applicative f => (defa -> f defb) -> (expra -> f exprb) ->
  Body defa expra ->
  f (Body defb exprb)
bitraverseBody onDef onExpr body =
  case body of
  BodyLam k x -> BodyLam k <$> traverse onExpr x
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
    go (Expression v s) = fmap (`Expression` s) $
      case v of
      BodyLam k (Lambda oldParamId paramType body) -> do
        newParamId <- lift $ state random
        fmap (BodyLam k) $ liftA2 (Lambda newParamId) (go paramType) .
          Reader.local (Map.insert oldParamId newParamId) $ go body
      BodyApply (Apply func arg) -> liftA2 makeApply (go func) (go arg)
      gv@(BodyLeaf (GetVariable (ParameterRef guid))) ->
        Reader.asks $
        maybe gv (Lens.review bodyParameterRef) .
        Map.lookup guid
      _ -> return v

-- Left-biased on parameter guids
{-# INLINE matchBody #-}
matchBody ::
  Eq def =>
  (Guid -> Guid -> a -> b -> c) -> -- ^ Lambda/Pi result match
  (a -> b -> c) ->                 -- ^ Ordinary structural match (Apply components, param type)
  (Guid -> Guid -> Bool) ->        -- ^ Match ParameterRef's
  Body def a -> Body def b -> Maybe (Body def c)
matchBody matchLamResult matchOther matchGetPar body0 body1 =
  case (body0, body1) of
  (BodyLam k0 (Lambda p0 pt0 r0), BodyLam k1 (Lambda p1 pt1 r1))
    | k0 == k1 ->
      Just . BodyLam k0 $
      Lambda p0 (matchOther pt0 pt1) $
      matchLamResult p0 p1 r0 r1
  (BodyApply (Apply f0 a0), BodyApply (Apply f1 a1)) ->
    Just . BodyApply $ Apply (matchOther f0 f1) (matchOther a0 a1)
  (BodyLeaf (GetVariable (ParameterRef p0)),
   BodyLeaf (GetVariable (ParameterRef p1)))
    | matchGetPar p0 p1
      -> Just $ Lens.review bodyParameterRef p0
  (BodyLeaf x, BodyLeaf y)
    | x == y -> Just $ BodyLeaf x
  _ -> Nothing

-- TODO: Generalize to defa/defb/defc with hof's to handle matching
-- them?  The returned expression gets the same guids as the left
-- expression
{-# INLINE matchExpression #-}
matchExpression ::
  (Eq def, Applicative f) =>
  (a -> b -> f c) ->
  (Expression def a -> Expression def b -> f (Expression def c)) ->
  Expression def a -> Expression def b -> f (Expression def c)
matchExpression onMatch onMismatch =
  go Map.empty
  where
    go scope e0@(Expression body0 pl0) e1@(Expression body1 pl1) =
      case matchBody matchLamResult matchOther matchGetPar body0 body1 of
      Nothing ->
        onMismatch e0 $
        (exprLeaves . _GetVariable . _ParameterRef %~ lookupGuid) e1
      Just bodyMatched -> Expression <$> sequenceA bodyMatched <*> onMatch pl0 pl1
      where
        matchGetPar p0 p1 = p0 == lookupGuid p1
        matchLamResult p0 p1 = go $ Map.insert p1 p0 scope
        matchOther = go scope
        lookupGuid guid = fromMaybe guid $ Map.lookup guid scope

exprLeaves ::
  Lens.Traversal (Expression defa a) (Expression defb a) (Leaf defa) (Leaf defb)
exprLeaves = eBody . bodyLeaves exprLeaves

bodyLeaves ::
  Applicative f =>
  Lens.LensLike f expra exprb (Leaf defa) (Leaf defb) ->
  Lens.LensLike f (Body defa expra) (Body defb exprb) (Leaf defa) (Leaf defb)
bodyLeaves recu onLeaves body =
  case body of
  BodyLam k (Lambda paramName paramType result) ->
    BodyLam k <$> (Lambda paramName <$> recLeaves paramType <*> recLeaves result)
  BodyApply (Apply func arg) ->
    BodyApply <$> (Apply <$> recLeaves func <*> recLeaves arg)
  BodyLeaf l -> BodyLeaf <$> onLeaves l
  where
    recLeaves = recu onLeaves

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

countArrows :: Expression def () -> Int
countArrows expr =
  case expr ^? eBody . bodyPi . lambdaBody of
  Just resultType -> 1 + countArrows resultType
  Nothing -> 0

countDependentPis :: Expression def () -> Int
countDependentPis expr =
  case expr ^? eBody . bodyPi . lambdaBody of
  Just resultType
    | isDependentPi expr -> 1 + countDependentPis resultType
  _ -> 0

-- TODO: Return a record, not a tuple
countPis :: Expression def () -> (Int, Int)
countPis expr =
  case expr ^? eBody . bodyPi . lambdaBody of
  Just resultType
    | isDependentPi expr -> Lens._1 +~ 1 $ countPis resultType
    | otherwise -> (0, 1 + countArrows resultType)
  Nothing -> (0, 0)

applyWithHoles :: Int -> Expression def () -> Expression def ()
applyWithHoles count expr =
  iterate addApply expr !! count
  where
    addApply = pureExpression . (`makeApply` pureHole)

applyDependentPis :: Expression def () -> Expression def () -> Expression def ()
applyDependentPis exprType = applyWithHoles (countDependentPis exprType)

-- Transform expression to expression applied with holes,
-- with all different sensible levels of currying.
applyForms :: Expression def () -> Expression def () -> [Expression def ()]
applyForms exprType expr
  | Lens.notNullOf (eBody . bodyLambda) expr = [expr]
  | otherwise =
    reverse . take (1 + arrows) $ iterate addApply withDepPisApplied
  where
    withDepPisApplied = applyWithHoles depPis expr
    (depPis, arrows) = countPis exprType
    addApply = pureExpression . (`makeApply` pureHole)

bodyLambda :: Lens.Prism' (Body def a) (Lambda a)
bodyLambda =
  Lens.prism' (BodyLam KindLambda) unBody
  where
    unBody (BodyLam KindLambda l) = Just l
    unBody _ = Nothing

bodyPi :: Lens.Prism' (Body def a) (Lambda a)
bodyPi =
  Lens.prism' (BodyLam KindPi) unBody
  where
    unBody (BodyLam KindPi l) = Just l
    unBody _ = Nothing
