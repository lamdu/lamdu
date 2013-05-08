{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction #-}

module Lamdu.Data.Expression.Utils
  ( makeApply, pureApply
  , makePi, makeLambda, makeLam
  , pureHole
  , pureSet
  , bodyParameterRef, bodyDefinitionRef
  , bodyLiteralInteger, pureLiteralInteger
  , bodyHole
  , pureIntegerType
  , _PureExpr, _PureTagExpr
  , pureExpression
  , randomizeExpr
  , canonizeParamIds, randomizeParamIds
  , matchBody, matchExpression
  , subExpressions, subExpressionsWithoutTags
  , isDependentPi
  , funcArguments
  , applyForms, applyDependentPis
  -- Traversals
  , bitraverseExpression
  , bitraverseBody
  , expressionBodyDef
  , expressionDef
  ) where

import Lamdu.Data.Expression

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Lens (Prism, Prism', (^.), (.~), (^?), (%~), (&))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(..), sequenceA)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified System.Random as Random

makeApply :: expr -> expr -> Body def expr
makeApply func arg = BodyApply $ Apply func arg

makeLam :: Kind -> Guid -> expr -> expr -> Body def expr
makeLam k argId argType resultType =
  BodyLam $ Lambda k argId argType resultType

-- TODO: Remove this? Take Type/Val as arg
makePi :: Guid -> expr -> expr -> Body def expr
makePi = makeLam Type

makeLambda :: Guid -> expr -> expr -> Body def expr
makeLambda = makeLam Val

bodyParameterRef :: Prism' (Body def expr) Guid
bodyParameterRef = _BodyLeaf . _GetVariable . _ParameterRef

bodyDefinitionRef :: Prism (Body defa expr) (Body defb expr) defa defb
bodyDefinitionRef = _BodyLeaf . _GetVariable . _DefinitionRef

bodyLiteralInteger :: Prism' (Body def expr) Integer
bodyLiteralInteger = _BodyLeaf . _LiteralInteger

bodyHole :: Prism' (Body def expr) ()
bodyHole = _BodyLeaf . _Hole

bitraverseBody ::
  Applicative f => (defa -> f defb) -> (expra -> f exprb) ->
  Body defa expra ->
  f (Body defb exprb)
bitraverseBody onDef onExpr body =
  case body of
  BodyLam x -> BodyLam <$> traverse onExpr x
  BodyApply x -> BodyApply <$> traverse onExpr x
  BodyRecord x -> BodyRecord <$> traverse onExpr x
  BodyGetField x -> BodyGetField <$> traverse onExpr x
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

-- TODO: Deprecate:
pureExpression :: Body def (Expression def ()) -> Expression def ()
pureExpression = (`Expression` ())

_PureExpr :: Lens.Iso' (Expression def ()) (Body def (Expression def ()))
_PureExpr = Lens.iso (^. eBody) (`Expression` ())

_PureTagExpr :: Lens.Prism' (Expression def ()) Guid
_PureTagExpr = _PureExpr . _BodyLeaf . _Tag

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
      BodyLam (Lambda k oldParamId paramType body) -> do
        newParamId <- lift $ state random
        fmap BodyLam $ liftA2 (Lambda k newParamId) (go paramType) .
          Reader.local (Map.insert oldParamId newParamId) $ go body
      gv@(BodyLeaf (GetVariable (ParameterRef guid))) ->
        Reader.asks $
        maybe gv (Lens.review bodyParameterRef) .
        Map.lookup guid
      x@BodyLeaf {}     -> return x
      x@BodyApply {}    -> traverse go x
      x@BodyGetField {} -> traverse go x
      x@BodyRecord {}   -> traverse go x

-- Left-biased on parameter guids
{-# INLINE matchBody #-}
matchBody ::
  Eq def =>
  (Guid -> Guid -> a -> b -> c) -> -- ^ Lambda/Pi result match
  (a -> b -> c) ->                 -- ^ Ordinary structural match (Apply components, param type)
  (Guid -> Guid -> Bool) ->        -- ^ Match ParameterRef's
  Body def a -> Body def b -> Maybe (Body def c)
matchBody matchLamResult matchOther matchGetPar body0 body1 =
  case body0 of
  BodyLam (Lambda k0 p0 pt0 r0) -> do
    Lambda k1 p1 pt1 r1 <- body1 ^? _BodyLam
    guard $ k0 == k1
    return . BodyLam $
      Lambda k0 p0 (matchOther pt0 pt1) $
      matchLamResult p0 p1 r0 r1
  BodyApply (Apply f0 a0) -> do
    Apply f1 a1 <- body1 ^? _BodyApply
    return . BodyApply $ Apply (matchOther f0 f1) (matchOther a0 a1)
  BodyRecord (Record k0 fs0) -> do
    Record k1 fs1 <- body1 ^? _BodyRecord
    guard $ k0 == k1
    BodyRecord . Record k0 <$> ListUtils.match matchPair fs0 fs1
  BodyGetField (GetField r0 f0) -> do
    GetField r1 f1 <- body1 ^? _BodyGetField
    return . BodyGetField $ GetField (matchOther r0 r1) (matchOther f0 f1)
  BodyLeaf (GetVariable (ParameterRef p0)) -> do
    p1 <- body1 ^? bodyParameterRef
    guard $ matchGetPar p0 p1
    return $ Lens.review bodyParameterRef p0
  BodyLeaf x -> do
    y <- body1 ^? _BodyLeaf
    guard $ x == y
    return $ BodyLeaf x
  where
    matchPair (k0, v0) (k1, v1) =
      (matchOther k0 k1, matchOther v0 v1)

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
        (expressionLeaves . _GetVariable . _ParameterRef %~ lookupGuid) e1
      Just bodyMatched -> Expression <$> sequenceA bodyMatched <*> onMatch pl0 pl1
      where
        matchGetPar p0 p1 = p0 == lookupGuid p1
        matchLamResult p0 p1 = go $ Map.insert p1 p0 scope
        matchOther = go scope
        lookupGuid guid = fromMaybe guid $ Map.lookup guid scope

expressionLeaves ::
  Lens.Traversal (Expression defa a) (Expression defb a) (Leaf defa) (Leaf defb)
expressionLeaves = eBody . bodyLeaves expressionLeaves

bodyLeaves ::
  Applicative f =>
  Lens.LensLike f expra exprb (Leaf defa) (Leaf defb) ->
  Lens.LensLike f (Body defa expra) (Body defb exprb) (Leaf defa) (Leaf defb)
bodyLeaves exprLeaves onLeaves body =
  case body of
  BodyLam x      -> BodyLam      <$> onExprs x
  BodyApply x    -> BodyApply    <$> onExprs x
  BodyRecord x   -> BodyRecord   <$> onExprs x
  BodyGetField x -> BodyGetField <$> onExprs x
  BodyLeaf l -> BodyLeaf <$> onLeaves l
  where
    onExprs = traverse (exprLeaves onLeaves)

subExpressions :: Expression def a -> [Expression def a]
subExpressions x =
  x : Foldable.concatMap subExpressions (x ^. eBody)

subExpressionsWithoutTags :: Expression def a -> [Expression def a]
subExpressionsWithoutTags x =
  x :
  case x ^. eBody of
  BodyGetField (GetField record _) -> subExpressionsWithoutTags record
  BodyRecord (Record _ fields) -> concatMap subExpressionsWithoutTags (map snd fields)
  body -> Foldable.concatMap subExpressionsWithoutTags body

hasGetVar :: Guid -> Expression def a -> Bool
hasGetVar =
  Lens.anyOf
  ( Lens.folding subExpressions
  . eBody . bodyParameterRef
  ) . (==)

isDependentPi :: Expression def a -> Bool
isDependentPi =
  Lens.anyOf (eBody . _BodyLam) f
  where
    f (Lambda Type g _ resultType) = hasGetVar g resultType
    f _ = False

funcArguments :: Expression def a -> [Expression def a]
funcArguments =
  Lens.toListOf (eBody . _BodyLam . Lens.folding f)
  where
    f (Lambda Val _ paramType body) =
      paramType : funcArguments body
    f _ = []

getParams :: Expression def a -> [(Guid, Expression def a)]
getParams expr =
  case expr ^? eBody . _BodyLam of
  Just (Lambda Type param paramType resultType) ->
    (param, paramType) : getParams resultType
  _ -> []

data PiWrappers def a = PiWrappers
  { _dependentPiParams :: [(Guid, Expression def a)]
  , nonDependentPiParams :: [(Guid, Expression def a)]
  }
LensTH.makeLenses ''PiWrappers

-- TODO: Return a record, not a tuple
getPiWrappers :: Expression def a -> PiWrappers def a
getPiWrappers expr =
  case expr ^? eBody . _BodyLam of
  Just (Lambda Type param paramType resultType)
    | isDependentPi expr ->
      getPiWrappers resultType & dependentPiParams %~ addParam
    | otherwise ->
        PiWrappers
        { _dependentPiParams = []
        , nonDependentPiParams = addParam (getParams resultType)
        }
    where
      addParam = ((param, paramType) :)
  _ -> PiWrappers [] []

getDependentParams :: Expression def a -> [(Guid, Expression def a)]
getDependentParams = (^. dependentPiParams) . getPiWrappers

compose :: [a -> a] -> a -> a
compose = foldr (.) id
{-# INLINE compose #-}

applyWith :: [Expression def ()] -> Expression def () -> Expression def ()
applyWith =
  compose . map addApply
  where
    addApply arg = pureExpression . (`makeApply` arg)

applyWithHoles :: Int -> Expression def () -> Expression def ()
applyWithHoles count = applyWith $ replicate count pureHole

applyDependentPis :: Expression def () -> Expression def () -> Expression def ()
applyDependentPis exprType = applyWithHoles (length (getDependentParams exprType))

-- Transform expression to expression applied with holes,
-- with all different sensible levels of currying.
applyForms :: Expression def () -> Expression def () -> [Expression def ()]
applyForms exprType expr
  | Lens.notNullOf (eBody . _BodyLam . lambdaKind . _Val) expr = [expr]
  | otherwise = reverse $ scanl (flip addApply) withDepPisApplied nonDepParams
  where
    withDepPisApplied = applyWithHoles (length depParams) expr
    PiWrappers
      { _dependentPiParams = depParams
      , nonDependentPiParams = nonDepParams
      } = getPiWrappers exprType
    addApply (_, paramType) =
      pureExpression . (`makeApply` arg)
      where
        arg =
          case paramType ^? eBody . _BodyRecord of
          Just (Record Type fields) ->
            pureExpression . BodyRecord . Record Val $
            fields & Lens.mapped . Lens._2 .~ pureHole
          _ -> pureHole
