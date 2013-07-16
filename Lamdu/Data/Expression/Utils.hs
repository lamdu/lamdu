{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expression.Utils
  ( makeApply
  , makePi, makeLambda, makeLam
  , pureApply
  , pureHole
  , pureSet
  , pureRecord
  , pureLam
  , pureGetField
  , pureLiteralInteger
  , pureIntegerType
  , pureExpression
  , randomizeExpr
  , randomizeParamIds
  , randomizeParamIdsG
  , randomizeExprAndParams
  , NameGen(..), onNgMakeName
  , randomNameGen, debugNameGen
  , matchBody, matchExpression, matchExpressionG
  , subExpressions, subExpressionsWithout
  , isDependentPi, exprHasGetVar
  , curriedFuncArguments
  , ApplyFormAnnotation(..), applyForms
  , recordValForm, structureForType
  , alphaEq, couldEq
  , subst, substGetPar
  , showBodyExpr, showsPrecBodyExpr
  , isTypeConstructorType
  , addExpressionContexts
  , addBodyContexts
  , PiWrappers(..), piWrappersDepParams, piWrappersMIndepParam, piWrappersResultType
  , getPiWrappers
  ) where

import Prelude hiding (pi)
import Lamdu.Data.Expression

import Control.Applicative (Applicative(..), liftA2, (<$>), (<$))
import Control.Arrow ((***))
import Control.Lens (Context(..))
import Control.Lens.Operators
import Control.Lens.Utils (addListContexts, addTuple2Contexts)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Map (Map)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (Any)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(..), sequenceA)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified System.Random as Random

data PiWrappers def a = PiWrappers
  { _piWrappersDepParams :: [(Guid, Expression def a)]
  , _piWrappersMIndepParam :: Maybe (Guid, Expression def a)
  , _piWrappersResultType :: Expression def a
  }
Lens.makeLenses ''PiWrappers

data NameGen pl = NameGen
  { ngSplit :: (NameGen pl, NameGen pl)
  , ngMakeName :: Guid -> pl -> (Guid, NameGen pl)
  }

onNgMakeName ::
  (NameGen b ->
   (Guid -> a -> (Guid, NameGen b)) ->
   Guid -> b -> (Guid, NameGen b)) ->
  NameGen a -> NameGen b
onNgMakeName onMakeName =
  go
  where
    go nameGen =
      result
      where
        result =
          nameGen
          { ngMakeName =
            ngMakeName nameGen
            & Lens.mapped . Lens.mapped . Lens._2 %~ go
            & onMakeName result
          , ngSplit =
            ngSplit nameGen
            & Lens.both %~ go
          }

getPiWrappers :: Expression def a -> PiWrappers def a
getPiWrappers expr =
  case expr ^? ExprLens.exprLam of
  Just (Lam KType param paramType resultType)
    | isDependentPi expr ->
      getPiWrappers resultType & piWrappersDepParams %~ (p :)
    | otherwise ->
        PiWrappers
        { _piWrappersDepParams = []
        , _piWrappersMIndepParam = Just p
        , _piWrappersResultType = resultType
        }
    where
      p = (param, paramType)
  _ -> PiWrappers [] Nothing expr

couldEq :: Eq def => Expression def a -> Expression def a -> Bool
couldEq x y =
  isJust $ matchExpression (const . Just) onMismatch x y
  where
    onMismatch (Expression (BodyLeaf Hole) _) e = Just e
    onMismatch e (Expression (BodyLeaf Hole) _) = Just e
    onMismatch _ _ = Nothing

alphaEq :: Eq def => Expression def a -> Expression def a -> Bool
alphaEq x y =
  isJust $ matchExpression
  ((const . const . Just) ())
  ((const . const) Nothing)
  x y

-- Useful functions:
substGetPar ::
  Guid ->
  Expression def a ->
  Expression def a ->
  Expression def a
substGetPar from =
  subst (ExprLens.exprParameterRef . Lens.filtered (== from))

subst ::
  Lens.Getting Any (Expression def a) b ->
  Expression def a ->
  Expression def a ->
  Expression def a
subst lens to expr
  | Lens.has lens expr = to
  | otherwise = expr & eBody . traverse %~ subst lens to

data ApplyFormAnnotation =
  Untouched | DependentParamAdded | IndependentParamAdded
  deriving Eq

-- Transform expression to expression applied with holes,
-- with all different sensible levels of currying.
applyForms :: Expression def () -> Expression def () -> [Expression def ApplyFormAnnotation]
applyForms exprType rawExpr
  | Lens.has (ExprLens.exprLam . lamKind . _KVal) expr = [expr]
  | otherwise = reverse withAllAppliesAdded
  where
    expr = Untouched <$ rawExpr
    withDepAppliesAdded =
      foldl (addApply DependentParamAdded) expr depParamTypes
    withAllAppliesAdded =
      scanl (addApply IndependentParamAdded) withDepAppliesAdded $
      indepParamTypes ++ assumeHoleIsPi
    depParamTypes = snd <$> depParams
    indepParamTypes = mNonDepParam ^.. Lens._Just . Lens._2
    assumeHoleIsPi
      | Lens.has ExprLens.exprHole resultType = [pureHole]
      | otherwise = []
    PiWrappers
      { _piWrappersDepParams = depParams
      , _piWrappersMIndepParam = mNonDepParam
      , _piWrappersResultType = resultType
      } = getPiWrappers exprType
    addApply ann func paramType =
      Expression (makeApply func arg) ann
      where
        arg = ann <$ fromMaybe pureHole (recordValForm paramType)

recordValForm :: Expression def () -> Maybe (Expression def ())
recordValForm paramType =
  replaceFieldTypesWithHoles <$>
  (paramType ^? ExprLens.exprKindedRecordFields KType)
  where
    replaceFieldTypesWithHoles fields =
      ExprLens.pureExpr . _BodyRecord .
      ExprLens.kindedRecordFields KVal #
      (fields & Lens.traversed . Lens._2 .~ pureHole)

structureForType ::
  Expression def () ->
  Expression def ()
structureForType =
  (eBody %~) $
  const (ExprLens.bodyHole # ())
  & Lens.outside (ExprLens.bodyKindedRecordFields KType) .~
    (ExprLens.bodyKindedRecordFields KVal # ) . (traverse . Lens._2 %~ structureForType)
  & Lens.outside (ExprLens.bodyKindedLam KType) .~
    (ExprLens.bodyKindedLam KVal # ) . (Lens._3 %~ structureForType)

randomizeExprAndParams :: (RandomGen gen, Random r) => gen -> Expression def (r -> a) -> Expression def a
randomizeExprAndParams gen = randomizeParamIds paramGen . randomizeExpr exprGen
  where
    (exprGen, paramGen) = Random.split gen

randomizeExpr :: (RandomGen gen, Random r) => gen -> Expression def (r -> a) -> Expression def a
randomizeExpr gen (Expression body pl) =
  (`evalState` gen) $ do
    r <- state random
    newBody <- body & traverse %%~ randomizeSubexpr
    return . Expression newBody $ pl r
  where
    randomizeSubexpr subExpr = do
      localGen <- state Random.split
      return $ randomizeExpr localGen subExpr

randomNameGen :: RandomGen g => g -> NameGen dummy
randomNameGen g = NameGen
  { ngSplit = Random.split g & Lens.both %~ randomNameGen
  , ngMakeName = const . const $ random g & Lens._2 %~ randomNameGen
  }

debugNameGen :: NameGen dummy
debugNameGen = ng names ""
  where
    names = (:[]) <$> ['a'..'z']
    ng [] _ = error "TODO: Infinite list of names"
    ng st@(l:ls) suffix =
      NameGen
      { ngSplit = (ng st "_0", ng st "_1")
      , ngMakeName = const . const $ (Guid.fromString (l++suffix), ng ls suffix)
      }

randomizeParamIds :: RandomGen g => g -> Expression def a -> Expression def a
randomizeParamIds gen = randomizeParamIdsG id (randomNameGen gen) Map.empty $ \_ _ a -> a

randomizeParamIdsG ::
  (a -> n) ->
  NameGen n -> Map Guid Guid ->
  (NameGen n -> Map Guid Guid -> a -> b) ->
  Expression def a -> Expression def b
randomizeParamIdsG preNG gen initMap convertPL =
  (`evalState` gen) . (`runReaderT` initMap) . go
  where
    go (Expression v s) = do
      guidMap <- Reader.ask
      newGen <- lift $ state ngSplit
      (`Expression` convertPL newGen guidMap s) <$>
        case v of
        BodyLam (Lam k oldParamId paramType body) -> do
          newParamId <- lift . state $ makeName oldParamId s
          fmap BodyLam $ liftA2 (Lam k newParamId) (go paramType) .
            Reader.local (Map.insert oldParamId newParamId) $ go body
        BodyLeaf (GetVariable (ParameterRef guid)) ->
          pure $ ExprLens.bodyParameterRef #
          fromMaybe guid (Map.lookup guid guidMap)
        x@BodyLeaf {}     -> traverse go x
        x@BodyApply {}    -> traverse go x
        x@BodyGetField {} -> traverse go x
        x@BodyRecord {}   -> traverse go x
    makeName oldParamId s nameGen =
      ngMakeName nameGen oldParamId $ preNG s

-- Left-biased on parameter guids
{-# INLINE matchBody #-}
matchBody ::
  Eq def =>
  (Guid -> Guid -> a -> b -> (Guid, c)) -> -- ^ Lam/Pi result match
  (a -> b -> c) ->                 -- ^ Ordinary structural match (Apply components, param type)
  (Guid -> Guid -> Bool) ->        -- ^ Match ParameterRef's
  Body def a -> Body def b -> Maybe (Body def c)
matchBody matchLamResult matchOther matchGetPar body0 body1 =
  case body0 of
  BodyLam (Lam k0 p0 pt0 r0) -> do
    Lam k1 p1 pt1 r1 <- body1 ^? _BodyLam
    guard $ k0 == k1
    let (p, res) = matchLamResult p0 p1 r0 r1
    return . BodyLam $
      Lam k0 p (matchOther pt0 pt1) res
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
    p1 <- body1 ^? ExprLens.bodyParameterRef
    guard $ matchGetPar p0 p1
    return $ ExprLens.bodyParameterRef # p0
  BodyLeaf x -> do
    y <- body1 ^? _BodyLeaf
    guard $ x == y
    return $ BodyLeaf x
  where
    matchPair (k0, v0) (k1, v1) =
      (matchOther k0 k1, matchOther v0 v1)

-- The returned expression gets the same guids as the left
-- expression
{-# INLINE matchExpression #-}
matchExpression ::
  (Eq def, Applicative f) =>
  (a -> b -> f c) ->
  (Expression def a -> Expression def b -> f (Expression def c)) ->
  Expression def a -> Expression def b -> f (Expression def c)
matchExpression = matchExpressionG . const . const $ pure ()

{-# INLINE matchExpressionG #-}
matchExpressionG ::
  (Eq def, Applicative f) =>
  (Guid -> Guid -> f ()) -> -- ^ Left expr guid overrides right expr guid
  (a -> b -> f c) ->
  (Expression def a -> Expression def b -> f (Expression def c)) ->
  Expression def a -> Expression def b -> f (Expression def c)
matchExpressionG overrideGuids onMatch onMismatch =
  go Map.empty
  where
    go scope e0@(Expression body0 pl0) e1@(Expression body1 pl1) =
      case matchBody matchLamResult matchOther matchGetPar body0 body1 of
      Nothing ->
        onMismatch e0 $
        (ExprLens.exprLeaves . ExprLens.parameterRef %~ lookupGuid) e1
      Just bodyMatched -> Expression <$> sequenceA bodyMatched <*> onMatch pl0 pl1
      where
        matchGetPar p0 p1 = p0 == lookupGuid p1
        matchLamResult p0 p1 r0 r1 = (p0, overrideGuids p0 p1 *> go (Map.insert p1 p0 scope) r0 r1)
        matchOther = go scope
        lookupGuid guid = fromMaybe guid $ Map.lookup guid scope

subExpressions :: Expression def a -> [Expression def a]
subExpressions x =
  x : Foldable.concatMap subExpressions (x ^. eBody)

subExpressionsWithout ::
  Lens.Traversal' (Expression def (Bool, a)) (Expression def (Bool, a)) ->
  Expression def a -> [Expression def a]
subExpressionsWithout group =
  map (fmap snd) .
  filter (fst . (^. ePayload)) .
  subExpressions .
  (group . ePayload . Lens._1 .~ False) .
  fmap ((,) True)

isDependentPi :: Expression def a -> Bool
isDependentPi =
  Lens.has (ExprLens.exprKindedLam KType . Lens.filtered f)
  where
    f (g, _, resultType) = exprHasGetVar g resultType

parameterRefs :: Lens.Fold (Expression def a) Guid
parameterRefs = Lens.folding subExpressions . ExprLens.exprParameterRef

exprHasGetVar :: Guid -> Expression def a -> Bool
exprHasGetVar g = Lens.anyOf parameterRefs (== g)

curriedFuncArguments :: Expression def a -> [Expression def a]
curriedFuncArguments =
  (^.. ExprLens.exprLam . ExprLens.kindedLam KVal . Lens.folding f)
  where
    f (_, paramType, body) = paramType : curriedFuncArguments body

pureIntegerType :: Expression def ()
pureIntegerType = ExprLens.pureExpr . ExprLens.bodyIntegerType # ()

pureLiteralInteger :: Integer -> Expression def ()
pureLiteralInteger = (ExprLens.pureExpr . ExprLens.bodyLiteralInteger # )

pureApply :: Expression def () -> Expression def () -> Expression def ()
pureApply f x = ExprLens.pureExpr . _BodyApply # Apply f x

pureHole :: Expression def ()
pureHole = ExprLens.pureExpr . ExprLens.bodyHole # ()

pureSet :: Expression def ()
pureSet = ExprLens.pureExpr . ExprLens.bodyType # ()

pureRecord :: Kind -> [(Expression def (), Expression def ())] -> Expression def ()
pureRecord k fields = ExprLens.pureExpr . ExprLens.bodyKindedRecordFields k # fields

pureLam :: Kind -> Guid -> Expression def () -> Expression def () -> Expression def ()
pureLam k paramGuid paramType result =
  ExprLens.pureExpr . ExprLens.bodyKindedLam k # (paramGuid, paramType, result)

pureGetField :: Expression def () -> Expression def () -> Expression def ()
pureGetField record field =
  ExprLens.pureExpr . _BodyGetField # GetField record field

-- TODO: Deprecate below here:
pureExpression :: Body def (Expression def ()) -> Expression def ()
pureExpression = (ExprLens.pureExpr # )

makeApply :: expr -> expr -> Body def expr
makeApply func arg = BodyApply $ Apply func arg

makeLam :: Kind -> Guid -> expr -> expr -> Body def expr
makeLam k argId argType resultType =
  BodyLam $ Lam k argId argType resultType

-- TODO: Remove the kind-passing wrappers
makePi :: Guid -> expr -> expr -> Body def expr
makePi = makeLam KType

makeLambda :: Guid -> expr -> expr -> Body def expr
makeLambda = makeLam KVal

isTypeConstructorType :: Expression def a -> Bool
isTypeConstructorType expr =
  case expr ^. eBody of
  BodyLeaf Type -> True
  BodyLam (Lam KType _ _ res) -> isTypeConstructorType res
  _ -> False

-- Show isntances:
showsPrecBody ::
  (Show def, Show expr) => (Guid -> expr -> Bool) ->
  Int -> Body def expr -> ShowS
showsPrecBody mayDepend prec body =
  case body of
  BodyLam (Lam KVal paramId paramType result) ->
    paren 0 $
    showChar '\\' . shows paramId . showChar ':' .
    showsPrec 11 paramType . showString "==>" .
    shows result
  BodyLam (Lam KType paramId paramType resultType) ->
    paren 0 $
    paramStr . showString "->" . shows resultType
    where
      paramStr
        | dependent =
          showString "(" . shows paramId . showString ":" . showsPrec 11 paramType . showString ")"
        | otherwise = showsPrec 1 paramType
      dependent = mayDepend paramId resultType
  BodyApply (Apply func arg) ->
    paren 10 $
    showsPrec 10 func . showChar ' ' . showsPrec 11 arg
  BodyRecord (Record k fields) ->
    paren 11 $ showString recStr
    where
      recStr =
        concat ["Rec", recType k, "{", List.intercalate ", " (map showField fields), "}"]
      showField (field, typ) =
        unwords [show field, sep k, show typ]
      sep KVal = "="
      sep KType = ":"
      recType KVal = "V"
      recType KType = "T"
  BodyGetField (GetField r tag) ->
    paren 8 $ showsPrec 8 r . showChar '.' . showsPrec 9 tag
  BodyLeaf leaf -> showsPrec prec leaf
  where
    paren innerPrec = showParen (prec > innerPrec)

showsPrecBodyExpr :: (Show def, Show a) => Int -> BodyExpr def a -> ShowS
showsPrecBodyExpr = showsPrecBody exprHasGetVar

showBodyExpr :: BodyExpr String String -> String
showBodyExpr = flip (showsPrecBodyExpr 0) ""

instance (Show def, Show expr) => Show (Body def expr) where
  showsPrec = showsPrecBody mayDepend
    where
      -- We are polymorphic on any expr, so we cannot tell...
      mayDepend _ _ = True

instance (Show def, Show a) => Show (Expression def a) where
  showsPrec prec (Expression body payload) =
    showsPrecBodyExpr bodyPrec body .
    showString showPayload
    where
      (bodyPrec, showPayload) =
        case show payload of
        "" -> (prec, "")
        "()" -> (prec, "")
        str -> (11, "{" ++ str ++ "}")

addBodyContexts ::
  (a -> b) -> Context (Body def a) (Body def b) container ->
  Body def (Context a b container)
addBodyContexts tob (Context intoContainer body) =
  afterSetter %~ intoContainer $
  case body of
  BodyLam (Lam k paramId func arg) ->
    Lam k paramId
    (Context (flip (Lam k paramId) (tob arg)) func)
    (Context (Lam k paramId (tob func)) arg)
    & BodyLam
    & afterSetter %~ BodyLam
  BodyApply (Apply func arg) ->
    Apply
    (Context (`Apply` tob arg) func)
    (Context (tob func `Apply`) arg)
    & BodyApply
    & afterSetter %~ BodyApply
  BodyRecord (Record k fields) ->
    (Record k .
     map (addTuple2Contexts tob) .
     addListContexts (tob *** tob))
    (Context (Record k) fields)
    & BodyRecord
    & afterSetter %~ BodyRecord
  BodyGetField (GetField record tag) ->
    GetField
    (Context (`GetField` tob tag) record)
    (Context (tob record `GetField`) tag)
    & BodyGetField
    & afterSetter %~ BodyGetField
  BodyLeaf leaf -> BodyLeaf leaf
  where
    afterSetter = Lens.mapped . Lens.mapped

addExpressionContexts ::
  (a -> b) ->
  Context (Expression def a) (Expression def b) container ->
  Expression def (Context a (Expression def b) container)
addExpressionContexts atob (Context intoContainer (Expression body a)) =
  Expression newBody (Context intoContainer a)
  where
    newBody =
      addExpressionContexts atob <$>
      addBodyContexts (fmap atob) bodyPtr
    bodyPtr =
      Context (intoContainer . (`Expression` atob a)) body
