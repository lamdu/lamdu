{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expr.Utils
  ( makeApply
  , makePi, makeLambda, makeLam
  , pureApply
  , pureHole
  , pureRecord
  , pureLam
  , pureGetField
  , pureLiteralInteger
  , pureIntegerType
  , pureTag
  , pureTagType
  , pureType
  , pureExpr
  , randomizeExpr
  , randomizeParamIds
  , randomizeParamIdsG
  , randomizeExprAndParams
  , NameGen(..), onNgMakeName
  , randomNameGen, debugNameGen
  , matchBody, matchBodyDeprecated
  , matchExpr, matchExprG
  , subExprs, subExprsWithout
  , isDependentPi, exprHasGetVar
  , curriedFuncArguments
  , ApplyFormAnnotation(..), applyForms
  , recordValForm, structureForType
  , alphaEq, couldEq
  , subst, substGetPar
  , showBodyExpr, showsPrecBodyExpr
  , isTypeConstructorType
  , addExprContexts
  , addBodyContexts
  , PiWrappers(..), piWrappersDepParams, piWrappersMIndepParam, piWrappersResultType
  , getPiWrappers
  ) where

import Prelude hiding (pi)
import Lamdu.Data.Expr

import Control.Applicative (Applicative(..), liftA2, (<$>), (<$))
import Control.Arrow ((***))
import Control.Lens (Context(..))
import Control.Lens.Operators
import Control.Lens.Utils (addListContexts, addTuple2Contexts)
import Control.Monad (guard, join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Functor.Identity (Identity(..))
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
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified System.Random as Random

data PiWrappers def a = PiWrappers
  { _piWrappersDepParams :: [(Guid, Expr def a)]
  , _piWrappersMIndepParam :: Maybe (Guid, Expr def a)
  , _piWrappersResultType :: Expr def a
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

getPiWrappers :: Expr def a -> PiWrappers def a
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

couldEq :: Eq def => Expr def a -> Expr def a -> Bool
couldEq x y =
  isJust $ matchExpr (const . Just) onMismatch x y
  where
    onMismatch (Expr (BodyLeaf Hole) _) e = Just e
    onMismatch e (Expr (BodyLeaf Hole) _) = Just e
    onMismatch _ _ = Nothing

alphaEq :: Eq def => Expr def a -> Expr def a -> Bool
alphaEq x y =
  isJust $ matchExpr
  ((const . const . Just) ())
  ((const . const) Nothing)
  x y

-- Useful functions:
substGetPar ::
  Guid ->
  Expr def a ->
  Expr def a ->
  Expr def a
substGetPar from =
  subst (ExprLens.exprParameterRef . Lens.filtered (== from))

subst ::
  Lens.Getting Any (Expr def a) b ->
  Expr def a ->
  Expr def a ->
  Expr def a
subst lens to expr
  | Lens.has lens expr = to
  | otherwise = expr & eBody . traverse %~ subst lens to

recordValForm :: Expr a () -> Maybe (Expr b ())
recordValForm paramType =
  paramType ^? ExprLens.exprKindedRecordFields KType
  >>= replaceFieldTypesWithHoles
  where
    castTag (BodyLeaf Hole) = Just (BodyLeaf Hole)
    castTag (BodyLeaf (Tag tag)) = Just (BodyLeaf (Tag tag))
    castTag _ = Nothing
    replaceFieldTypesWithHoles fields =
      fields
      & Lens.traversed %%~
        (Lens._1 . eBody %%~ castTag) .
        (Lens._2 .~ pureHole)
      <&> (ExprLens.pureExpr . _BodyRecord . ExprLens.kindedRecordFields KVal #)

data ApplyFormAnnotation =
  Untouched | DependentParamAdded | IndependentParamAdded
  deriving Eq

addApply :: ann -> Expr a ann -> Expr b () -> Expr a ann
addApply ann func paramType =
  Expr (makeApply func arg) ann
  where
    arg = ann <$ fromMaybe pureHole (recordValForm paramType)

-- Transform expression to expression applied with holes,
-- with all different sensible levels of currying.
applyForms :: Expr a () -> Expr b () -> [Expr b ApplyFormAnnotation]
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

structureForType ::
  Expr def () ->
  Expr def ()
structureForType =
  (eBody %~) $
  const (ExprLens.bodyHole # ())
  & Lens.outside (ExprLens.bodyKindedRecordFields KType) .~
    (ExprLens.bodyKindedRecordFields KVal # ) . (traverse . Lens._2 %~ structureForType)
  & Lens.outside (ExprLens.bodyKindedLam KType) .~
    (ExprLens.bodyKindedLam KVal # ) . (Lens._3 %~ structureForType)

randomizeExprAndParams :: (RandomGen gen, Random r) => gen -> Expr def (r -> a) -> Expr def a
randomizeExprAndParams gen = randomizeParamIds paramGen . randomizeExpr exprGen
  where
    (exprGen, paramGen) = Random.split gen

randomizeExpr :: (RandomGen gen, Random r) => gen -> Expr def (r -> a) -> Expr def a
randomizeExpr gen (Expr body pl) =
  (`evalState` gen) $ do
    r <- state random
    newBody <- body & traverse %%~ randomizeSubexpr
    return . Expr newBody $ pl r
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

randomizeParamIds :: RandomGen g => g -> Expr def a -> Expr def a
randomizeParamIds gen = randomizeParamIdsG id (randomNameGen gen) Map.empty $ \_ _ a -> a

randomizeParamIdsG ::
  (a -> n) ->
  NameGen n -> Map Guid Guid ->
  (NameGen n -> Map Guid Guid -> a -> b) ->
  Expr def a -> Expr def b
randomizeParamIdsG preNG gen initMap convertPL =
  (`evalState` gen) . (`runReaderT` initMap) . go
  where
    go (Expr v s) = do
      guidMap <- Reader.ask
      newGen <- lift $ state ngSplit
      (`Expr` convertPL newGen guidMap s) <$>
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

matchEq :: (Eq b, Applicative f) => a -> b -> Lens.Prism' a b -> f (Maybe a)
matchEq leaf1 d0 prism = sequenceA $ do
  d1 <- leaf1 ^? Lens.clonePrism prism
  pure (Lens.clonePrism prism # d0) <$ guard (d0 == d1)

matchLeaf ::
  (Eq def, Applicative f) =>
  (par -> par -> f (Maybe par)) ->
  Leaf def par ->
  Leaf def par ->
  f (Maybe (Leaf def par))
matchLeaf matchGetPar leaf0 leaf1 =
  case leaf0 of
    -- TODO: Clean this up
  GetVariable (ParameterRef p0) ->
    fmap join .
    traverse ((fmap . fmap) (ExprLens.parameterRef # ) . matchGetPar p0) $
    leaf1 ^? ExprLens.parameterRef
  GetVariable (DefinitionRef d0) -> matchEq leaf1 d0 ExprLens.definitionRef
  LiteralInteger x               -> matchEq leaf1 x Expr._LiteralInteger
  Type                           -> matchEq leaf1 () Expr._Type
  IntegerType                    -> matchEq leaf1 () Expr._IntegerType
  Hole                           -> matchEq leaf1 () Expr._Hole
  TagType                        -> matchEq leaf1 () Expr._TagType
  Tag x                          -> matchEq leaf1 x Expr._Tag

-- Left-biased on parameter guids
{-# INLINE matchBody #-}
matchBody ::
  (Applicative f, Eq def) =>
  (par -> par -> a -> b -> f (par, c)) ->  -- ^ Lam/Pi result match
  (a -> b -> f c) ->                        -- ^ Ordinary structural match (Apply components, param type)
  (par -> par -> f (Maybe par)) ->         -- ^ Match ParameterRef's
  Body def par a -> Body def par b -> f (Maybe (Body def par c))
matchBody matchLamResult matchOther matchGetPar body0 body1 =
  case body0 of
  BodyLam (Lam k0 p0 pt0 r0) -> sequenceA $ do
    Lam k1 p1 pt1 r1 <- body1 ^? _BodyLam
    guard $ k0 == k1
    let buildLam paramType (param, result) = BodyLam $ Lam k0 param paramType result
    Just $
      buildLam
      <$> matchOther pt0 pt1
      <*> matchLamResult p0 p1 r0 r1
  BodyApply (Apply f0 a0) -> sequenceA $ do
    Apply f1 a1 <- body1 ^? _BodyApply
    Just $ BodyApply <$> (Apply <$> matchOther f0 f1 <*> matchOther a0 a1)
  BodyRecord (Record k0 fs0) -> sequenceA $ do
    Record k1 fs1 <- body1 ^? _BodyRecord
    guard $ k0 == k1
    matchedPairs <- ListUtils.match matchPair fs0 fs1
    Just $ BodyRecord . Record k0 <$> sequenceA matchedPairs
  BodyGetField (GetField r0 f0) -> sequenceA $ do
    GetField r1 f1 <- body1 ^? _BodyGetField
    Just $ BodyGetField <$> (GetField <$> matchOther r0 r1 <*> matchOther f0 f1)
  BodyLeaf leaf0 ->
    fmap join . traverse ((fmap . fmap) BodyLeaf . matchLeaf matchGetPar leaf0) $
    body1 ^? _BodyLeaf
  where
    matchPair (k0, v0) (k1, v1) =
      (,) <$> matchOther k0 k1 <*> matchOther v0 v1

-- TODO: Delete this
matchBodyDeprecated ::
  Eq def =>
  (par -> par -> a -> b -> (par, c)) -> -- ^ Lam/Pi result match
  (a -> b -> c) ->                 -- ^ Ordinary structural match (Apply components, param type)
  (par -> par -> Bool) ->        -- ^ Match ParameterRef's
  Body def par a -> Body def par b -> Maybe (Body def par c)
matchBodyDeprecated matchLamResult matchOther matchGetPar body0 body1 =
  runIdentity $
  matchBody
  (matchLamResult & Lens.mapped . Lens.mapped . Lens.mapped . Lens.mapped %~ Identity)
  (matchOther & Lens.mapped . Lens.mapped %~ Identity)
  (matchGetPar & Lens.imapped <. Lens.mapped %@~ matchGetParMaybe)
  body0 body1
  where
    matchGetParMaybe apar isMatch = Identity $ apar <$ guard isMatch

-- The returned expression gets the same guids as the left
-- expression
{-# INLINE matchExpr #-}
matchExpr ::
  (Eq def, Applicative f) =>
  (a -> b -> f c) ->
  (Expr def a -> Expr def b -> f (Expr def c)) ->
  Expr def a -> Expr def b -> f (Expr def c)
matchExpr = matchExprG . const . const $ pure ()

{-# INLINE matchExprG #-}
matchExprG ::
  (Eq def, Applicative f) =>
  (Guid -> Guid -> f ()) -> -- ^ Left expr guid overrides right expr guid
  (a -> b -> f c) ->
  (Expr def a -> Expr def b -> f (Expr def c)) ->
  Expr def a -> Expr def b -> f (Expr def c)
matchExprG overrideGuids onMatch onMismatch =
  go Map.empty
  where
    go scope e0@(Expr body0 pl0) e1@(Expr body1 pl1) =
      case matchBodyDeprecated matchLamResult matchOther matchGetPar body0 body1 of
      Nothing ->
        onMismatch e0 $
        (ExprLens.exprLeaves . ExprLens.parameterRef %~ lookupGuid) e1
      Just bodyMatched -> Expr <$> sequenceA bodyMatched <*> onMatch pl0 pl1
      where
        matchGetPar p0 p1 = p0 == lookupGuid p1
        matchLamResult p0 p1 r0 r1 = (p0, overrideGuids p0 p1 *> go (Map.insert p1 p0 scope) r0 r1)
        matchOther = go scope
        lookupGuid guid = fromMaybe guid $ Map.lookup guid scope

subExprs :: Expr def a -> [Expr def a]
subExprs x =
  x : Foldable.concatMap subExprs (x ^. eBody)

subExprsWithout ::
  Lens.Traversal' (Expr def (Bool, a)) (Expr def (Bool, a)) ->
  Expr def a -> [Expr def a]
subExprsWithout group =
  map (fmap snd) .
  filter (fst . (^. ePayload)) .
  subExprs .
  (group . ePayload . Lens._1 .~ False) .
  fmap ((,) True)

isDependentPi :: Expr def a -> Bool
isDependentPi =
  Lens.has (ExprLens.exprKindedLam KType . Lens.filtered f)
  where
    f (g, _, resultType) = exprHasGetVar g resultType

parameterRefs :: Lens.Fold (Expr def a) Guid
parameterRefs = Lens.folding subExprs . ExprLens.exprParameterRef

exprHasGetVar :: Guid -> Expr def a -> Bool
exprHasGetVar g = Lens.anyOf parameterRefs (== g)

curriedFuncArguments :: Expr def a -> [Expr def a]
curriedFuncArguments =
  (^.. ExprLens.exprLam . ExprLens.kindedLam KVal . Lens.folding f)
  where
    f (_, paramType, body) = paramType : curriedFuncArguments body

pureIntegerType :: Expr def ()
pureIntegerType = ExprLens.pureExpr . ExprLens.bodyIntegerType # ()

pureTagType :: Expr def ()
pureTagType = ExprLens.pureExpr . ExprLens.bodyTagType # ()

pureType :: Expr def ()
pureType = ExprLens.pureExpr . ExprLens.bodyType # ()

pureTag :: Guid -> Expr def ()
pureTag = (ExprLens.pureExpr . ExprLens.bodyTag # )

pureLiteralInteger :: Integer -> Expr def ()
pureLiteralInteger = (ExprLens.pureExpr . ExprLens.bodyLiteralInteger # )

pureApply :: Expr def () -> Expr def () -> Expr def ()
pureApply f x = ExprLens.pureExpr . _BodyApply # Apply f x

pureHole :: Expr def ()
pureHole = ExprLens.pureExpr . ExprLens.bodyHole # ()

pureRecord :: Kind -> [(Expr def (), Expr def ())] -> Expr def ()
pureRecord k fields = ExprLens.pureExpr . ExprLens.bodyKindedRecordFields k # fields

pureLam :: Kind -> Guid -> Expr def () -> Expr def () -> Expr def ()
pureLam k paramGuid paramType result =
  ExprLens.pureExpr . ExprLens.bodyKindedLam k # (paramGuid, paramType, result)

pureGetField :: Expr def () -> Expr def () -> Expr def ()
pureGetField record field =
  ExprLens.pureExpr . _BodyGetField # GetField record field

-- TODO: Deprecate below here:
pureExpr :: Body def Guid (Expr def ()) -> Expr def ()
pureExpr = (ExprLens.pureExpr # )

makeApply :: expr -> expr -> Body def par expr
makeApply func arg = BodyApply $ Apply func arg

makeLam :: Kind -> Guid -> expr -> expr -> Body def Guid expr
makeLam k argId argType resultType =
  BodyLam $ Lam k argId argType resultType

-- TODO: Remove the kind-passing wrappers
makePi :: Guid -> expr -> expr -> Body def Guid expr
makePi = makeLam KType

makeLambda :: Guid -> expr -> expr -> Body def Guid expr
makeLambda = makeLam KVal

isTypeConstructorType :: Expr def a -> Bool
isTypeConstructorType expr =
  case expr ^. eBody of
  BodyLeaf Type -> True
  BodyLam (Lam KType _ _ res) -> isTypeConstructorType res
  _ -> False

-- Show isntances:
showsPrecBody ::
  (Show def, Show par, Show expr) => (par -> expr -> Bool) ->
  Int -> Body def par expr -> ShowS
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

showsPrecBodyExpr :: (Show def, Show a) => Int -> BodyExpr def Guid a -> ShowS
showsPrecBodyExpr = showsPrecBody exprHasGetVar

showBodyExpr :: BodyExpr String Guid String -> String
showBodyExpr = flip (showsPrecBodyExpr 0) ""

instance (Show def, Show par, Show expr) => Show (Body def par expr) where
  showsPrec = showsPrecBody mayDepend
    where
      -- We are polymorphic on any expr, so we cannot tell...
      mayDepend _ _ = True

instance (Show def, Show a) => Show (Expr def a) where
  showsPrec prec (Expr body payload) =
    showsPrecBodyExpr bodyPrec body .
    showString showPayload
    where
      (bodyPrec, showPayload) =
        case show payload of
        "" -> (prec, "")
        "()" -> (prec, "")
        str -> (11, "{" ++ str ++ "}")

addBodyContexts ::
  (a -> b) -> Context (Body def par a) (Body def par b) container ->
  Body def par (Context a b container)
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

addExprContexts ::
  (a -> b) ->
  Context (Expr def a) (Expr def b) container ->
  Expr def (Context a (Expr def b) container)
addExprContexts atob (Context intoContainer (Expr body a)) =
  Expr newBody (Context intoContainer a)
  where
    newBody =
      addExprContexts atob <$>
      addBodyContexts (fmap atob) bodyPtr
    bodyPtr =
      Context (intoContainer . (`Expr` atob a)) body
