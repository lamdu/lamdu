{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}
module InferCombinators where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Lens.Tuple
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable)
import DefinitionTypes
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Expr.Scheme (Scheme(..))
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.TypeVars as TypeVars

data ResumptionStep
  -- Any resumptions will have no effect:
  = Final
  -- Only one ResumeWith allowed for given depth (rest must be Final/NewInferred)
  | ResumeWith ExprWithResumptions
  | ResumeOnSide
    {-New expro load/infer-}ExprWithResumptions
    {-Our new inferred-}Resumptions
  -- Some ResumeWith must exist at our level, and it will cause uso
  -- becomehis forhe next level:
  | NewInferred Resumptions

data Resumptions = Resumptions
  { _rTyp :: E.Type
  , _rStep :: ResumptionStep
  }

rTyp :: Lens' Resumptions E.Type
rTyp f ipl = mk <$> f (_rTyp ipl)
  where
    mk x = ipl { _rTyp = x }

rStep :: Lens' Resumptions ResumptionStep
rStep f ipl = mk <$> f (_rStep ipl)
  where
    mk x = ipl { _rStep = x }

-- Like a ZipList but repeats the last elements of all lists infinitely
-- The digits of 1/3 would be represented as: RepeatList "0.3"
data RepeatList a = RRepeat a | RCons a (RepeatList a)
  deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance Applicative RepeatList where
  pure = RRepeat
  RRepeat f <*> RRepeat x = RRepeat (f x)
  RRepeat f <*> RCons x xs = RCons (f x) (RRepeat f <*> xs)
  RCons f fs <*> RRepeat x = RCons (f x) (fs <*> RRepeat x)
  RCons f fs <*> RCons x xs = RCons (f x) (fs <*> xs)

type TypeStream = RepeatList E.Type

typeStream :: Resumptions -> TypeStream
typeStream (Resumptions typ step) =
  case step of
    Final -> RRepeat typ
    ResumeWith expr -> RCons typ $ exprTypeStream expr
    ResumeOnSide _ rs -> RCons typ $ typeStream rs
    NewInferred rs -> RCons typ $ typeStream rs

exprTypeStream :: ExprWithResumptions -> TypeStream
exprTypeStream = typeStream . (^. E.valPayload)

mkExprWithResumptions ::
  E.ValBody ExprWithResumptions -> TypeStream -> ExprWithResumptions
mkExprWithResumptions body types =
  E.Val (go types) body
  where
    go (RRepeat t) = Resumptions t Final
    go (RCons t ts) = Resumptions t $ NewInferred $ go ts

type ExprWithResumptions = E.Val Resumptions

iType :: Lens' ExprWithResumptions E.Type
iType = E.valPayload . rTyp

resumeHere :: ExprWithResumptions -> ExprWithResumptions -> ExprWithResumptions
resumeHere (E.Val (Resumptions typ Final) body) newExpr =
  E.Val (Resumptions typ (ResumeWith newExpr)) body
resumeHere (E.Val (Resumptions _ _) _) _ = error "Contradicting resumptions"

resumedToType :: TypeStream -> TypeStream -> TypeStream
resumedToType (RRepeat t) newTyp = RCons t newTyp
resumedToType _ _ = error "Contradicting type resumptions"

compositeTypeVar :: E.TypeVar (E.CompositeType p) -> RepeatList (E.CompositeType p)
compositeTypeVar ctv = pure $ E.CVar ctv

emptyCompositeType :: RepeatList (E.CompositeType p)
emptyCompositeType = pure $ E.CEmpty

compositeTypeExtend ::
  E.Tag -> TypeStream -> RepeatList E.ProductType -> RepeatList E.ProductType
compositeTypeExtend tag typ base =
  E.CExtend tag <$> typ <*> base

-- TODO: Re-use Subst and re-expose??
instantiate :: Scheme -> [(E.TypeVar E.Type, E.Type)] -> E.Type
instantiate scheme typeVarAssignments =
  onTVars subst (schemeType scheme)
  where
    subst =
      fromMaybe (error "Missing type var assignment") .
      (`lookup` typeVarAssignments)

onTVars :: (E.TypeVar E.Type -> E.Type) -> E.Type -> E.Type
onTVars f (E.TVar v) = f v
onTVars f t = t & E.typeNextLayer %~ onTVars f

glob :: [TypeStream] -> E.GlobalId -> ExprWithResumptions
glob typeVarAssignments globalId =
  mkExprWithResumptions (E.VLeaf (E.VGlobal globalId)) $
  instantiate scheme <$>
  Lens.sequenceAOf (Lens.traversed . _2) typeVarAssignments'
  where
    scheme =
      fromMaybe (error ("global " ++ show globalId ++ " does not exist")) $
      Map.lookup globalId definitionTypes
    schemeVars = Set.toList $ TypeVars.getVars $ schemeForAll scheme
    typeVarAssignments' = zip schemeVars typeVarAssignments

intType :: TypeStream
intType = pure E.intType

literalInteger :: Integer -> ExprWithResumptions
literalInteger x =
  mkExprWithResumptions (E.VLeaf (E.VLiteralInteger x)) intType

-- TODO: Make this take a (TypeStream) (WHICH SHOULD BE NAMED TypeStream)
-- and then make combinators to build type streams?
holeWithInferredType :: TypeStream -> ExprWithResumptions
holeWithInferredType = mkExprWithResumptions (E.VLeaf E.VHole)

typeVar :: E.TypeVar E.Type -> TypeStream
typeVar = pure . TypeVars.liftVar

(~>) :: TypeStream -> TypeStream -> TypeStream
a ~> r = E.TFun <$> a <*> r

lambda ::
  E.ValVar -> TypeStream ->
  (ExprWithResumptions -> ExprWithResumptions) -> ExprWithResumptions
lambda name paramType mkResult =
  mkExprWithResumptions (E.VAbs (E.Lam name result))
  (E.TFun <$> paramType <*> exprTypeStream result)
  where
    result = mkResult $ mkExprWithResumptions (E.VLeaf (E.VVar name)) paramType

getField :: ExprWithResumptions -> E.Tag -> ExprWithResumptions
getField recordVal tag =
  mkExprWithResumptions
  (E.VGetField (E.GetField recordVal tag))
  (findTypeOfField tag <$> exprTypeStream recordVal)

findTypeOfField :: E.Tag -> E.Type -> E.Type
findTypeOfField tag (E.TRecord p) = findTypeOfTagInComposite tag p
findTypeOfField _ _ = error "Test combinators type checking failed in findTypeOfField"

findTypeOfTagInComposite :: E.Tag -> E.CompositeType t -> E.Type
findTypeOfTagInComposite expectedTag (E.CExtend tag typ rest)
  | expectedTag == tag = typ
  | otherwise = findTypeOfTagInComposite expectedTag rest
findTypeOfTagInComposite _ _ = error "Test combinators type checking failed in findTypeOfTagInComposite"

-- TODO: Reuse FlatComposite if it gets exposed:
compositeOfList :: [(E.Tag, E.Type)] -> E.CompositeType t
compositeOfList [] = E.CEmpty
compositeOfList ((tag, typ):rest) = E.CExtend tag typ $ compositeOfList rest

lambdaRecord ::
  E.ValVar -> [(E.Tag, TypeStream)] ->
  ([ExprWithResumptions] -> ExprWithResumptions) -> ExprWithResumptions
lambdaRecord paramsName fields mkResult =
  lambda paramsName recordType $ \params ->
  mkResult $ map (getField params . fst) fields
  where
    recordType = E.TRecord . compositeOfList <$> Lens.sequenceAOf (Lens.traversed . _2) fields

whereItem ::
  E.ValVar -> ExprWithResumptions -> (ExprWithResumptions -> ExprWithResumptions) -> ExprWithResumptions
whereItem name val mkBody = lambda name (exprTypeStream val) mkBody $$ val

-- Uses inferred holes for cons type
nonEmptyList :: [ExprWithResumptions] -> ExprWithResumptions
nonEmptyList [] = error "Given empty list in nonEmptyList"
nonEmptyList items@(x:_) =
  foldr cons nil items
  where
    typ = exprTypeStream x
    cons h t = glob [typ] ":" $$: [h, t]
    nil = glob [typ] "[]"

tInst :: E.TypeId -> [(E.TypeParamId, TypeStream)] -> TypeStream
tInst name =
  fmap (E.TInst name . Map.fromList) . Lens.sequenceAOf (Lens.traversed . _2)

boolType :: TypeStream
boolType = tInst "Bool" []

listOf :: TypeStream -> TypeStream
listOf t = tInst "List" [("val", t)]

maybeOf :: TypeStream -> TypeStream
maybeOf t = tInst "Maybe" [("val", t)]

eRecEmpty :: ExprWithResumptions
eRecEmpty = mkExprWithResumptions (E.VLeaf E.VRecEmpty) $ pure $ E.TRecord E.CEmpty

eRecExtend :: E.Tag -> ExprWithResumptions -> ExprWithResumptions -> ExprWithResumptions
eRecExtend tag v rest =
  mkExprWithResumptions (E.VRecExtend (E.RecExtend tag v rest)) $
  f <$> exprTypeStream v <*> exprTypeStream rest
  where
    f tv (E.TRecord txs) = E.TRecord $ E.CExtend tag tv txs
    f _ _ = error "eRecExtend with non record type"

record :: [(E.Tag, ExprWithResumptions)] -> ExprWithResumptions
record = foldr (uncurry eRecExtend) eRecEmpty

infixl 4 $$
infixl 4 $$:
infixl 4 $.

($.) :: ExprWithResumptions -> E.Tag -> ExprWithResumptions
($.) = getField

($$) :: ExprWithResumptions -> ExprWithResumptions -> ExprWithResumptions
($$) func arg =
  mkExprWithResumptions (E.VApp (E.Apply func arg)) $
  mkType <$> exprTypeStream func <*> exprTypeStream arg
  where
    mkType (E.TFun p r) a
      | p == a = r
      | otherwise = error "Incompatible types in $$"
    mkType _ _ = error "Apply of non-func type!"

($$:) :: ExprWithResumptions -> [ExprWithResumptions] -> ExprWithResumptions
($$:) f args =
  f $$ record (zip tags args)
  where
    tags =
      case f ^. iType of
      E.TFun (E.TRecord p) _ -> compositeTags p
      _ -> error "not a record func in ($$:)"

compositeTags :: E.CompositeType p -> [E.Tag]
compositeTags E.CEmpty = []
compositeTags E.CVar {} = error "unknown tags in compositeTags"
compositeTags (E.CExtend t _ r) = t : compositeTags r
