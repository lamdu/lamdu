{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}
module InferCombinators where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Lens.Tuple
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Traversable (Traversable)
import DefinitionTypes
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..))
import Lamdu.Expr.Val (Val(..))
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Scheme as S
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import qualified Text.PrettyPrint as PP

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
  { _rTyp :: Type
  , _rStep :: ResumptionStep
  }

rTyp :: Lens' Resumptions Type
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

type TypeStream = RepeatList Type

typeStream :: Resumptions -> TypeStream
typeStream (Resumptions typ step) =
  case step of
    Final -> RRepeat typ
    ResumeWith expr -> RCons typ $ exprTypeStream expr
    ResumeOnSide _ rs -> RCons typ $ typeStream rs
    NewInferred rs -> RCons typ $ typeStream rs

exprTypeStream :: ExprWithResumptions -> TypeStream
exprTypeStream = typeStream . (^. V.payload)

mkExprWithResumptions ::
  V.Body ExprWithResumptions -> TypeStream -> ExprWithResumptions
mkExprWithResumptions body types =
  Val (go types) body
  where
    go (RRepeat t) = Resumptions t Final
    go (RCons t ts) = Resumptions t $ NewInferred $ go ts

type ExprWithResumptions = Val Resumptions

iType :: Lens' ExprWithResumptions Type
iType = V.payload . rTyp

resumeHere :: ExprWithResumptions -> ExprWithResumptions -> ExprWithResumptions
resumeHere (Val (Resumptions typ Final) body) newExpr =
  Val (Resumptions typ (ResumeWith newExpr)) body
resumeHere (Val (Resumptions _ _) _) _ = error "Contradicting resumptions"

resumedType :: TypeStream -> TypeStream -> TypeStream
resumedType (RRepeat t) newTyp = RCons t newTyp
resumedType _ _ = error "Contradicting type resumptions"

compositeTypeVar :: T.Var (T.Composite p) -> RepeatList (T.Composite p)
compositeTypeVar ctv = pure $ T.CVar ctv

emptyCompositeType :: RepeatList (T.Composite p)
emptyCompositeType = pure $ T.CEmpty

compositeTypeExtend ::
  T.Tag -> TypeStream ->
  RepeatList (T.Composite T.Product) ->
  RepeatList (T.Composite T.Product)
compositeTypeExtend tag typ base =
  T.CExtend tag <$> typ <*> base

-- TODO: Re-use Subst and re-expose??
instantiate :: Scheme -> [(T.Var Type, Type)] -> Type
instantiate scheme typeVarAssignments =
  onTVars subst (schemeType scheme)
  where
    subst =
      fromMaybe (error "Missing type var assignment") .
      (`lookup` typeVarAssignments)

onTVars :: (T.Var Type -> Type) -> Type -> Type
onTVars f (T.TVar v) = f v
onTVars f t = t & T.nextLayer %~ onTVars f

glob :: [TypeStream] -> V.GlobalId -> ExprWithResumptions
glob typeVarAssignments globalId
  | Set.null rtvs =
    mkExprWithResumptions (V.BLeaf (V.LGlobal globalId)) $
    instantiate scheme <$>
    Lens.sequenceAOf (Lens.traversed . _2) typeVarAssignments'
  | otherwise = error "TODO: Handle record type vars in globals"
  where
    scheme =
      fromMaybe (error ("global " ++ show globalId ++ " does not exist")) $
      Map.lookup globalId definitionTypes
    TypeVars tvs rtvs = schemeForAll scheme
    typeVarAssignments' = zip (Set.toList tvs) typeVarAssignments

intType :: TypeStream
intType = pure T.int

literalInteger :: Integer -> ExprWithResumptions
literalInteger x =
  mkExprWithResumptions (V.BLeaf (V.LLiteralInteger x)) intType

-- TODO: Make this take a (TypeStream) (WHICH SHOULD BE NAMED TypeStream)
-- and then make combinators to build type streams?
holeWithInferredType :: TypeStream -> ExprWithResumptions
holeWithInferredType = mkExprWithResumptions (V.BLeaf V.LHole)

typeVar :: T.Var Type -> TypeStream
typeVar = pure . T.liftVar

(~>) :: TypeStream -> TypeStream -> TypeStream
a ~> r = T.TFun <$> a <*> r

lambdaConstrained ::
  V.Var -> Scheme -> TypeStream ->
  (ExprWithResumptions -> ExprWithResumptions) -> ExprWithResumptions
lambdaConstrained name paramTypeConstraint paramType mkResult =
  mkExprWithResumptions (V.BAbs (V.Lam name paramTypeConstraint result))
  (T.TFun <$> paramType <*> exprTypeStream result)
  where
    result = mkResult $ mkExprWithResumptions (V.BLeaf (V.LVar name)) paramType

lambda ::
  V.Var -> TypeStream ->
  (ExprWithResumptions -> ExprWithResumptions) -> ExprWithResumptions
lambda name = lambdaConstrained name S.any

getField :: ExprWithResumptions -> T.Tag -> ExprWithResumptions
getField recordVal tag =
  mkExprWithResumptions
  (V.BGetField (V.GetField recordVal tag))
  (findTypeOfField tag <$> exprTypeStream recordVal)

findTypeOfField :: T.Tag -> Type -> Type
findTypeOfField tag (T.TRecord p) = findTypeOfTagInComposite tag p
findTypeOfField _ _ = error "Test combinators type checking failed in findTypeOfField"

findTypeOfTagInComposite :: T.Tag -> T.Composite t -> Type
findTypeOfTagInComposite expectedTag (T.CExtend tag typ rest)
  | expectedTag == tag = typ
  | otherwise = findTypeOfTagInComposite expectedTag rest
findTypeOfTagInComposite _ _ = error "Test combinators type checking failed in findTypeOfTagInComposite"

-- TODO: Reuse FlatComposite if it gets exposed:
compositeOfList :: [(T.Tag, Type)] -> T.Composite t
compositeOfList [] = T.CEmpty
compositeOfList ((tag, typ):rest) = T.CExtend tag typ $ compositeOfList rest

lambdaRecordConstrained ::
  V.Var -> Scheme -> [(T.Tag, TypeStream)] ->
  ([ExprWithResumptions] -> ExprWithResumptions) -> ExprWithResumptions
lambdaRecordConstrained paramsName paramTypeConstraint fields mkResult =
  lambdaConstrained paramsName paramTypeConstraint recordType $ \params ->
  mkResult $ map (getField params . fst) fields
  where
    recordType = T.TRecord . compositeOfList <$> Lens.sequenceAOf (Lens.traversed . _2) fields

lambdaRecord ::
  V.Var -> [(T.Tag, TypeStream)] ->
  ([ExprWithResumptions] -> ExprWithResumptions) -> ExprWithResumptions
lambdaRecord paramsName = lambdaRecordConstrained paramsName S.any

lambdaRecordConstrainedTags ::
  V.Var -> [(T.Tag, TypeStream)] ->
  ([ExprWithResumptions] -> ExprWithResumptions) -> ExprWithResumptions
lambdaRecordConstrainedTags paramsName fields mkResult =
  lambdaRecordConstrained paramsName s fields mkResult
  where
    s = S.make mempty recType
    recType = T.TRecord . foldr (uncurry T.CExtend) T.CEmpty $ zip fieldNames infiniteVars
    fieldNames = map fst fields
    infiniteVars = map (T.liftVar . fromString . (:[])) ['a'..]

whereItem ::
  V.Var -> ExprWithResumptions -> (ExprWithResumptions -> ExprWithResumptions) -> ExprWithResumptions
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

tInst :: T.Id -> [(T.ParamId, TypeStream)] -> TypeStream
tInst name =
  fmap (T.TInst name . Map.fromList) . Lens.sequenceAOf (Lens.traversed . _2)

boolType :: TypeStream
boolType = tInst "Bool" []

listOf :: TypeStream -> TypeStream
listOf t = tInst "List" [("val", t)]

maybeOf :: TypeStream -> TypeStream
maybeOf t = tInst "Maybe" [("val", t)]

eRecEmpty :: ExprWithResumptions
eRecEmpty = mkExprWithResumptions (V.BLeaf V.LRecEmpty) $ pure $ T.TRecord T.CEmpty

eRecExtend :: T.Tag -> ExprWithResumptions -> ExprWithResumptions -> ExprWithResumptions
eRecExtend tag v rest =
  mkExprWithResumptions (V.BRecExtend (V.RecExtend tag v rest)) $
  f <$> exprTypeStream v <*> exprTypeStream rest
  where
    f tv (T.TRecord txs) = T.TRecord $ T.CExtend tag tv txs
    f _ _ = error "eRecExtend with non record type"

record :: [(T.Tag, ExprWithResumptions)] -> ExprWithResumptions
record = foldr (uncurry eRecExtend) eRecEmpty

infixl 4 $$
infixl 4 $$:
infixl 4 $.

($.) :: ExprWithResumptions -> T.Tag -> ExprWithResumptions
($.) = getField

($$) :: ExprWithResumptions -> ExprWithResumptions -> ExprWithResumptions
($$) func arg =
  mkExprWithResumptions (V.BApp (V.Apply func arg)) $
  mkType <$> exprTypeStream func <*> exprTypeStream arg
  where
    mkType (T.TFun p r) a
      | p == a = r
      | otherwise =
        error $
        "Incompatible types in '" ++
        show (V.pPrintUnannotated func <+> PP.text "$$" <+> V.pPrintUnannotated arg) ++
        "' param is " ++
        show (pPrint p) ++ " and arg is " ++ show (pPrint a)
    mkType _ _ = error "Apply of non-func type!"

($$:) :: ExprWithResumptions -> [ExprWithResumptions] -> ExprWithResumptions
($$:) f args =
  f $$ record (zip tags args)
  where
    tags =
      case f ^. iType of
      T.TFun (T.TRecord p) _ -> compositeTags p
      _ -> error "not a record func in ($$:)"

compositeTags :: T.Composite p -> [T.Tag]
compositeTags T.CEmpty = []
compositeTags T.CVar {} = error "unknown tags in compositeTags"
compositeTags (T.CExtend t _ r) = t : compositeTags r
