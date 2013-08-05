{-# LANGUAGE TemplateHaskell, DeriveFunctor#-}
module InferCombinators where

import Control.Applicative (Applicative(..), ZipList(..), liftA2, (<$>))
import Control.Arrow ((***))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void)
import Data.Map ((!))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Kind(..))
import Lamdu.Data.Expression.Utils (pureHole, pureLiteralInteger, pureIntegerType, pureTag, pureTagType, pureType)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type InputExpr = Expr.Expression Def InputPayload

data Resumption
  -- Any resumptions will have no effect:
  = Same
  -- Only one ResumeWith allowed for given depth (rest must be Same/NewInferred)
  | ResumeWith InputExpr
  | ResumeOnSide {-New expr to load/infer-}InputExpr {-Our new inferred-}InputPayload
  -- Some ResumeWith must exist at our level, and it will cause us to
  -- become this for the next level:
  | NewInferred InputPayload

data InputPayload = InputPayload
  { _ipVal :: Expr ()
  , _ipTyp :: Expr ()
  , _ipResumption :: Resumption
  }
Lens.makeLenses ''InputPayload

inferredOfInput :: InputPayload -> (Expr (), Expr ())
inferredOfInput (InputPayload val typ _) = (val, typ)

addResumption :: Resumption -> InputExpr -> InputExpr
addResumption x a =
  a & Expr.ePayload . ipResumption %~ resumption
  where
    resumption Same = x
    resumption _ = error "Contradicting resumptions"

resumeHere :: InputExpr -> InputExpr -> InputExpr
resumeHere x newExpr = addResumption (ResumeWith newExpr) x

inputExprInferredNow :: InputExpr -> InputPayload
inputExprInferredNow x = InputPayload (x ^. iVal) (x ^. iType) Same

resumeOnSide :: InputExpr -> InputExpr -> InputExpr
resumeOnSide x newExpr =
  addResumption (ResumeOnSide newExpr (inputExprInferredNow x)) x

resumedTo :: InputExpr -> InputExpr -> InputExpr
resumedTo x newVal = addResumption (NewInferred (inputExprInferredNow newVal)) x

resumedToType :: InputExpr -> InputExpr -> InputExpr
resumedToType x newTyp = addResumption (NewInferred (InputPayload (x ^. iVal) (newTyp ^. iVal) Same)) x

iVal :: Lens' InputExpr (Expr ())
iVal = Expr.ePayload . ipVal

iType :: Lens' InputExpr (Expr ())
iType = Expr.ePayload . ipTyp

tag :: Guid -> InputExpr
tag guid =
  Expr.Expression (ExprLens.bodyTag # guid) (InputPayload (pureTag guid) pureTagType Same)

set :: InputExpr
set =
  Expr.Expression (ExprLens.bodyType # ()) (InputPayload pureType pureType Same)

tagType :: InputExpr
tagType =
  Expr.Expression (ExprLens.bodyTagType # ()) (InputPayload pureTagType pureType Same)

integerType :: InputExpr
integerType =
  Expr.Expression (ExprLens.bodyIntegerType # ()) (InputPayload pureIntegerType pureType Same)

getDef :: String -> InputExpr
getDef name =
  Expr.Expression (ExprLens.bodyDefinitionRef # Def name)
  InputPayload
  { _ipVal = ExprLens.pureExpr . ExprLens.bodyDefinitionRef # Def name
  , _ipTyp = void (definitionTypes ! Def name)
  , _ipResumption = Same
  }

hole :: InputExpr
hole = Expr.Expression (ExprLens.bodyHole # ()) (InputPayload pureHole pureHole Same)

getRecursiveDef :: Expr ()
getRecursiveDef =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # recursiveDefI

literalInteger :: Integer -> InputExpr
literalInteger x =
  Expr.Expression (ExprLens.bodyLiteralInteger # x)
  (InputPayload (pureLiteralInteger x) pureIntegerType Same)

-- R represents a cross-section of the whole expression with a new
-- resume level, where the Monoid.Any represents whether any change
-- happened (or everyone's the Same)
newtype R a = R (ZipList (Monoid.Any, a))
  deriving (Functor)
instance Applicative R where
  pure = R . pure . pure
  R f <*> R x = R $ (liftA2 . liftA2) ($) f x

nextResumption :: InputExpr -> (Monoid.Any, InputExpr)
nextResumption expr@(Expr.Expression body (InputPayload _ _ r)) =
  case r of
  Same ->
    (Monoid.Any False, expr)
  ResumeWith newExpr ->
    (Monoid.Any True, newExpr)
  ResumeOnSide _ ipl  ->
    (Monoid.Any True, Expr.Expression body ipl)
  NewInferred ipl ->
    (Monoid.Any True, Expr.Expression body ipl)

resumptions :: InputExpr -> R InputExpr
resumptions expr = R . ZipList $ iterate (nextResumption . snd) (Monoid.Any True, expr)

makeResumption :: [(Monoid.Any, (Expr (), Expr ()))] -> Resumption
makeResumption ((Monoid.Any False, _):_) = Same
makeResumption ((Monoid.Any True, (val, typ)):nexts) = NewInferred InputPayload
  { _ipVal = val
  , _ipTyp = typ
  , _ipResumption = makeResumption nexts
  }
makeResumption [] = error "makeResumption called with finite list"

runR :: R (Expr.Body Def InputExpr, Expr (), Expr ()) -> InputExpr
runR (R (ZipList ~((_, (body, firstVal, firstTyp)):nexts))) =
  Expr.Expression body . InputPayload firstVal firstTyp .
  makeResumption $ nexts <&> Lens._2 %~ valTyp
  where
    valTyp (_body, val, typ) = (val, typ)

-- Sometimes we have an inferred type that comes outside-in but cannot
-- be inferred inside-out:
setInferredType :: InputExpr -> InputExpr -> InputExpr
setInferredType val typ =
  runR $ mk <$> resumptions val <*> resumptions typ
  where
    mk valR typR = (valR ^. Expr.eBody, valR ^. iVal, typR ^. iVal)

recurse :: InputExpr -> InputExpr
recurse typ =
  runR $ mk <$> resumptions typ
  where
    mk typR =
      ( ExprLens.bodyDefinitionRef # recursiveDefI
      , ExprLens.pureExpr . ExprLens.bodyDefinitionRef # recursiveDefI
      , typR ^. iVal
      )

-- Return a hole whose inferred val is the inferred type of the input
inferredTypeAsHole :: InputExpr -> InputExpr
inferredTypeAsHole expr =
  runR $ mk <$> resumptions expr
  where
    mk exprR =
      -- needs me to get InputExprs here! but I don't have them,
      -- because I have the expr, not its type!
      ( ExprLens.bodyHole # ()
      , exprR ^. iType
      , pureType
      )

-- Potential alias for this is holeWithInferredVal:
asHole :: InputExpr -> InputExpr
asHole = Expr.eBody .~ (ExprLens.bodyHole # ())

holeWithInferredType :: InputExpr -> InputExpr
holeWithInferredType typ =
  runR $ mk <$> resumptions typ
  where
    mk typR = (ExprLens.bodyHole # (), pureHole, typR ^. iVal)

mkLam ::
  Kind -> (Guid -> Expr () -> Expr () -> Expr ()) ->
  String -> InputExpr ->
  (InputExpr -> InputExpr) ->
  InputExpr
mkLam k mkPureType name paramType mkResult =
  runR $ mk <$> resumptions paramType <*> resumptions result
  where
    guid = Guid.fromString name
    result = mkResult $ getParam name paramType
    mk paramTypeR resultR =
      ( ExprUtil.makeLam k guid paramTypeR resultR
      , ExprUtil.pureLam k guid (paramTypeR ^. iVal) (resultR ^. iVal)
      , mkPureType guid (paramTypeR ^. iVal) (resultR ^. iType)
      )

piType ::
  String -> InputExpr ->
  (InputExpr -> InputExpr) ->
  InputExpr
piType =
  mkLam KType mkPureType
  where
    mkPureType _name _pureParamType _pureResultType = pureType

infixr 4 ~>
(~>) :: InputExpr -> InputExpr -> InputExpr
(~>) src dest = piType "" src (const dest)

lambda ::
  String -> InputExpr ->
  (InputExpr -> InputExpr) ->
  InputExpr
lambda = mkLam KVal (ExprUtil.pureLam KType)

getField :: InputExpr -> InputExpr -> InputExpr
getField recordVal tagVal =
  runR $ getFieldR <$> resumptions recordVal <*> resumptions tagVal

-- TODO: Support the infer logic of only-one-potentially-matching-field
getFieldR :: InputExpr -> InputExpr -> (Expr.Body Def InputExpr, Expr (), Expr ())
getFieldR recordVal tagVal =
  ( body
  , circumcizedVal
  , fieldType
  )
  where
    body = Expr._BodyGetField # Expr.GetField recordVal tagVal
    circumcizedVal
      | Lens.nullOf ExprLens.exprDefinitionRef recordVal = pureHole
      | otherwise = ExprLens.pureExpr # fmap (^. iVal) body
    fieldType
      | allFieldsMismatch = error "getField has no valid type because all fields mismatch"
      | otherwise = pureFieldType
    mFields = recordVal ^? iType . ExprLens.exprKindedRecordFields KType
    allFieldsMismatch =
      case mFields of
      Nothing -> False
      Just fields -> all (tagMismatch . fst) fields
    tagMismatch fieldTag =
      Lens.nullOf ExprLens.exprHole fieldTag &&
      Lens.nullOf ExprLens.exprHole tagVal &&
      (fieldTag ^?! ExprLens.exprTag /= tagVal ^?! ExprLens.exprTag)
    mPureFieldType tagGuid =
      mFields ^?
      Lens._Just . Lens.traversed .
      Lens.filtered
      (Lens.has (Lens._1 . ExprLens.exprTag . Lens.filtered (tagGuid ==))) .
      Lens._2
    pureFieldType =
      fromMaybe pureHole $
      mPureFieldType =<< tagVal ^? ExprLens.exprTag

lambdaRecord ::
  String -> [(String, InputExpr)] ->
  ([InputExpr] -> InputExpr) -> InputExpr
lambdaRecord paramsName strFields mkResult =
  lambda paramsName (record KType fields) $ \params ->
  mkResult $ map (getField params) fieldTags
  where
    fields = strFields & Lens.traversed . Lens._1 %~ tagStr
    fieldTags = map fst fields

whereItem ::
  String -> InputExpr -> (InputExpr -> InputExpr) -> InputExpr
whereItem name val mkBody =
  lambda name (runR (mkParamType <$> resumptions val)) mkBody $$ val
  where
    mkParamType valR = (bodyHole, valR ^. iType, pureType)

typedWhereItem ::
  String -> InputExpr -> InputExpr -> (InputExpr -> InputExpr) -> InputExpr
typedWhereItem name typ val mkBody =
  lambda name typ mkBody $$ val

getGuidParam :: Guid -> InputExpr -> InputExpr
getGuidParam guid typ =
  runR $ mk <$> resumptions typ
  where
    mk typR =
      ( ExprLens.bodyParameterRef # guid
      , ExprLens.pureExpr . ExprLens.bodyParameterRef # guid
      , typR ^. iVal
      )

getParam :: String -> InputExpr -> InputExpr
getParam = getGuidParam . Guid.fromString

listOf :: InputExpr -> InputExpr
listOf = (getDef "List" $$)

-- Uses inferred holes for cons type
list :: [InputExpr] -> InputExpr
list [] = getDef "[]" $$ hole
list items@(x:_) =
  foldr cons nil items
  where
    typ = inferredTypeAsHole x
    cons h t = getDef ":" $$ typ $$: [h, t]
    nil = getDef "[]" $$ typ

maybeOf :: InputExpr -> InputExpr
maybeOf = (getDef "Maybe" $$)

tagStr :: String -> InputExpr
tagStr = tag . Guid.fromString

infixl 4 $$
infixl 3 $$:

($$:) :: InputExpr -> [InputExpr] -> InputExpr
($$:) f args =
  f $$ record KVal (zip tags args)
  where
    tags = recType ^.. Lens.traversed . Lens._1 . ExprLens.exprTag . Lens.to tag
    recType =
      fromMaybe (error msg) $
      f ^? iType . ExprLens.exprKindedLam KType . Lens._2 . ExprLens.exprKindedRecordFields KType
    msg = "$$: must be applied on a func of record type, not: " ++ show (f ^. iType)

($$) :: InputExpr -> InputExpr -> InputExpr
($$) func arg =
  runR $ mk <$> resumptions func <*> resumptions arg
  where
    mk funcR argR =
      ( ExprUtil.makeApply funcR argR
      , substLam False -- Redex support disabled in new infer:
        KVal (funcR ^. iVal) (argR ^. iVal) pureHole $
        circumcizedApplyVal (funcR ^. iVal) (argR ^. iVal)
      , substLam True KType (funcR ^. iType) (argR ^. iVal) piErr piErr
      )
    piErr = error "Apply of non-Pi type!"
    substLam doSubst k e argVal caseHole caseOther =
      case e ^. Expr.eBody of
      Expr.BodyLam (Expr.Lam k1 paramGuid _ result)
        | k == k1 ->
          if doSubst
          then ExprUtil.substGetPar paramGuid argVal result
          else caseOther
      Expr.BodyLeaf Expr.Hole -> caseHole
      _ -> caseOther
    circumcizedApplyVal funcVal argVal
      | Lens.nullOf ExprLens.exprDefinitionRef funcVal = pureHole
      | otherwise = ExprUtil.pureApply funcVal argVal

record :: Kind -> [(InputExpr, InputExpr)] -> InputExpr
record k fields =
  runR $ mk <$> (fields & Lens.traverse . Lens.both %%~ resumptions)
  where
    mk fieldsR =
      ( ExprLens.bodyKindedRecordFields k # fieldsR
      , ExprLens.pureExpr .
        ExprLens.bodyKindedRecordFields k #
        (fieldsR <&> ((^. iVal) *** (^. iVal)))
      , case k of
        KVal ->
          ExprLens.pureExpr .
          ExprLens.bodyKindedRecordFields KType #
          (fieldsR <&> ((^. iVal) *** (^. iType)))
        KType -> pureType
      )

typeAnnotate :: InputExpr -> InputExpr -> InputExpr
typeAnnotate t v = getDef "id" $$ t $$ v `setInferredType` t
