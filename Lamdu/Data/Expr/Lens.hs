{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expr.Lens
  ( pureExpr
  , parameterRef, definitionRef
  , kindedLam, kindedRecordFields
  , exprLam
  , exprApply
  , exprRecord
  , exprGetField
  , exprLeaf
  , bodyKindedRecordFields, exprKindedRecordFields
  , bodyKindedLam, exprKindedLam
  , bodyNTraverse, exprNTraverse
  , bodyDef, exprDef
  , bodyPar, exprPar
  , bodyLeaves, exprLeaves
  , bodyParameterRef, exprParameterRef
  , bodyDefinitionRef, exprDefinitionRef
  , bodyLiteralInteger, exprLiteralInteger
  , bodyHole, exprHole
  , bodyTag, exprTag
  , bodyType, exprType
  , bodyIntegerType, exprIntegerType
  , bodyTagType, exprTagType
  , bodyGetVariable, exprGetVariable
  , subTreesThat
  , tagPositions
  , lambdaParamTypes
  , holePayloads
  , bodyParamIds
  ) where

import Prelude hiding (pi)
import Lamdu.Data.Expr

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import qualified Control.Lens as Lens

-- Traversals:
exprLam :: Lens.Traversal' (Expr def par a) (Lam par (Expr def par a))
exprLam = eBody . _BodyLam

exprApply :: Lens.Traversal' (Expr def par a) (Apply (Expr def par a))
exprApply = eBody . _BodyApply

exprRecord :: Lens.Traversal' (Expr def par a) (Record (Expr def par a))
exprRecord = eBody . _BodyRecord

exprGetField :: Lens.Traversal' (Expr def par a) (GetField (Expr def par a))
exprGetField = eBody . _BodyGetField

exprLeaf :: Lens.Traversal' (Expr def par a) (Leaf def par)
exprLeaf = eBody . _BodyLeaf

exprKindedRecordFields :: Kind -> Lens.Traversal' (Expr def par a) [(Expr def par a, Expr def par a)]
exprKindedRecordFields k = eBody . bodyKindedRecordFields k

exprKindedLam ::
  Kind ->
  Lens.Traversal' (Expr def par a)
  (par, Expr def par a, Expr def par a)
exprKindedLam k = eBody . bodyKindedLam k

exprTag :: Lens.Traversal' (Expr def par a) Guid
exprTag = eBody . bodyTag

exprParameterRef :: Lens.Traversal' (Expr def par a) par
exprParameterRef = eBody . bodyParameterRef

exprGetVariable ::
  Lens.Traversal' (Expr def par a) (VariableRef def par)
exprGetVariable = eBody . bodyGetVariable

exprLiteralInteger :: Lens.Traversal' (Expr def par a) Integer
exprLiteralInteger = eBody . bodyLiteralInteger

exprDefinitionRef :: Lens.Traversal' (Expr def par a) def
exprDefinitionRef = eBody . bodyDefinitionRef

exprHole :: Lens.Traversal' (Expr def par a) ()
exprHole = eBody . bodyHole

exprType :: Lens.Traversal' (Expr def par a) ()
exprType = eBody . bodyType

exprIntegerType :: Lens.Traversal' (Expr def par a) ()
exprIntegerType = eBody . bodyIntegerType

exprTagType :: Lens.Traversal' (Expr def par a) ()
exprTagType = eBody . bodyTagType

exprLeaves ::
  Lens.Traversal (Expr defa par a) (Expr defb par a) (Leaf defa par) (Leaf defb par)
exprLeaves = eBody . bodyLeaves exprLeaves

variableRefNTraverse ::
  Applicative f =>
  (defa -> f defb) -> (para -> f parb) ->
  VariableRef defa para -> f (VariableRef defb parb)
variableRefNTraverse onDef onPar varRef =
  case varRef of
  ParameterRef par -> ParameterRef <$> onPar par
  DefinitionRef def -> DefinitionRef <$> onDef def

leafNTraverse ::
  Applicative f =>
  (defa -> f defb) -> (para -> f parb) ->
  Leaf defa para -> f (Leaf defb parb)
leafNTraverse onDef onPar =
  _GetVariable (variableRefNTraverse onDef onPar)

lamNTraverse ::
  Applicative f =>
  (para -> f parb) -> (expra -> f exprb) ->
  Lam para expra -> f (Lam parb exprb)
lamNTraverse onPar onExpr (Lam k par paramType result) =
  Lam k <$> onPar par <*> onExpr paramType <*> onExpr result

bodyNTraverse ::
  Applicative f =>
  (defa -> f defb) -> (para -> f parb) -> (expra -> f exprb) ->
  Body defa para expra -> f (Body defb parb exprb)
bodyNTraverse onDef onPar onExpr body =
  case body of
  BodyLam x -> BodyLam <$> lamNTraverse onPar onExpr x
  BodyApply x -> BodyApply <$> traverse onExpr x
  BodyRecord x -> BodyRecord <$> traverse onExpr x
  BodyGetField x -> BodyGetField <$> traverse onExpr x
  BodyLeaf leaf -> BodyLeaf <$> leafNTraverse onDef onPar leaf

bodyDef :: Lens.Traversal (Body defa par expr) (Body defb par expr) defa defb
bodyDef f = bodyNTraverse f pure pure

bodyPar :: Lens.Traversal (Body def para expr) (Body def parb expr) para parb
bodyPar f = bodyNTraverse pure f pure

exprNTraverse ::
  Applicative f =>
  (defa -> f defb) -> (para -> f parb) -> (pla -> f plb) ->
  Expr defa para pla -> f (Expr defb parb plb)
exprNTraverse onDef onPar onPl = f
  where
    f (Expr body payload) =
      Expr <$> bodyNTraverse onDef onPar f body <*> onPl payload

exprDef :: Lens.Traversal (Expr defa par pl) (Expr defb par pl) defa defb
exprDef onDef = exprNTraverse onDef pure pure

exprPar :: Lens.Traversal (Expr def para pl) (Expr def parb pl) para parb
exprPar onPar = exprNTraverse pure onPar pure

-- TODO: Does this function make sense? It has no way of correcting the par's in the Lams
bodyLeaves ::
  Applicative f =>
  Lens.LensLike f expra exprb (Leaf defa par) (Leaf defb par) ->
  Lens.LensLike f (Body defa par expra) (Body defb par exprb) (Leaf defa par) (Leaf defb par)
bodyLeaves leaves onLeaves body =
  case body of
  BodyLam x      -> BodyLam      <$> onExprs x
  BodyApply x    -> BodyApply    <$> onExprs x
  BodyRecord x   -> BodyRecord   <$> onExprs x
  BodyGetField x -> BodyGetField <$> onExprs x
  BodyLeaf l     -> BodyLeaf     <$> onLeaves l
  where
    onExprs = traverse (leaves onLeaves)

-- Prisms:
parameterRef :: Lens.Prism' (Leaf def par) par
parameterRef = _GetVariable . _ParameterRef

definitionRef :: Lens.Prism (Leaf defa par) (Leaf defb par) defa defb
definitionRef = _GetVariable . _DefinitionRef

bodyParameterRef :: Lens.Prism' (Body def par expr) par
bodyParameterRef = _BodyLeaf . parameterRef

bodyDefinitionRef :: Lens.Prism (Body defa par expr) (Body defb par expr) defa defb
bodyDefinitionRef = _BodyLeaf . definitionRef

bodyLiteralInteger :: Lens.Prism' (Body def par expr) Integer
bodyLiteralInteger = _BodyLeaf . _LiteralInteger

bodyGetVariable ::
  Lens.Prism (Body defa par expr) (Body defb par expr)
  (VariableRef defa par) (VariableRef defb par)
bodyGetVariable = _BodyLeaf . _GetVariable

bodyHole :: Lens.Prism' (Body def par expr) ()
bodyHole = _BodyLeaf . _Hole

bodyTag :: Lens.Prism' (Body def par expr) Guid
bodyTag = _BodyLeaf . _Tag

bodyType :: Lens.Prism' (Body def par expr) ()
bodyType = _BodyLeaf . _Type

bodyIntegerType :: Lens.Prism' (Body def par expr) ()
bodyIntegerType = _BodyLeaf . _IntegerType

bodyTagType :: Lens.Prism' (Body def par expr) ()
bodyTagType = _BodyLeaf . _TagType

kindedRecordFields ::
  Kind -> Lens.Prism' (Record a) [(a, a)]
kindedRecordFields k0 = Lens.prism' to from
  where
    to = Record k0
    from (Record k1 fields)
      | k0 == k1 = Just fields
      | otherwise = Nothing

kindedLam :: Kind -> Lens.Prism' (Lam par expr) (par, expr, expr)
kindedLam k = Lens.prism' toLam fromLam
  where
    toLam (param, paramType, result) =
      Lam k param paramType result
    fromLam (Lam k0 param paramType result)
      | k == k0 = Just (param, paramType, result)
      | otherwise = Nothing

bodyKindedLam :: Kind -> Lens.Prism' (Body def par expr) (par, expr, expr)
bodyKindedLam k = _BodyLam . kindedLam k

bodyKindedRecordFields :: Kind -> Lens.Prism' (Body def par expr) [(expr, expr)]
bodyKindedRecordFields k = _BodyRecord . kindedRecordFields k

-- Pure expressions:
pureExpr :: Lens.Iso' (Expr def par ()) (Body def par (Expr def par ()))
pureExpr = Lens.iso (^. eBody) (`Expr` ())

subTreesThat :: (Expr def par a -> Bool) -> Lens.Traversal' (Expr def par a) (Expr def par a)
subTreesThat cond f expr
  | cond expr = f expr
  | otherwise = expr & eBody . Lens.traversed %%~ subTreesThat cond f

-- Exprs in tag positions of Record and GetField.
-- Not recursive (no tags inside tags), a valid traversal.
tagPositions :: Lens.Traversal' (Expr def par a) (Expr def par a)
tagPositions f =
  traverse go
  & Lens.outside _BodyGetField .~ fmap BodyGetField . getFieldRecord go
  & Lens.outside _BodyRecord .~ fmap BodyRecord . (recordFields . traverse . Lens._2) go
  & eBody
  where
    go = tagPositions f

-- Lambda param types not including param types inside param types (a valid traversal)
lambdaParamTypes :: Lens.Traversal' (Expr def par a) (Expr def par a)
lambdaParamTypes f =
  traverse go
  & Lens.outside (bodyKindedLam KVal) .~ fmap (bodyKindedLam KVal # ) . onLambda
  & eBody
  where
    go = lambdaParamTypes f
    onLambda (paramId, paramType, body) =
      (,,) paramId <$> f paramType <*> go body

holePayloads :: Lens.Traversal' (Expr def par a) a
holePayloads f (Expr (BodyLeaf Hole) pl) =
  Expr (BodyLeaf Hole) <$> f pl
holePayloads f (Expr body pl) =
  (`Expr` pl) <$> traverse (holePayloads f) body

-- Everywhere, including lams:
bodyParamIds :: Lens.Traversal' (Body def par a) par
bodyParamIds f body =
  case body of
  BodyLeaf (GetVariable (ParameterRef par)) ->
    (bodyParameterRef # ) <$> f par
  BodyLam lam ->
    BodyLam <$> (lam & lamParamId %%~ f)
  _ -> pure body
