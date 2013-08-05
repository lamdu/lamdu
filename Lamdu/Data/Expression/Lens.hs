{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expression.Lens
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
  , bodyBitraverse, exprBitraverse
  , bodyDef, exprDef
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
import Lamdu.Data.Expression

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import qualified Control.Lens as Lens

-- Traversals:
exprLam :: Lens.Traversal' (Expression def a) (Lam (Expression def a))
exprLam = eBody . _BodyLam

exprApply :: Lens.Traversal' (Expression def a) (Apply (Expression def a))
exprApply = eBody . _BodyApply

exprRecord :: Lens.Traversal' (Expression def a) (Record (Expression def a))
exprRecord = eBody . _BodyRecord

exprGetField :: Lens.Traversal' (Expression def a) (GetField (Expression def a))
exprGetField = eBody . _BodyGetField

exprLeaf :: Lens.Traversal' (Expression def a) (Leaf def)
exprLeaf = eBody . _BodyLeaf

exprKindedRecordFields :: Kind -> Lens.Traversal' (Expression def a) [(Expression def a, Expression def a)]
exprKindedRecordFields k = eBody . bodyKindedRecordFields k

exprKindedLam ::
  Kind ->
  Lens.Traversal' (Expression def a)
  (Guid, Expression def a, Expression def a)
exprKindedLam k = eBody . bodyKindedLam k

exprTag :: Lens.Traversal' (Expression def a) Guid
exprTag = eBody . bodyTag

exprParameterRef :: Lens.Traversal' (Expression def a) Guid
exprParameterRef = eBody . bodyParameterRef

exprGetVariable ::
  Lens.Traversal' (Expression def a) (VariableRef def)
exprGetVariable = eBody . bodyGetVariable

exprLiteralInteger :: Lens.Traversal' (Expression def a) Integer
exprLiteralInteger = eBody . bodyLiteralInteger

exprDefinitionRef :: Lens.Traversal' (Expression def a) def
exprDefinitionRef = eBody . bodyDefinitionRef

exprHole :: Lens.Traversal' (Expression def a) ()
exprHole = eBody . bodyHole

exprType :: Lens.Traversal' (Expression def a) ()
exprType = eBody . bodyType

exprIntegerType :: Lens.Traversal' (Expression def a) ()
exprIntegerType = eBody . bodyIntegerType

exprTagType :: Lens.Traversal' (Expression def a) ()
exprTagType = eBody . bodyTagType

exprLeaves ::
  Lens.Traversal (Expression defa a) (Expression defb a) (Leaf defa) (Leaf defb)
exprLeaves = eBody . bodyLeaves exprLeaves

bodyBitraverse ::
  Applicative f => (defa -> f defb) -> (expra -> f exprb) ->
  Body defa expra ->
  f (Body defb exprb)
bodyBitraverse onDef onExpr body =
  case body of
  BodyLam x -> BodyLam <$> traverse onExpr x
  BodyApply x -> BodyApply <$> traverse onExpr x
  BodyRecord x -> BodyRecord <$> traverse onExpr x
  BodyGetField x -> BodyGetField <$> traverse onExpr x
  BodyLeaf leaf -> BodyLeaf <$> traverse onDef leaf

bodyDef :: Lens.Traversal (Body a expr) (Body b expr) a b
bodyDef = (`bodyBitraverse` pure)

exprBitraverse ::
  Applicative f =>
  (defa -> f defb) -> (pla -> f plb) ->
  Expression defa pla -> f (Expression defb plb)
exprBitraverse onDef onPl = f
  where
    f (Expression body payload) =
      Expression <$> bodyBitraverse onDef f body <*> onPl payload

exprDef :: Lens.Traversal (Expression a pl) (Expression b pl) a b
exprDef = (`exprBitraverse` pure)

bodyLeaves ::
  Applicative f =>
  Lens.LensLike f expra exprb (Leaf defa) (Leaf defb) ->
  Lens.LensLike f (Body defa expra) (Body defb exprb) (Leaf defa) (Leaf defb)
bodyLeaves leaves onLeaves body =
  case body of
  BodyLam x      -> BodyLam      <$> onExprs x
  BodyApply x    -> BodyApply    <$> onExprs x
  BodyRecord x   -> BodyRecord   <$> onExprs x
  BodyGetField x -> BodyGetField <$> onExprs x
  BodyLeaf l -> BodyLeaf <$> onLeaves l
  where
    onExprs = traverse (leaves onLeaves)

-- Prisms:
parameterRef :: Lens.Prism' (Leaf def) Guid
parameterRef = _GetVariable . _ParameterRef

definitionRef :: Lens.Prism (Leaf defa) (Leaf defb) defa defb
definitionRef = _GetVariable . _DefinitionRef

bodyParameterRef :: Lens.Prism' (Body def expr) Guid
bodyParameterRef = _BodyLeaf . parameterRef

bodyDefinitionRef :: Lens.Prism (Body defa expr) (Body defb expr) defa defb
bodyDefinitionRef = _BodyLeaf . definitionRef

bodyLiteralInteger :: Lens.Prism' (Body def expr) Integer
bodyLiteralInteger = _BodyLeaf . _LiteralInteger

bodyGetVariable ::
  Lens.Prism (Body defa expr) (Body defb expr)
  (VariableRef defa) (VariableRef defb)
bodyGetVariable = _BodyLeaf . _GetVariable

bodyHole :: Lens.Prism' (Body def expr) ()
bodyHole = _BodyLeaf . _Hole

bodyTag :: Lens.Prism' (Body def expr) Guid
bodyTag = _BodyLeaf . _Tag

bodyType :: Lens.Prism' (Body def expr) ()
bodyType = _BodyLeaf . _Type

bodyIntegerType :: Lens.Prism' (Body def expr) ()
bodyIntegerType = _BodyLeaf . _IntegerType

bodyTagType :: Lens.Prism' (Body def expr) ()
bodyTagType = _BodyLeaf . _TagType

kindedRecordFields ::
  Kind -> Lens.Prism' (Record a) [(a, a)]
kindedRecordFields k0 = Lens.prism' to from
  where
    to = Record k0
    from (Record k1 fields)
      | k0 == k1 = Just fields
      | otherwise = Nothing

kindedLam :: Kind -> Lens.Prism' (Lam expr) (Guid, expr, expr)
kindedLam k = Lens.prism' toLam fromLam
  where
    toLam (paramGuid, paramType, result) =
      Lam k paramGuid paramType result
    fromLam (Lam k0 paramGuid paramType result)
      | k == k0 = Just (paramGuid, paramType, result)
      | otherwise = Nothing

bodyKindedLam :: Kind -> Lens.Prism' (Body def expr) (Guid, expr, expr)
bodyKindedLam k = _BodyLam . kindedLam k

bodyKindedRecordFields :: Kind -> Lens.Prism' (Body def expr) [(expr, expr)]
bodyKindedRecordFields k = _BodyRecord . kindedRecordFields k

-- Pure expressions:
pureExpr :: Lens.Iso' (Expression def ()) (Body def (Expression def ()))
pureExpr = Lens.iso (^. eBody) (`Expression` ())

subTreesThat :: (Expression def a -> Bool) -> Lens.Traversal' (Expression def a) (Expression def a)
subTreesThat cond f expr
  | cond expr = f expr
  | otherwise = expr & eBody . Lens.traversed %%~ subTreesThat cond f

-- Expressions in tag positions of Record and GetField.
-- Not recursive (no tags inside tags), a valid traversal.
tagPositions :: Lens.Traversal' (Expression def a) (Expression def a)
tagPositions f =
  traverse go
  & Lens.outside _BodyGetField .~ fmap BodyGetField . getFieldRecord go
  & Lens.outside _BodyRecord .~ fmap BodyRecord . (recordFields . traverse . Lens._2) go
  & eBody
  where
    go = tagPositions f

-- Lambda param types not including param types inside param types (a valid traversal)
lambdaParamTypes :: Lens.Traversal' (Expression def a) (Expression def a)
lambdaParamTypes f =
  traverse go
  & Lens.outside (bodyKindedLam KVal) .~ fmap (bodyKindedLam KVal # ) . onLambda
  & eBody
  where
    go = lambdaParamTypes f
    onLambda (paramId, paramType, body) =
      (,,) paramId <$> f paramType <*> go body

holePayloads :: Lens.Traversal' (Expression def a) a
holePayloads f (Expression (BodyLeaf Hole) pl) =
  Expression (BodyLeaf Hole) <$> f pl
holePayloads f (Expression body pl) =
  (`Expression` pl) <$> traverse (holePayloads f) body

-- Everywhere, including lams:
bodyParamIds :: Lens.Traversal' (Body def a) Guid
bodyParamIds f body =
  case body of
  BodyLeaf (GetVariable (ParameterRef guid)) ->
    (bodyParameterRef # ) <$> f guid
  BodyLam lam ->
    BodyLam <$> (lam & lamParamId %%~ f)
  _ -> pure body
