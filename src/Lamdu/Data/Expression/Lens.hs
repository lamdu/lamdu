{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expression.Lens
  ( pureExpr
  , exprLam
  , exprApply
  , exprRecord
  , exprGetField
  , exprLeaf
  , exprRecordKinded
  , exprTag
  , exprParameterRef
  , bitraverseBody, bitraverseExpression
  , expressionBodyDef, expressionDef
  , expressionLeaves, bodyLeaves
  , parameterRef, definitionRef
  , bodyParameterRef, bodyDefinitionRef
  , bodyLiteralInteger
  , bodyHole
  , bodyTag
  , bodySet
  , bodyIntegerType
  , kindedRecordFields
  , kindedLam
  , lambda, pi
  ) where

import Prelude hiding (pi)
import Lamdu.Data.Expression

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import qualified Control.Lens as Lens

-- Traversals:
exprLam :: Lens.Traversal' (Expression def a) (Lambda (Expression def a))
exprLam = eBody . _BodyLam

exprApply :: Lens.Traversal' (Expression def a) (Apply (Expression def a))
exprApply = eBody . _BodyApply

exprRecord :: Lens.Traversal' (Expression def a) (Record (Expression def a))
exprRecord = eBody . _BodyRecord

exprGetField :: Lens.Traversal' (Expression def a) (GetField (Expression def a))
exprGetField = eBody . _BodyGetField

exprLeaf :: Lens.Traversal' (Expression def a) (Leaf def)
exprLeaf = eBody . _BodyLeaf

exprRecordKinded :: Kind -> Lens.Traversal' (Expression def a) [(Expression def a, Expression def a)]
exprRecordKinded k = exprRecord . kindedRecordFields k

exprTag :: Lens.Traversal' (Expression def a) Guid
exprTag = eBody . bodyTag

exprParameterRef :: Lens.Traversal' (Expression def a) Guid
exprParameterRef = eBody . bodyParameterRef

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

bodyHole :: Lens.Prism' (Body def expr) ()
bodyHole = _BodyLeaf . _Hole

bodyTag :: Lens.Prism' (Body def expr) Guid
bodyTag = _BodyLeaf . _Tag

bodySet :: Lens.Prism' (Body def expr) ()
bodySet = _BodyLeaf . _Set

bodyIntegerType :: Lens.Prism' (Body def expr) ()
bodyIntegerType = _BodyLeaf . _IntegerType

kindedRecordFields ::
  Kind -> Lens.Prism' (Record a) [(a, a)]
kindedRecordFields k0 = Lens.prism' to from
  where
    to fields = Record k0 fields
    from (Record k1 fields)
      | k0 == k1 = Just fields
      | otherwise = Nothing

kindedLam :: Kind -> Lens.Prism' (Lambda expr) (Guid, expr, expr)
kindedLam k = Lens.prism' toLam fromLam
  where
    toLam (paramGuid, paramType, result) =
      Lambda k paramGuid paramType result
    fromLam (Lambda k0 paramGuid paramType result)
      | k == k0 = Just (paramGuid, paramType, result)
      | otherwise = Nothing

-- Pure expressions:
pureExpr :: Lens.Iso' (Expression def ()) (Body def (Expression def ()))
pureExpr = Lens.iso (^. eBody) (`Expression` ())

-- TODO: Remove the kind-passing wrappers
lambda :: Lens.Prism' (Lambda expr) (Guid, expr, expr)
lambda = kindedLam Val

pi :: Lens.Prism' (Lambda expr) (Guid, expr, expr)
pi = kindedLam Type
