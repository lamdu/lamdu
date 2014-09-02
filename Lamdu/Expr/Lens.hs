{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Expr.Lens
  -- ValLeaf prisms:
  ( _VGlobal, _VHole, _VRecEmpty
  -- ValBody prisms:
  , _VLeaf
  , _VApp
  -- Leafs
  , valGlobal   , valBodyGlobal
  , valHole     , valBodyHole
  , valRecEmpty , valBodyRecEmpty
  -- Non-leafs
  , valApply
  -- Pure vals:
  , pureValBody
  , pureValApply
  -- Types:
  , _TRecord
  -- Composites:
  , compositeTags
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (Traversal', Prism', prism', Iso', iso)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

-- -- Traversals:
-- exprLam :: Traversal' (Expr def par a) (Lam par (Expr def par a))
-- exprLam = eBody . _VAbs

valApply :: Traversal' (Val a) (V.Apply (Val a))
valApply = V.body . _VApp

pureValBody :: Iso' (Val ()) (V.Body (Val ()))
pureValBody = iso V._valBody (Val ())

pureValApply :: Prism' (Val ()) (V.Apply (Val ()))
pureValApply = pureValBody . _VApp

-- exprRecord :: Traversal' (Expr def par a) (Record (Expr def par a))
-- exprRecord = eBody . _VRec

-- exprGetField :: Traversal' (Expr def par a) (GetField (Expr def par a))
-- exprGetField = eBody . _VGetField

-- exprLeaf :: Traversal' (Expr def par a) (Leaf def par)
-- exprLeaf = eBody . _VLeaf

-- exprKindedRecordFields :: Kind -> Traversal' (Expr def par a) [(Tag, Expr def par a)]
-- exprKindedRecordFields k = eBody . bodyKindedRecordFields k

-- exprKindedLam ::
--   Kind ->
--   Traversal' (Expr def par a)
--   (par, Expr def par a, Expr def par a)
-- exprKindedLam k = eBody . bodyKindedLam k

-- exprParameterRef :: Traversal' (Expr def par a) par
-- exprParameterRef = eBody . bodyParameterRef

-- exprVVar ::
--   Traversal' (Expr def par a) (VariableRef def par)
-- exprVVar = eBody . bodyVVar

-- exprVLiteralInteger :: Traversal' (Expr def par a) Integer
-- exprVLiteralInteger = eBody . bodyVLiteralInteger

valGlobal :: Traversal' (Val a) V.GlobalId
valGlobal = V.body . valBodyGlobal

valHole :: Traversal' (Val a) ()
valHole = V.body . valBodyHole

valRecEmpty :: Traversal' (Val a) ()
valRecEmpty = V.body . valBodyRecEmpty

-- exprType :: Traversal' (Expr def par a) ()
-- exprType = eBody . bodyType

-- exprIntegerType :: Traversal' (Expr def par a) ()
-- exprIntegerType = eBody . bodyIntegerType

-- exprLeaves ::
--   Traversal (Expr defa par a) (Expr defb par a) (Leaf defa par) (Leaf defb par)
-- exprLeaves = eBody . bodyLeaves exprLeaves

-- variableRefNTraverse ::
--   Applicative f =>
--   (defa -> f defb) -> (para -> f parb) ->
--   VariableRef defa para -> f (VariableRef defb parb)
-- variableRefNTraverse onDef onPar varRef =
--   case varRef of
--   ParameterRef par -> ParameterRef <$> onPar par
--   DefinitionRef def -> DefinitionRef <$> onDef def

-- leafNTraverse ::
--   Applicative f =>
--   (defa -> f defb) -> (para -> f parb) ->
--   Leaf defa para -> f (Leaf defb parb)
-- leafNTraverse onDef onPar =
--   _VVar (variableRefNTraverse onDef onPar)

-- lamNTraverse ::
--   Applicative f =>
--   (para -> f parb) -> (expra -> f exprb) ->
--   Lam para expra -> f (Lam parb exprb)
-- lamNTraverse onPar onExpr (Lam k par paramType result) =
--   Lam k <$> onPar par <*> onExpr paramType <*> onExpr result

-- bodyNTraverse ::
--   Applicative f =>
--   (defa -> f defb) -> (para -> f parb) -> (expra -> f exprb) ->
--   Body defa para expra -> f (Body defb parb exprb)
-- bodyNTraverse onDef onPar onExpr body =
--   case body of
--   VAbs x -> VAbs <$> lamNTraverse onPar onExpr x
--   VApp x -> VApp <$> traverse onExpr x
--   VRec x -> VRec <$> traverse onExpr x
--   VGetField x -> VGetField <$> traverse onExpr x
--   VLeaf leaf -> VLeaf <$> leafNTraverse onDef onPar leaf

-- bodyDef :: Traversal (Body defa par a) (Body defb par a) defa defb
-- bodyDef f = bodyNTraverse f pure pure

-- bodyPar :: Traversal (Body def para a) (Body def parb a) para parb
-- bodyPar f = bodyNTraverse pure f pure

-- exprNTraverse ::
--   Applicative f =>
--   (defa -> f defb) -> (para -> f parb) -> (pla -> f plb) ->
--   Expr defa para pla -> f (Expr defb parb plb)
-- exprNTraverse onDef onPar onPl = f
--   where
--     f (Expr body payload) =
--       Expr <$> bodyNTraverse onDef onPar f body <*> onPl payload

-- exprDef :: Traversal (Expr defa par pl) (Expr defb par pl) defa defb
-- exprDef onDef = exprNTraverse onDef pure pure

-- exprPar :: Traversal (Expr def para pl) (Expr def parb pl) para parb
-- exprPar onPar = exprNTraverse pure onPar pure

-- -- TODO: Does this function make sense? It has no way of correcting the par's in the Lams
-- bodyLeaves ::
--   Applicative f =>
--   Lens.LensLike f expra exprb (Leaf defa par) (Leaf defb par) ->
--   Lens.LensLike f (Body defa par expra) (Body defb par exprb) (Leaf defa par) (Leaf defb par)
-- bodyLeaves leaves onLeaves body =
--   case body of
--   VAbs x      -> VAbs      <$> onExprs x
--   VApp x      -> VApp      <$> onExprs x
--   VRec x      -> VRec      <$> onExprs x
--   VGetField x -> VGetField <$> onExprs x
--   VLeaf l     -> VLeaf     <$> onLeaves l
--   where
--     onExprs = traverse (leaves onLeaves)

-- -- Prisms:
-- parameterRef :: Lens.Prism (Leaf def p) (Leaf def q) p q
-- parameterRef = _VVar . _ParameterRef

-- definitionRef :: Lens.Prism (Leaf defa par) (Leaf defb par) defa defb
-- definitionRef = _VVar . _DefinitionRef

-- bodyParameterRef :: Lens.Prism' (Body def par expr) par
-- bodyParameterRef = _VLeaf . parameterRef

_VGlobal :: Prism' V.Leaf V.GlobalId
_VGlobal = prism' V.LGlobal getGlobal
  where
    getGlobal (V.LGlobal gid) = Just gid
    getGlobal _ = Nothing

_VHole :: Prism' V.Leaf ()
_VHole = prism' (\() -> V.LHole) getHole
  where
    getHole V.LHole = Just ()
    getHole _ = Nothing

_VRecEmpty :: Prism' V.Leaf ()
_VRecEmpty = prism' (\() -> V.LRecEmpty) getHole
  where
    getHole V.LRecEmpty = Just ()
    getHole _ = Nothing

-- TODO: _V* -> _B*
_VLeaf :: Prism' (V.Body a) V.Leaf
_VLeaf = prism' V.BLeaf getLeaf
  where
    getLeaf (V.BLeaf x) = Just x
    getLeaf _ = Nothing

_VApp :: Prism' (V.Body a) (V.Apply a)
_VApp = prism' V.BApp getVApp
  where
    getVApp (V.BApp x) = Just x
    getVApp _ = Nothing

-- _VAbs          (Lam exp)
-- -VGetField     (GetField exp)
-- _VRecExtend    (RecExtend exp)

valBodyGlobal :: Prism' (V.Body e) V.GlobalId
valBodyGlobal = _VLeaf . _VGlobal

-- bodyVLiteralInteger :: Lens.Prism' (Body def par expr) Integer
-- bodyVLiteralInteger = _VLeaf . _VLiteralInteger

-- bodyVVar ::
--   Lens.Prism (Body defa par expr) (Body defb par expr)
--   (VariableRef defa par) (VariableRef defb par)
-- bodyVVar = _VLeaf . _VVar

valBodyHole :: Prism' (V.Body expr) ()
valBodyHole = _VLeaf . _VHole

valBodyRecEmpty :: Prism' (V.Body expr) ()
valBodyRecEmpty = _VLeaf . _VRecEmpty

-- bodyType :: Lens.Prism' (Body def par expr) ()
-- bodyType = _VLeaf . _Type

-- bodyIntegerType :: Lens.Prism' (Body def par expr) ()
-- bodyIntegerType = _VLeaf . _IntegerType

-- kindedRecordFields ::
--   Kind -> Lens.Prism' (Record a) [(Tag, a)]
-- kindedRecordFields k0 = Lens.prism' to from
--   where
--     to = Record k0
--     from (Record k1 fields)
--       | k0 == k1 = Just fields
--       | otherwise = Nothing

-- kindedLam :: Kind -> Lens.Prism' (Lam par expr) (par, expr, expr)
-- kindedLam k = Lens.prism' toLam fromLam
--   where
--     toLam (param, paramType, result) =
--       Lam k param paramType result
--     fromLam (Lam k0 param paramType result)
--       | k == k0 = Just (param, paramType, result)
--       | otherwise = Nothing

-- bodyKindedLam :: Kind -> Lens.Prism' (Body def par expr) (par, expr, expr)
-- bodyKindedLam k = _VAbs . kindedLam k

-- bodyKindedRecordFields :: Kind -> Lens.Prism' (Body def par expr) [(Tag, expr)]
-- bodyKindedRecordFields k = _VRec . kindedRecordFields k

-- -- Pure expressions:
-- pureExpr :: Lens.Iso' (Expr def par ()) (Body def par (Expr def par ()))
-- pureExpr = Lens.iso (^. eBody) (`Expr` ())

-- subTreesThat :: (Expr def par a -> Bool) -> Traversal' (Expr def par a) (Expr def par a)
-- subTreesThat cond f expr
--   | cond expr = f expr
--   | otherwise = expr & eBody . Lens.traversed %%~ subTreesThat cond f

-- -- Exprs in tag positions of Record and GetField.
-- -- Not recursive (no tags inside tags), a valid traversal.
-- tagPositions :: Traversal' (Expr def par a) (Expr def par a)
-- tagPositions f =
--   traverse go
--   & Lens.outside _VGetField .~ fmap VGetField . getFieldRecord go
--   & Lens.outside _VRec .~ fmap VRec . (recordFields . traverse . Lens._2) go
--   & eBody
--   where
--     go = tagPositions f

-- -- Lambda param types not including param types inside param types (a valid traversal)
-- lambdaParamTypes :: Traversal' (Expr def par a) (Expr def par a)
-- lambdaParamTypes f =
--   traverse go
--   & Lens.outside (bodyKindedLam KVal) .~ fmap (bodyKindedLam KVal # ) . onLambda
--   & eBody
--   where
--     go = lambdaParamTypes f
--     onLambda (paramId, paramType, body) =
--       (,,) paramId <$> f paramType <*> go body

-- holePayloads :: Traversal' (Expr def par a) a
-- holePayloads f (Expr (VLeaf VHole) pl) =
--   Expr (VLeaf VHole) <$> f pl
-- holePayloads f (Expr body pl) =
--   (`Expr` pl) <$> traverse (holePayloads f) body

_TRecord :: Prism' Type (T.Composite T.Product)
_TRecord = prism' T.TRecord get
  where
    get (T.TRecord x) = Just x
    get _ = Nothing

compositeTags :: Traversal' (T.Composite p) T.Tag
compositeTags f (T.CExtend tag typ rest) =
  mkCExtend <$> f tag <*> compositeTags f rest
  where
    mkCExtend tag' rest' = T.CExtend tag' typ rest'
compositeTags _ r = pure r
