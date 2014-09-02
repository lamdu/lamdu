{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, FlexibleContexts #-}
module Lamdu.Expr.Lens
  -- ValLeaf prisms:
  ( _VGlobal, _VHole, _VRecEmpty, _VVar
  -- ValBody prisms:
  , _VLeaf
  , _VApp
  , _VGetField
  -- Leafs
  , valGlobal   , valBodyGlobal
  , valHole     , valBodyHole
  , valVar      , valBodyVar
  , valRecEmpty , valBodyRecEmpty
  -- Non-leafs
  , valGetField
  , valApply
  -- Pure vals:
  , pureValBody
  , pureValApply
  -- Types:
  , _TRecord
  , _TFun
  -- Composites:
  , compositeTags
  -- Subexpressions:
  , subExprPayloads
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (Traversal', Prism', prism', Iso', iso)
import Control.Lens.Operators
import Control.Monad (void)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import qualified Control.Lens as Lens
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

valApply :: Traversal' (Val a) (V.Apply (Val a))
valApply = V.body . _VApp

pureValBody :: Iso' (Val ()) (V.Body (Val ()))
pureValBody = iso V._valBody (Val ())

pureValApply :: Prism' (Val ()) (V.Apply (Val ()))
pureValApply = pureValBody . _VApp

valGlobal :: Traversal' (Val a) V.GlobalId
valGlobal = V.body . valBodyGlobal

valHole :: Traversal' (Val a) ()
valHole = V.body . valBodyHole

valVar :: Traversal' (Val a) V.Var
valVar = V.body . valBodyVar

valRecEmpty :: Traversal' (Val a) ()
valRecEmpty = V.body . valBodyRecEmpty

valGetField  :: Traversal' (Val a) (V.GetField (Val a))
valGetField = V.body . _VGetField

_VGlobal :: Prism' V.Leaf V.GlobalId
_VGlobal = prism' V.LGlobal get
  where
    get (V.LGlobal gid) = Just gid
    get _ = Nothing

_VHole :: Prism' V.Leaf ()
_VHole = prism' (\() -> V.LHole) get
  where
    get V.LHole = Just ()
    get _ = Nothing

_VRecEmpty :: Prism' V.Leaf ()
_VRecEmpty = prism' (\() -> V.LRecEmpty) get
  where
    get V.LRecEmpty = Just ()
    get _ = Nothing

_VVar :: Prism' V.Leaf V.Var
_VVar = prism' V.LVar get
  where
    get (V.LVar gid) = Just gid
    get _ = Nothing

-- TODO: _V* -> _B*
_VLeaf :: Prism' (V.Body a) V.Leaf
_VLeaf = prism' V.BLeaf get
  where
    get (V.BLeaf x) = Just x
    get _ = Nothing

_VApp :: Prism' (V.Body a) (V.Apply a)
_VApp = prism' V.BApp get
  where
    get (V.BApp x) = Just x
    get _ = Nothing

_VGetField :: Prism' (V.Body a) (V.GetField a)
_VGetField = prism' V.BGetField get
  where
    get (V.BGetField x) = Just x
    get _ = Nothing

valBodyGlobal :: Prism' (V.Body e) V.GlobalId
valBodyGlobal = _VLeaf . _VGlobal

valBodyHole :: Prism' (V.Body expr) ()
valBodyHole = _VLeaf . _VHole

valBodyVar :: Prism' (V.Body expr) V.Var
valBodyVar = _VLeaf . _VVar

valBodyRecEmpty :: Prism' (V.Body expr) ()
valBodyRecEmpty = _VLeaf . _VRecEmpty

_TRecord :: Prism' Type (T.Composite T.Product)
_TRecord = prism' T.TRecord get
  where
    get (T.TRecord x) = Just x
    get _ = Nothing

_TFun :: Prism' Type (Type, Type)
_TFun = prism' (uncurry T.TFun) get
  where
    get (T.TFun arg res) = Just (arg, res)
    get _ = Nothing

compositeTags :: Traversal' (T.Composite p) T.Tag
compositeTags f (T.CExtend tag typ rest) =
  mkCExtend <$> f tag <*> compositeTags f rest
  where
    mkCExtend tag' = T.CExtend tag' typ
compositeTags _ r = pure r

subExprPayloads :: Lens.IndexedTraversal (Val ()) (Val a) (Val b) a b
subExprPayloads f val@(Val pl body) =
  Val
  <$> Lens.indexed f (void val) pl
  <*> (body & Lens.traversed .> subExprPayloads %%~ f)
