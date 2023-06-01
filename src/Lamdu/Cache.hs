-- | Functions memoized with Data.Cache.Fenced

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Lamdu.Cache
    ( Functions(..)
    , infer
    , make, FencedCache.Cache, FencedCache.fence
    ) where

import           Control.Monad.RWS (RWST(..))
import           Data.Cache.Fenced (Decl, function)
import qualified Data.Cache.Fenced as FencedCache
import           Hyper
import           Hyper.Class.ZipMatch
import           Hyper.Infer (InferResult)
import           Hyper.Recurse
import           Hyper.Unify (UVar)
import           Lamdu.Calc.Infer (InferState, PureInfer(..), runPureInfer)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Sugar.Convert.Load (unmemoizedInfer, InferFunc)

import           Lamdu.Prelude

-- Like InferFunc but has the form of (a -> b) where *entire* input is
-- inside "a" and *entire* output is inside "b"
type MemoableInferFunc =
    ( Definition.Expr (Val ())
    , V.Scope # UVar
    , InferState
    ) ->
    Either (T.TypeError # UVar)
    (Ann (InferResult UVar) # V.Term, V.Scope # UVar, InferState)

newtype Functions = Functions
    { inferMemoized :: MemoableInferFunc
    }

rZipMatch ::
    forall h a b.
    (RTraversable h, Recursively ZipMatch h) =>
    Ann a # h -> Ann b # h ->
    Maybe (Ann (a :*: b) # h)
rZipMatch (Ann a0 b0) (Ann a1 b1) =
    withDict (recurse (Proxy @(RTraversable h))) $
    withDict (recursively (Proxy @(ZipMatch h))) $
    zipMatchA
    ( Proxy @RTraversable #*# Proxy @(Recursively ZipMatch) #> rZipMatch
    ) b0 b1
    & join
    <&> Ann (a0 :*: a1)

-- | We know that inferMemoized retains the shape, so we strip the
-- payload and cover it after
infer :: forall a. Functions -> InferFunc a
infer funcs defExpr =
    fmap (_1 %~ unvoid) . PureInfer . RWST $
    \env s ->
    inferMemoized funcs (defExpr <&> hflipped %~ hmap (const (const (Const ()))), env, s)
    <&> \(iterm, topLevelScope, s') -> ((iterm, topLevelScope), s', ())
    where
        origExpr = defExpr ^. Definition.expr
        unvoid ::
            Ann (InferResult UVar) # V.Term ->
            Ann (a :*: InferResult UVar) # V.Term
        unvoid resExpr =
            rZipMatch origExpr resExpr
            & fromMaybe (error "cached expression of wrong shape!")

memoableInfer :: MemoableInferFunc
memoableInfer (expr, env, state) =
    unmemoizedInfer expr & runPureInfer env state
    <&>
    \((resTerm, topLevelScope), newState) ->
    ( resTerm & hflipped %~ hmap (const (^. _2))
    , topLevelScope
    , newState
    )

decl :: Decl Functions
decl =
    Functions
    <$> function memoableInfer

make :: IO (FencedCache.Cache, Functions)
make = FencedCache.make decl
