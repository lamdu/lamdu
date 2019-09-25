-- | Preprocess of input to sugar
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferredType, inferResult, stored, evalResults, userData, localsInScope
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , preparePayloads
    , initLocalsInScope
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Hyper (Tree, Pure, Ann(..), traverseK1)
import           Hyper.Unify.Binding (UVar)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (ValP)
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ER.ScopeId (ER.Val (Tree Pure Type))
    , _eAppliesOfLam :: Map ER.ScopeId [(ER.ScopeId, ER.Val (Tree Pure Type))]
    }

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferredType :: Tree Pure Type
    , -- The inference result before binding universal quantifiers,
      -- Useful for resuming inference in holes.
      _inferResult :: Tree V.IResult UVar
    , _localsInScope :: [V.Var]
    , _stored :: ValP m
    , _evalResults :: CurAndPrev EvalResultsForExpr
    , -- The GetVars of this lambda's var if this is a lambda
      _varRefsOfLambda :: [EntityId]
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''EvalResultsForExpr
Lens.makeLenses ''Payload

emptyEvalResults :: EvalResultsForExpr
emptyEvalResults = EvalResultsForExpr Map.empty Map.empty

preparePayloads :: Val (EntityId, [EntityId] -> pl) -> Val pl
preparePayloads =
    snd . go
    where
        go :: Val (EntityId, [EntityId] -> pl) -> (Map V.Var [EntityId], Val pl)
        go (Ann (x, mkPayload) body) =
            ( childrenVars
              & case body of
                V.BLeaf (V.LVar var) -> Lens.at var <>~ Just [x]
                V.BLam (V.Lam var _) -> Lens.at var .~ Nothing
                _ -> id
            , b & traverseK1 %~ (^. Lens._Wrapped . _2)
              & Ann
                ( case body of
                  V.BLam (V.Lam var _) -> childrenVars ^. Lens.ix var
                  _ -> []
                  & mkPayload
                )
            )
            where
                childrenVars = Map.unionsWith (++) (b ^.. traverseK1 . Lens._Wrapped . _1)
                b = body & traverseK1 %~ Lens.Const . go

initLocalsInScope :: [V.Var] -> Val (Payload m a) -> Val (Payload m a)
initLocalsInScope locals (Ann pl body) =
    case body of
    V.BLam (V.Lam var b) -> initLocalsInScope (var : locals) b & V.Lam var & V.BLam
    x -> x & traverseK1 %~ initLocalsInScope locals
    & Ann (pl & localsInScope <>~ locals)
