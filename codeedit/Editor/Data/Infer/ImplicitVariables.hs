module Editor.Data.Infer.ImplicitVariables
  ( addVariables
  ) where

import Control.Applicative ((<$), (<$>))
import Control.Arrow (second)
import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Trans.State (runState)
import Data.Hashable (hash)
import Data.Maybe (fromMaybe)
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.Infer as Infer
import qualified System.Random as Random

onFirstHole ::
  (Data.Expression def a -> Data.Expression def a) ->
  Data.Expression def a -> Maybe (Data.Expression def a)
onFirstHole f =
  toMaybe . (`runState` False) . go
  where
    toMaybe (x, isDone) = x <$ guard isDone
    go expr@(Data.Expression body payload) = do
      isDone <- State.get
      if isDone
        then return expr
        else
          case body of
          Data.ExpressionLeaf Data.Hole ->
            let newExpr = f expr
            in newExpr <$ State.put True
          _ ->
            (`Data.Expression` payload) <$>
            Traversable.mapM go body

addVariablesGen ::
  RandomGen g => g -> Infer.RefMap ->
  Infer.Expression a -> (Infer.RefMap, Infer.Expression a)
addVariablesGen gen refMap expr =
  case mFirstHoleReplaced of
  Nothing -> (refMap, expr)
  Just replacedType ->
    let
      (newExpr, newRefMap) =
        fromMaybe
        (error
         "Adding type-variable should never fail type checking") $
        Infer.updateAndInfer actions refMap [(typeRef, replacedType)]
        expr
    in
      -- TODO: This is a hack, because it makes the inferred payload
      -- go out of sync with the inferred refs/refmap. Need to add the
      -- ability to "reparent" the root under the lambda properly.
      (second . Lens.over Data.ePayload) wrapLambda $
      addVariablesGen newGen newRefMap newExpr
  where
    wrapLambda inferred =
      -- TODO: Use lens
      inferred
      { Infer.iType =
        Data.pureExpression . Data.makePi paramGuid Data.pureSet $
        Infer.iType inferred
      }
    mFirstHoleReplaced =
      onFirstHole (const getVar) exprType
    typeRef =
      Infer.tvType . Infer.nRefs . Infer.iPoint $
      expr ^. Data.ePayload
    getVar = Data.pureExpression $ Data.makeParameterRef paramGuid
    (paramGuid, newGen) = random gen
    exprType = Infer.iType $ expr ^. Data.ePayload
    actions = Infer.InferActions (const Nothing)

addVariables ::
  Infer.RefMap -> Infer.Expression a -> (Infer.RefMap, Infer.Expression a)
addVariables = addVariablesGen . Random.mkStdGen $ hash "AddVars"
