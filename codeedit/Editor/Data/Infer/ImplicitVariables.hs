module Editor.Data.Infer.ImplicitVariables
  ( onFirstHole
  ) where

import Control.Applicative ((<$), (<$>))
import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Trans.State (runState)
import qualified Control.Monad.Trans.State as State
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.Infer as Infer

onFirstHole ::
  (Data.Expression def a -> Data.Expression def a) ->
  Data.Expression def a -> Maybe (Data.Expression def a)
onFirstHole f =
  toMaybe . (`runState` False) . go
  where
    toMaybe (x, isChanged) = x <$ guard isChanged
    go expr@(Data.Expression body payload) = do
      isDone <- State.get
      if isDone
        then return expr
        else
          case body of
          Data.ExpressionLeaf Data.Hole ->
            f expr <$ State.put True
          _ ->
            (`Data.Expression` payload) <$>
            Traversable.mapM go body
