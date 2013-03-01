module Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.NameGen
  ( NameGen, initial, getName
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.State (State, state)
import Data.Map (Map)
import qualified Data.Map as Map

data NameGen g = NameGen
  { _ngUnusedNames :: [String]
  , _ngUsedNames :: Map g String
  }

initial :: Ord g => NameGen g
initial =
  NameGen names Map.empty
  where
    alphabet = map (:[]) ['a'..'z']
    names = alphabet ++ ((++) <$> names <*> alphabet)

getName :: Ord g => g -> State (NameGen g) String
getName g = state $ \gen@(NameGen (nextName:nextNames) used) ->
  case Map.lookup g used of
  Nothing ->
    ( nextName
    , NameGen nextNames $ Map.insert g nextName used
    )
  Just name -> (name, gen)
