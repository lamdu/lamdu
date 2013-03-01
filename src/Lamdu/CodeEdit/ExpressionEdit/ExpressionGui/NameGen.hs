{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.NameGen
  ( NameGen, initial
  , IsDependent(..), getName
  ) where

import Control.Arrow ((&&&))
import Control.Lens ((%=))
import Control.Monad.Trans.State (State, state)
import Data.Map (Map)
import qualified Control.Lens as Lens
import qualified Data.Map as Map

data IsDependent = Dependent | Independent

data NameGen g = NameGen
  { _ngUnusedDependentNames :: [String]
  , _ngUnusedIndependentNames :: [String]
  , _ngUsedNames :: Map g String
  }
Lens.makeLenses ''NameGen

initial :: Ord g => NameGen g
initial =
  NameGen depNames indepNames Map.empty
  where
    indepNames = numberCycle ["x", "y", "z", "w", "u", "v"]
    depNames = numberCycle $ map (:[]) ['a'..'e']
    numberCycle s = (s ++) . concat . zipWith appendAll [0::Int ..] $ repeat s
    appendAll num = map (++ show num)

getName :: Ord g => IsDependent -> g -> State (NameGen g) String
getName isDep g = do
  existing <- Lens.uses ngUsedNames $ Map.lookup g
  maybe (newName isDep g) return existing

newName :: Ord g => IsDependent -> g -> State (NameGen g) String
newName isDep g = do
  name <- Lens.zoom (l isDep) $ state (head &&& tail)
  ngUsedNames %= Map.insert g name
  return name
  where
    l Dependent = ngUnusedDependentNames
    l Independent = ngUnusedIndependentNames
