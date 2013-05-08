{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.Sugar.NameGen
  ( NameGen, initial
  , IsDependent(..), existingName, newName, ban
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Monad.Trans.State (State, state)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set

data IsDependent = Dependent | Independent

data NameGen g = NameGen
  { _ngUnusedDependentNames :: [String]
  , _ngUnusedIndependentNames :: [String]
  , _ngBannedNames :: Set String
  , _ngUsedNames :: Map g String
  }
Lens.makeLenses ''NameGen

ban :: Set String -> NameGen g -> NameGen g
ban newBanned = ngBannedNames <>~ newBanned

initial :: Ord g => NameGen g
initial =
  NameGen depNames indepNames Set.empty Map.empty
  where
    indepNames = numberCycle ["x", "y", "z", "w", "u", "v"]
    depNames = numberCycle $ map (:[]) ['a'..'e']
    numberCycle s = (s ++) . concat . zipWith appendAll [0::Int ..] $ repeat s
    appendAll num = map (++ show num)

existingName :: (Ord g, Show g) => g -> State (NameGen g) String
existingName g =
  fromMaybe ("TodoError:" ++ show g) <$>
  Lens.uses ngUsedNames (Map.lookup g)

newName :: Ord g => (String -> Bool) -> IsDependent -> g -> State (NameGen g) String
newName acceptName isDep g = do
  bannedNames <- Lens.use ngBannedNames
  let
    loop = do
      name <- state (head &&& tail)
      if acceptName name && name `Set.notMember` bannedNames
        then return name
        else loop
  name <- Lens.zoom (l isDep) loop
  ngUsedNames %= Map.insert g name
  return name
  where
    l Dependent = ngUnusedDependentNames
    l Independent = ngUnusedIndependentNames
