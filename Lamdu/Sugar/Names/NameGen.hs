{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.Sugar.Names.NameGen
    ( NameGen, initial
    , VarInfo(..), existingName, newName
    ) where

import           Prelude.Compat

import           Control.Arrow ((&&&))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.State (State, state)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

data VarInfo = Function | NormalVar

data NameGen g = NameGen
    { _ngUnusedNames :: [String]
    , _ngUnusedFuncNames :: [String]
    , _ngUsedNames :: Map g String
    }
Lens.makeLenses ''NameGen

initial :: Ord g => NameGen g
initial =
    NameGen indepNames indepFuncNames Map.empty
    where
        indepFuncNames = numberCycle ["f", "g", "h"]
        indepNames = numberCycle ["x", "y", "z", "w", "u", "v"]
        numberCycle s = (s ++) . concat . zipWith appendAll [0::Int ..] $ repeat s
        appendAll num = map (++ show num)

existingName :: (Ord g, Show g) => g -> State (NameGen g) String
existingName g =
    fromMaybe ("TodoError:" ++ show g) <$>
    Lens.uses ngUsedNames (Map.lookup g)

newName :: Ord g => (String -> Bool) -> VarInfo -> g -> State (NameGen g) String
newName acceptName isFunc g =
    do
        name <- Lens.zoom names loop
        ngUsedNames %= Map.insert g name
        return name
    where
        loop =
            do
                name <- state (head &&& tail)
                if acceptName name
                    then return name
                    else loop
        names =
            case isFunc of
            NormalVar -> ngUnusedNames
            Function -> ngUnusedFuncNames
