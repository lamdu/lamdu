{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}
module Lamdu.Sugar.Names.NameGen
    ( NameGen, initial
    , VarInfo(..), existingName, newName
    ) where

import           Control.Arrow ((&&&))
import qualified Control.Lens as Lens
import           Control.Monad.Trans.State (State, state)
import qualified Data.Map as Map
import qualified Data.Text as Text

import           Lamdu.Prelude

data VarInfo = Function | NormalVar

data NameGen g = NameGen
    { _ngUnusedNames :: [Text]
    , _ngUnusedFuncNames :: [Text]
    , _ngUsedNames :: Map g Text
    }
Lens.makeLenses ''NameGen

initial :: NameGen g
initial =
    NameGen indepNames indepFuncNames Map.empty
    where
        indepFuncNames = numberCycle ["f", "g", "h"]
        indepNames = numberCycle ["x", "y", "z", "w", "u", "v"]
        numberCycle s = (s <>) . mconcat . zipWith appendAll [0::Int ..] $ repeat s
        appendAll num = map (<> Text.pack (show num))

existingName :: (Ord g, Show g) => g -> State (NameGen g) Text
existingName g =
    fromMaybe ("TodoError:" <> Text.pack (show g)) <$>
    Lens.uses ngUsedNames (Map.lookup g)

newName :: Ord g => (Text -> Bool) -> VarInfo -> g -> State (NameGen g) Text
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
