{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Names.NameGen
    ( NameGen, initial
    , existingName, newName
    ) where

import           Control.Arrow ((&&&))
import qualified Control.Lens as Lens
import           Control.Monad.Trans.State (State, state)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Lamdu.Sugar.Types.Parts (VarInfo(..))

import           Lamdu.Prelude

data NameGen g = NameGen
    { _ngUnusedNames :: [Text]
    , _ngUnusedFuncNames :: [Text]
    , _ngUnusedActionNames :: [Text]
    , _ngUsedNames :: Map g Text
    }
Lens.makeLenses ''NameGen

initial :: NameGen g
initial =
    NameGen
    { _ngUnusedNames = numberCycle ["x", "y", "z", "w", "u", "v"]
    , _ngUnusedFuncNames = numberCycle ["f", "g", "h"]
    , _ngUnusedActionNames = numberCycle ["a", "b", "c"]
    , _ngUsedNames = Map.empty
    }
    where
        numberCycle s = (s <>) . mconcat . Lens.imap appendAll $ repeat s
        appendAll num = map (<> Text.pack (show num))

existingName :: (Ord g, Show g) => g -> State (NameGen g) Text
existingName g =
    fromMaybe ("TodoError:" <> Text.pack (show g)) <$>
    Lens.uses ngUsedNames (Map.lookup g)

newName :: Ord g => VarInfo -> (Text -> Bool) -> g -> State (NameGen g) Text
newName varInfo =
    case varInfo of
    VarNormal -> ngUnusedNames
    VarFunction -> ngUnusedFuncNames
    VarAction -> ngUnusedActionNames
    & newNameOf

newNameOf ::
    Ord g =>
    Lens.ALens' (NameGen g) [Text] -> (Text -> Bool) -> g -> State (NameGen g) Text
newNameOf names acceptName g =
    do
        name <- Lens.zoom (Lens.cloneLens names) loop
        ngUsedNames %= Map.insert g name
        pure name
    where
        loop =
            do
                name <- state (head &&& tail)
                if acceptName name
                    then pure name
                    else loop
