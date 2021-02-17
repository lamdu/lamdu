{-# LANGUAGE RankNTypes #-}
module Lamdu.VersionControl.Actions
    ( Actions(..), hoist
    ) where

import Data.Property (Property(..))
import GUI.Momentu.State (GUIState)
import Revision.Deltum.Rev.Branch (Branch)

import Lamdu.Prelude

data Actions t m = Actions
    { branches :: [Branch t]
    , currentBranch :: Property m (Branch t)
    , deleteBranch :: Branch t -> m (Branch t)
    , makeBranch :: m (Branch t)
    , mUndo :: Maybe (m GUIState)
    , mRedo :: Maybe (m GUIState)
    }

hoist :: (forall a. m a -> n a) -> Actions t m -> Actions t n
hoist f (Actions bs (Property cb setCb) delB mkBranch mU mR) =
    Actions bs (Property cb (f . setCb)) (f . delB) (f mkBranch)
    (mU <&> f) (mR <&> f)
