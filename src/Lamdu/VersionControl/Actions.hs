{-# LANGUAGE RankNTypes #-}
module Lamdu.VersionControl.Actions
    ( Actions(..), hoist
    ) where

import Control.Lens
import GUI.Momentu.State (GUIState)
import Revision.Deltum.Rev.Branch (Branch)

import Lamdu.Prelude

data Actions t m = Actions
    { branches :: [Branch t]
    , currentBranch :: Branch t
    , setCurrentBranch :: Branch t -> m ()
    , deleteBranch :: Branch t -> m (Branch t)
    , makeBranch :: m (Branch t)
    , mUndo :: Maybe (m GUIState)
    , mRedo :: Maybe (m GUIState)
    }

hoist :: (forall a. m a -> n a) -> Actions t m -> Actions t n
hoist f (Actions bs cb setCb delB mkBranch mU mR) =
    Actions bs cb (f . setCb) (f . delB) (f mkBranch) (mU <&> f) (mR <&> f)
