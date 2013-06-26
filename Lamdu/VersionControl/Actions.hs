module Lamdu.VersionControl.Actions
  ( Actions(..)
  ) where

import Data.Store.Rev.Branch (Branch)
import qualified Graphics.UI.Bottle.Widget as Widget

data Actions t m = Actions
  { branches :: [Branch t]
  , currentBranch :: Branch t
  , setCurrentBranch :: Branch t -> m ()
  , deleteBranch :: Branch t -> m (Branch t)
  , makeBranch :: m (Branch t)
  , mUndo :: Maybe (m Widget.Id)
  , mRedo :: Maybe (m Widget.Id)
  }
