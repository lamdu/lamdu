module Editor.VersionControl.Actions
  ( Actions(..)
  ) where

import Data.Store.Rev.Branch (Branch)
import qualified Graphics.UI.Bottle.Widget as Widget

data Actions m = Actions
  { branches :: [Branch]
  , currentBranch :: Branch
  , setCurrentBranch :: Branch -> m ()
  , deleteBranch :: Branch -> m Branch
  , makeBranch :: m Branch
  , mUndo :: Maybe (m Widget.Id)
  , mRedo :: Maybe (m Widget.Id)
  }
