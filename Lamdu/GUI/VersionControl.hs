{-# LANGUAGE TypeOperators, OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.VersionControl (make) where

import Control.Applicative (Applicative, (<$>), pure)
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Config (Config)
import Lamdu.VersionControl.Actions (Actions(..))
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Data.List.Utils as ListUtils
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Edges as Edges
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds

branchNameFDConfig :: FocusDelegator.Config
branchNameFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyF2]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Branches", "Rename"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Branches", "Done renaming"]
  }

undoEventMap :: Functor m => Config -> Maybe (m Widget.Id) -> Widget.EventHandlers m
undoEventMap config =
  maybe mempty .
  Widget.keysEventMapMovesCursor (Config.undoKeys config) $ E.Doc ["Edit", "Undo"]

redoEventMap :: Functor m => Config -> Maybe (m Widget.Id) -> Widget.EventHandlers m
redoEventMap config =
  maybe mempty .
  Widget.keysEventMapMovesCursor (Config.redoKeys config) $ E.Doc ["Edit", "Redo"]

globalEventMap :: Applicative f => Config -> Actions t f -> Widget.EventHandlers f
globalEventMap config actions = mconcat
  [ Widget.keysEventMapMovesCursor (Config.makeBranchKeys config)
    (E.Doc ["Branches", "New"]) $
    FocusDelegator.delegatingId . WidgetIds.fromGuid . Branch.guid <$>
    makeBranch actions
  , Widget.keysEventMapMovesCursor (Config.jumpToBranchesKeys config)
    (E.Doc ["Branches", "Select"]) $
    pure currentBranchWidgetId
  , undoEventMap config $ mUndo actions
  , redoEventMap config $ mRedo actions
  ]
  where
    currentBranchWidgetId = WidgetIds.fromGuid . Branch.guid $ currentBranch actions

choiceWidgetConfig :: Config -> BWidgets.ChoiceWidgetConfig
choiceWidgetConfig config = BWidgets.ChoiceWidgetConfig
  { BWidgets.cwcFDConfig =
    FocusDelegator.Config
    { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
    , FocusDelegator.startDelegatingDoc = E.Doc ["Branches", "Select"]
    , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
    , FocusDelegator.stopDelegatingDoc = E.Doc ["Branches", "Choose selected"]
    }
  , BWidgets.cwcExpandMode =
      BWidgets.AutoExpand $ Config.selectedBranchColor config
  , BWidgets.cwcOrientation = Box.vertical
  , BWidgets.cwcBgLayer = Config.layerChoiceBG $ Config.layers config
  }

make ::
  (MonadA m, MonadA n) =>
  (forall a. Transaction n a -> m a) ->
  Widget.Size -> Actions (Tag n) m -> Widget m ->
  WidgetEnvT m (Widget m)
make transaction size actions widget = do
  config <- WE.readConfig
  let
    makeBranchNameEdit branch = do
      let
        branchGuid = Branch.guid branch
        branchEditId = WidgetIds.fromGuid branchGuid
      nameProp <-
        lift . transaction . (Lens.mapped . Property.pSet . Lens.mapped %~ transaction) $
        Anchors.assocNameRef branchGuid ^. Transaction.mkProperty
      branchNameEdit <-
        BWidgets.wrapDelegatedOT branchNameFDConfig
        FocusDelegator.NotDelegating id
        (BWidgets.makeLineEdit nameProp)
        branchEditId
      let
        delEventMap
          | ListUtils.isLengthAtLeast 2 (branches actions) =
            Widget.keysEventMapMovesCursor
            (Config.delBranchKeys config) (E.Doc ["Branches", "Delete"])
            (WidgetIds.fromGuid . Branch.guid <$> deleteBranch actions branch)
          | otherwise = mempty
      return (branch, branchNameEdit & Widget.weakerEvents delEventMap)
  branchNameEdits <- traverse makeBranchNameEdit $ branches actions
  branchSelector <-
    BWidgets.makeChoiceWidget (setCurrentBranch actions)
    branchNameEdits (currentBranch actions) (choiceWidgetConfig config)
    WidgetIds.branchSelection
  return .
    Widget.strongerEvents (globalEventMap config actions) $
    Edges.makeVertical size widget branchSelector
