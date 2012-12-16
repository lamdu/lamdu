{-# LANGUAGE TypeOperators, OverloadedStrings, RankNTypes #-}
module Lamdu.BranchGUI
  ( make
  , branchNameProp
  ) where

import Control.Applicative (pure)
import Control.Arrow (first)
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.VersionControl.Actions (Actions(..))
import Lamdu.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Edges as Edges
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

branchNameFDConfig :: FocusDelegator.Config
branchNameFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyF2
  , FocusDelegator.startDelegatingDoc = E.Doc ["Branches", "Rename"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Branches", "Done renaming"]
  }

branchSelectionFocusDelegatorConfig :: FocusDelegator.Config
branchSelectionFocusDelegatorConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Branches", "Select"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Branches", "Choose selected"]
  }

undoEventMap :: Functor m => Maybe (m Widget.Id) -> Widget.EventHandlers m
undoEventMap =
  maybe mempty (Widget.keysEventMapMovesCursor Config.undoKeys (E.Doc ["Edit", "Undo"]))

redoEventMap :: Functor m => Maybe (m Widget.Id) -> Widget.EventHandlers m
redoEventMap =
  maybe mempty (Widget.keysEventMapMovesCursor Config.redoKeys (E.Doc ["Edit", "Redo"]))

branchNameProp ::
  MonadA m => Branch (Tag m) -> Transaction m (Transaction.Property m String)
branchNameProp = Transaction.assocDataRefDef "" "name" . Branch.guid

make ::
  (MonadA m, MonadA n) =>
  (forall a. Transaction n a -> m a) ->
  Widget.Size -> Actions (Tag n) m -> Widget m ->
  WidgetEnvT m (Widget m)
make transaction size actions widget = do
  branchSelectorFocused <-
    fmap isJust $ WE.subCursor WidgetIds.branchSelection
  branchSelector <-
    flip
    (BWidgets.wrapDelegatedOT
     branchSelectionFocusDelegatorConfig
     FocusDelegator.NotDelegating id)
    WidgetIds.branchSelection $ \innerId ->
    WE.assignCursor innerId currentBranchWidgetId $ do
      branchNameEdits <- traverse makeBranchNameEdit $ branches actions
      return .
        Widget.strongerEvents delBranchEventMap .
        makeBranchChoice branchSelectorFocused
        (Widget.toAnimId WidgetIds.branchSelection)
        Box.vertical branchNameEdits $ currentBranch actions
  return .
    Widget.strongerEvents eventMap $
    Edges.makeVertical size widget branchSelector
  where
    eventMap = mconcat
    -- TODO: Get Quit out of here
      [ Widget.keysEventMap Config.quitKeys (E.Doc ["Quit"]) (error "Quit")
      , Widget.keysEventMapMovesCursor Config.makeBranchKeys (E.Doc ["Branches", "New"]) .
        fmap
        (FocusDelegator.delegatingId .
         WidgetIds.fromGuid . Branch.guid) $ makeBranch actions
      , Widget.keysEventMapMovesCursor Config.jumpToBranchesKeys
        (E.Doc ["Branches", "Select"]) $ pure currentBranchWidgetId
      , undoEventMap $ mUndo actions
      , redoEventMap $ mRedo actions
      ]
    makeBranchNameEdit branch = do
      let branchEditId = WidgetIds.fromGuid $ Branch.guid branch
      nameProp <-
        lift . transaction . (fmap . Lens.over (Property.pSet . Lens.mapped)) transaction $
        branchNameProp branch
      branchNameEdit <-
        BWidgets.wrapDelegatedOT branchNameFDConfig
        FocusDelegator.NotDelegating id
        (BWidgets.makeLineEdit nameProp)
        branchEditId
      let setBranch = setCurrentBranch actions branch
      return
        ( branch
        , (Lens.over (Widget.wMaybeEnter . Lens.mapped . Lens.mapped) .
           Lens.over Widget.enterResultEvent)
          (setBranch >>) branchNameEdit
        )
    currentBranchWidgetId = WidgetIds.fromGuid . Branch.guid $ currentBranch actions

    delBranchEventMap
      | null (drop 1 (branches actions)) = mempty
      | otherwise =
        Widget.keysEventMapMovesCursor Config.delBranchKeys (E.Doc ["Branches", "Delete"]) .
        fmap (WidgetIds.fromGuid . Branch.guid) .
        deleteBranch actions $ currentBranch actions

makeBranchChoice
  :: Eq a
  => Bool -> AnimId
  -> Box.Orientation
  -> [(a, Widget f)]
  -> a
  -> Widget f
makeBranchChoice forceExpand selectionAnimId orientation children curChild =
  maybe Box.toWidget Box.toWidgetBiased mCurChildIndex box
  where
    childFocused = any (Lens.view Widget.wIsFocused . snd) children
    pairs = (map . first) (curChild ==) children
    visiblePairs
      | childFocused || forceExpand = pairs
      | otherwise = filter fst pairs
    mCurChildIndex = findIndex fst visiblePairs
    box = Box.makeAlign 0 orientation colorizedPairs
    colorizedPairs
      -- focus shows selection already
      | childFocused = map snd visiblePairs
      -- need to show selection even as focus is elsewhere
      | otherwise = map colorize visiblePairs
      where
        colorize (True, w) = Widget.backgroundColor Layers.branchChoice selectionAnimId selectedColor w
        colorize (False, w) = w
        selectedColor = Config.selectedBranchColor
