{-# LANGUAGE OverloadedStrings #-}

module Lamdu.CodeEdit.ExpressionEdit.ExpressionGui
  ( ExpressionGui(..), egWidget
  , fromValueWidget
  , hbox, hboxSpaced, addBelow
  , addType -- TODO: s/type/info
  , TypeStyle(..)
  , parenify, wrapExpression, wrapParenify, wrapDelegated
  -- ExprGuiM:
  , makeNameEdit
  , nameSrcTint
  , withBgColor
  -- TODO: Maybe move to ExpressionGui.Collapser:
  , Collapser(..), makeCollapser
  ) where

import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId, Layer)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Box (KBox)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Types (WidgetT, ExpressionGui(..), egWidget, egAlignment)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

fromValueWidget :: WidgetT m -> ExpressionGui m
fromValueWidget widget = ExpressionGui widget 0.5

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox guis =
  ExpressionGui (Box.toWidget box) $
  case box ^. Box.boxContent of
  ((_, x) : _) -> x ^. Grid.elementAlign . Vector2.second
  _ -> error "hbox must not get empty list :("
  where
    box = Box.make Box.horizontal $ map f guis
    f (ExpressionGui widget alignment) = (Vector2 0.5 alignment, widget)

hboxSpaced :: [ExpressionGui m] -> ExpressionGui m
hboxSpaced = hbox . List.intersperse (fromValueWidget BWidgets.stdSpaceWidget)

fromBox :: KBox Bool (Transaction m) -> ExpressionGui m
fromBox box =
  ExpressionGui (Box.toWidget box) alignment
  where
    alignment =
      maybe (error "True disappeared from box list?!")
        (Lens.view (Grid.elementAlign . Vector2.second)) .
      lookup True $ box ^. Box.boxContent

addBelow ::
  [(Box.Alignment, WidgetT m)] ->
  ExpressionGui m ->
  ExpressionGui m
addBelow ws eg =
  fromBox . Box.makeKeyed Box.vertical $
  (True, (Vector2 0.5 (eg ^. egAlignment), eg ^. egWidget)) :
  map ((,) False) ws

data TypeStyle = HorizLine | Background

wWidth :: Lens.SimpleLens (Widget f) Widget.R
wWidth = Widget.wSize . Vector2.first

addType ::
  TypeStyle ->
  Widget.Id ->
  [WidgetT m] ->
  ExpressionGui m ->
  ExpressionGui m
addType _ _ [] eg = eg
addType style exprId typeEdits eg =
  addBelow items eg
  where
    items = middleElement : [(0.5, annotatedTypes)]
    middleElement =
      case style of
      HorizLine -> (0.5, Spacer.makeHorizLine underlineId (Vector2 width 1))
      Background -> (0.5, Spacer.makeWidget 5)
    annotatedTypes =
      addBackground . Lens.set wWidth width $
      Widget.translate (Vector2 ((width - typesBox ^. wWidth)/2) 0) typesBox
    width = on max (Lens.view wWidth) (eg ^. egWidget) typesBox
    typesBox = Box.vboxCentered typeEdits
    isError = length typeEdits >= 2
    bgAnimId = Widget.toAnimId exprId ++ ["type background"]
    addBackground = maybe id (Widget.backgroundColor Layers.types bgAnimId) bgColor
    bgColor
      | isError = Just Config.inferredTypeErrorBGColor
      | otherwise = do
        Background <- Just style
        return Config.inferredTypeBGColor
    underlineId = WidgetIds.underlineId $ Widget.toAnimId exprId

exprFocusDelegatorConfig :: FocusDelegator.Config
exprFocusDelegatorConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.enterSubexpressionKey
  , FocusDelegator.startDelegatingDoc = EventMap.Doc ["Navigation", "Enter subexpression"]
  , FocusDelegator.stopDelegatingKey = Config.leaveSubexpressionKey
  , FocusDelegator.stopDelegatingDoc = EventMap.Doc ["Navigation", "Leave subexpression"]
  }

-- ExprGuiM GUIs (TODO: Move to Monad.hs?)

makeNameEdit ::
  MonadA m => (ExprGuiM.NameSource, String) -> Guid -> Widget.Id -> ExprGuiM m (WidgetT m)
makeNameEdit (nameSrc, name) ident myId =
  fmap (nameSrcTint nameSrc) .
  (ExprGuiM.atEnv . Lens.over WE.envTextStyle)
  (Lens.set TextEdit.sEmptyUnfocusedString name .
   Lens.set TextEdit.sEmptyFocusedString (concat ["<", name, ">"])) $
  ExprGuiM.widgetEnv . flip makeEditor myId =<<
  (ExprGuiM.transaction . Anchors.assocNameRef) ident
  where
    makeEditor =
      (fmap . fmap . fmap . Lens.over Widget.wEventMap)
      (EventMap.filterChars (`notElem` "[]\\`"))
      BWidgets.makeWordEdit

nameSrcTint :: ExprGuiM.NameSource -> Widget f -> Widget f
nameSrcTint ExprGuiM.AutoGeneratedName = Widget.tint Config.autoGeneratedNameTint
nameSrcTint ExprGuiM.StoredName = id

wrapDelegated ::
  (MonadA f, MonadA m) =>
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
wrapDelegated fdConfig isDelegating =
  ExprGuiM.wrapDelegated fdConfig isDelegating $ Lens.over egWidget

wrapExpression ::
  (MonadA m, MonadA f) =>
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
wrapExpression =
  wrapDelegated exprFocusDelegatorConfig FocusDelegator.Delegating

parenify ::
  MonadA m =>
  Sugar.HasParens ->
  (Widget.Id -> ExpressionGui f -> ExprGuiM m (ExpressionGui f)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
parenify Sugar.DontHaveParens _ mkWidget myId = mkWidget myId
parenify Sugar.HaveParens addParens mkWidget myId = addParens myId =<< mkWidget myId

wrapParenify ::
  (MonadA f, MonadA m) =>
  Sugar.HasParens ->
  (Widget.Id -> ExpressionGui f -> ExprGuiM m (ExpressionGui f)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
wrapParenify hasParens addParens =
  wrapExpression . parenify hasParens addParens

withBgColor :: Layer -> Draw.Color -> AnimId -> ExpressionGui m -> ExpressionGui m
withBgColor layer color animId =
  Lens.over egWidget $ Widget.backgroundColor layer animId color

data Collapser m = Collapser
  { cMakeExpanded :: ExprGuiM m (ExpressionGui m)
  , cOnFocusedExpanded :: ExpressionGui m -> ExpressionGui m
  , cMakeFocusedCompact :: ExprGuiM m (ExpressionGui m)
  }

makeCollapser ::
  MonadA m =>
  FocusDelegator.Config ->
  (Widget.Id -> Collapser m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeCollapser fdConfig f =
  wrapDelegated fdConfig FocusDelegator.NotDelegating $
  \myId -> do
    let Collapser makeExpanded onFocusedExpanded makeFocusedCompact = f myId
    -- TODO: This is just to detect whether cursor is in the full
    -- expression.  Even when it's not displayed, which may be wasteful
    -- (even with laziness, at least the names are going to be read).
    expandedEdit <- makeExpanded
    -- We are inside a focus delegator, so if the cursor is on us it
    -- means user entered our widget.
    if expandedEdit ^. egWidget . Widget.wIsFocused
      then return $ onFocusedExpanded expandedEdit
      else makeFocusedCompact
