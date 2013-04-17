{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleCommon
  ( adHocTextEditEventMap, disallowChars, makeSearchTermWidget, holeFDConfig
  , makeInactive, makeBackground, resultsPrefixId
  ) where

import Control.Applicative ((<$))
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers

type T = Transaction

adHocTextEditEventMap :: MonadA m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap textProp =
  mconcat . concat $
  [ [ disallowChars .
      E.simpleChars "Character"
      (E.Doc ["Edit", "Search Term", "Append character"]) $
      changeText . flip (++) . (: [])
    ]
  , [ E.keyPresses (map (E.ModKey E.noMods) [E.KeyBackspace])
      (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
      changeText init
    | (not . null . Property.value) textProp
    ]
  ]
  where
    changeText f = Widget.emptyEventResult <$ Property.pureModify textProp f

disallowedHoleChars :: [(Char, E.IsShifted)]
disallowedHoleChars =
  E.anyShiftedChars ",`\n() " ++
  [ ('0', E.Shifted)
  , ('9', E.Shifted)
  ]

disallowChars :: E.EventMap a -> E.EventMap a
disallowChars = E.filterSChars $ curry (`notElem` disallowedHoleChars)

makeSearchTermWidget ::
  MonadA m =>
  Property (T m) String -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermWidget searchTermProp searchTermId =
  ExprGuiM.widgetEnv .
  fmap
  (flip ExpressionGui (0.5 / Config.holeSearchTermScaleFactor) .
   Widget.scale Config.holeSearchTermScaleFactor .
   Lens.over Widget.wEventMap disallowChars) $
  BWidgets.makeWordEdit searchTermProp searchTermId

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Navigation", "Hole", "Enter"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Navigation", "Hole", "Leave"]
  }

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]

makeInactive ::
  MonadA m => Bool -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactive isReadOnly myId =
  fmap
  (ExpressionGui.fromValueWidget .
   makeBackground myId Layers.inactiveHole unfocusedColor) .
  ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "  " myId
  where
    unfocusedColor
      | isReadOnly = Config.holeBackgroundColor
      | otherwise = Config.readOnlyHoleBackgroundColor

resultsPrefixId :: Widget.Id -> Widget.Id
resultsPrefixId holeId = mconcat [holeId, Widget.Id ["results"]]
