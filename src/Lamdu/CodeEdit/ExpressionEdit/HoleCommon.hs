module Lamdu.CodeEdit.ExpressionEdit.HoleCommon
  ( adHocTextEditEventMap, disallowChars, makeSearchTermWidget
  ) where

import Control.Applicative ((<$))
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Config as Config

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
  Property (T m) String -> Widget.Id -> Widget.EventHandlers (T m) ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermWidget searchTermProp searchTermId pickResultEventMap =
  ExprGuiM.widgetEnv .
  fmap
  (flip ExpressionGui (0.5 / Config.holeSearchTermScaleFactor) .
   Widget.scale Config.holeSearchTermScaleFactor .
   Widget.strongerEvents pickResultEventMap .
   Lens.over Widget.wEventMap disallowChars) $
  BWidgets.makeWordEdit searchTermProp searchTermId
