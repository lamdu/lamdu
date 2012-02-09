module Editor.CodeEdit.LiteralEdit(makeInt, makeIntView) where

import Control.Monad (liftM)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import Editor.CTransaction (CTransaction, TWidget)
import Editor.MonadF(MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

setColor :: TWidget t m -> TWidget t m
setColor = BWidgets.setTextColor Config.literalIntColor

makeIntView ::
  (MonadF m) => Widget.Id -> Integer ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m))
makeIntView myId integer =
  setColor $ BWidgets.makeTextView (show integer) myId

makeInt ::
  (Monad m) => IRef Data.Expression -> Integer ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
makeInt expressionI integer =
  liftM (flip (,) (WidgetIds.fromIRef expressionI)) . setColor $
    BWidgets.makeWordEdit intAsStr myId
  where
    expressionRef = Transaction.fromIRef expressionI
    intAsStr =
      Property.pureCompose
        (const (show integer))
        (Data.ExpressionLiteralInteger . read . ("0" ++)) expressionRef
    myId = WidgetIds.fromIRef expressionI
