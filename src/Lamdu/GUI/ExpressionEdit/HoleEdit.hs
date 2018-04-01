{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Literal (makeLiteralEventMap)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms (allowedSearchTermCommon)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

allowedHoleSearchTerm :: Text -> Bool
allowedHoleSearchTerm searchTerm =
    any (searchTerm &)
    [ allowedSearchTermCommon ":."
    , isPositiveNumber
    , isNegativeNumber
    , isLiteralBytes
    ]
    where
        isPositiveNumber t =
            case Text.splitOn "." t of
            [digits]             -> Text.all Char.isDigit digits
            [digits, moreDigits] -> Text.all Char.isDigit (digits <> moreDigits)
            _ -> False
        isLiteralBytes = prefixed '#' (Text.all Char.isHexDigit)
        isNegativeNumber = prefixed '-' isPositiveNumber
        prefixed char restPred t =
            case Text.uncons t of
            Just (c, rest) -> c == char && restPred rest
            _ -> False

make ::
    Monad m =>
    Sugar.Hole (T m) (Sugar.Expression (Name (T m)) (T m) ()) ->
    Sugar.Payload (Name (T m)) (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui (T m))
make hole pl =
    do
        searchTerm <- SearchMenu.readSearchTerm searchMenuId
        delKeys <- Lens.view Config.config <&> Config.delKeys
        let (litEventMap, delEventMap)
                | searchTerm == "" =
                    ( makeLiteralEventMap (hole ^. Sugar.holeOptionLiteral)
                    , foldMap
                        (E.keysEventMapMovesCursor delKeys (E.Doc ["Edit", "Delete"]) . fmap WidgetIds.fromEntityId)
                        (hole ^. Sugar.holeMDelete)
                    )
                | otherwise = (mempty, mempty)
        ExprEventMap.add options pl
            <*> ( SearchArea.make (hole ^. Sugar.holeOptions)
                    (Just (hole ^. Sugar.holeOptionLiteral)) pl allowedHoleSearchTerm ?? Menu.AnyPlace
                    <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> delEventMap)
                )
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (litEventMap <>)
    & GuiState.assignCursor (hidHole widgetIds) searchMenuId
    where
        searchMenuId = hidOpen widgetIds
        widgetIds = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
        options =
            ExprEventMap.defaultOptions
            { ExprEventMap.addOperatorSetHoleState = Just (pl ^. Sugar.plEntityId)
            }
