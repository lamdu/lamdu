module Lamdu.GUI.Expr.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.HoleEdit.ResultGroups as ResultGroups
import qualified Lamdu.GUI.Expr.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.Expr.HoleEdit.ValTerms (allowedSearchTerm)
import           Lamdu.GUI.Expr.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.Expr.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Types as ExprGui
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

allowedHoleSearchTerm :: Text -> Bool
allowedHoleSearchTerm searchTerm =
    any (searchTerm &)
    [ allowedSearchTerm
    , isLiteralBytes
    ]
    where
        isLiteralBytes = prefixed '#' (Text.all Char.isHexDigit)
        prefixed char restPred t =
            case Text.uncons t of
            Just (c, rest) -> c == char && restPred rest
            _ -> False

make :: _ => Annotated (ExprGui.Payload i o) # Const (Sugar.Hole Name i o) -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Const hole)) =
    do
        searchTerm <- SearchMenu.readSearchTerm searchMenuId
        let mkLitEventMap
                | searchTerm == "" = ExprEventMap.makeLiteralEventMap ?? pl ^. Sugar.plActions
                | searchTerm == "-" = ExprEventMap.makeLiteralNumberEventMap "-" ?? pl ^. Sugar.plActions . Sugar.setToLiteral
                | otherwise = mempty
        litEventMap <- mkLitEventMap
        (ExprEventMap.add options pl <&> (Align.tValue %~))
            <*> ( SearchArea.make ResultGroups.PreferLocals SearchArea.WithAnnotation (hole ^. Sugar.holeOptions)
                    pl allowedHoleSearchTerm widgetIds ?? Menu.AnyPlace
                )
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (litEventMap <>)
            <&> Responsive.fromWithTextPos
    where
        searchMenuId = hidOpen widgetIds
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        options =
            ExprEventMap.defaultOptions
            { ExprEventMap.addOperatorSetHoleState = Just (pl ^. Sugar.plEntityId)
            }
