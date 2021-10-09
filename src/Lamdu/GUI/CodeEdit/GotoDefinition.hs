-- | A goto-definition widget
module Lamdu.GUI.CodeEdit.GotoDefinition
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as BS8
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

myId :: Widget.Id
myId = Widget.Id ["goto-def"]

allowSearchTerm :: Text -> Bool
allowSearchTerm = Name.isValidText

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

nameToText :: _ => Name -> m Text
nameToText name =
    Name.visible name <&>
    \(Name.TagText text textCol, tagCol) ->
    text <> collisionText textCol <> collisionText tagCol
    where
        collisionText Name.NoCollision = ""
        collisionText (Name.Collision i) = Text.pack (show i)
        collisionText Name.UnknownCollision = "?"

makeOptions ::
    ( MonadReader env m, Has (Texts.Navigation Text) env, Has (Texts.Name Text) env
    , M.HasCursor env, M.HasAnimIdPrefix env, Has Theme env, Has TextView.Style env
    , Has Dir.Layout env
    , Applicative o
    ) =>
    Sugar.Globals Name m o -> SearchMenu.ResultsContext -> m (Menu.OptionList (Menu.Option m o))
makeOptions globals (SearchMenu.ResultsContext searchTerm prefix)
    | Text.null searchTerm =
        pure Menu.OptionList { Menu._olIsTruncated = False, Menu._olOptions = [] }
    | otherwise =
        do
            goto <- Lens.view (has . Texts.goto)
            let toRenderedOption nameRef widget =
                    Menu.RenderedOption
                    { Menu._rWidget = widget
                    , Menu._rPick =
                        Widget.PreEvent
                        { Widget._pDesc = goto
                        , Widget._pAction =
                            nameRef ^. Sugar.nrGotoDefinition
                            <&> WidgetIds.fromEntityId <&> toPickResult
                        , Widget._pTextRemainder = ""
                        }
                    }
            let makeOption color (idx, nameRef) =
                    Menu.Option
                    { Menu._oId = optId
                    , Menu._oRender =
                        GetVarEdit.makeSimpleView color name optId
                        <&> toRenderedOption nameRef
                        & local (M.animIdPrefix .~ Widget.toAnimId optId)
                    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
                    }
                    where
                        name = nameRef ^. Sugar.nrName
                        optId = prefix `Widget.joinId` [BS8.pack (show idx)]
            let globalOpts color lens =
                    globals ^. lens <&> zip [0::Int ..]
                    >>= traverse withText
                    <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
                    <&> map (makeOption color . snd)
            (<>)
                <$> globalOpts TextColors.definitionColor Sugar.globalDefs
                <*> globalOpts TextColors.nomColor Sugar.globalNominals
                <&> Menu.OptionList isTruncated
    where
        isTruncated = False
        withText (idx, nameRef) =
            nameToText (nameRef ^. Sugar.nrName) <&> \x -> (x, (idx, nameRef))
        toPickResult x = Menu.PickResult x (Just x)

make :: _ => Sugar.Globals Name m o -> m (StatusBar.StatusWidget o)
make globals =
    do
        goto <- Lens.view (has . Texts.goto)
        SearchMenu.make (SearchMenu.searchTermEdit myId (pure . allowSearchTerm))
            (makeOptions globals) M.empty myId ?? Menu.Below
            & local (has . Theme.searchTerm %~ onTermStyle goto)
            <&> \searchWidget -> StatusBar.StatusWidget
            { StatusBar._widget = searchWidget
            , StatusBar._globalEventMap = mempty
            }
    where
        onTermStyle goto x =
            x
            & SearchMenu.emptyStrings . Lens.mapped .~ goto
            & SearchMenu.bgColors . Lens.mapped .~ M.Color 0 0 0 0
