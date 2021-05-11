-- | A goto-definition widget
module Lamdu.GUI.CodeEdit.GotoDefinition
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as BS8
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Navigation as Navigation
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

nameSearchTerm :: _ => Name -> m Text
nameSearchTerm name =
    Name.visible name <&>
    \(Name.TagText text textCol, tagCol) ->
    text <> collisionText textCol <> collisionText tagCol
    where
        collisionText Name.NoCollision = ""
        collisionText (Name.Collision i) = Text.pack (show i)
        collisionText Name.UnknownCollision = "?"

makeOptions ::
    _ => m [Sugar.NameRef Name o] -> SearchMenu.ResultsContext -> m (Menu.OptionList (Menu.Option m o))
makeOptions readGlobals (SearchMenu.ResultsContext searchTerm prefix)
    | Text.null searchTerm = pure Menu.TooMany
    | otherwise =
        do
            goto <- Lens.view (has . Navigation.goto)
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
            let makeOption (idx, nameRef) =
                    GetVarEdit.makeSimpleView TextColors.definitionColor name optId
                    <&> toRenderedOption nameRef
                    & local (M.animIdPrefix .~ Widget.toAnimId optId)
                    & wrapOption optId
                    where
                        name = nameRef ^. Sugar.nrName
                        optId = prefix `Widget.joinId` [BS8.pack (show idx)]
            readGlobals <&> zip [0::Int ..]
                >>= traverse withText
                <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
                <&> map (makeOption . snd)
                <&> Menu.FullList
    where
        withText (idx, nameRef) =
            nameSearchTerm (nameRef ^. Sugar.nrName) <&> \x -> (x, (idx, nameRef))
        wrapOption optId mkRenedered =
            Menu.Option
            { Menu._oId = optId
            , Menu._oRender = mkRenedered
            , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
            }
        toPickResult x = Menu.PickResult x (Just x)

make :: _ => m [Sugar.NameRef Name o] -> m (StatusBar.StatusWidget o)
make readGlobals =
    do
        goto <- Lens.view (has . Navigation.goto)
        SearchMenu.make (SearchMenu.searchTermEdit myId (pure . allowSearchTerm))
            (makeOptions readGlobals) M.empty myId ?? Menu.Below
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
