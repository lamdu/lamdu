-- | A goto-definition widget
module Lamdu.GUI.CodeEdit.GotoDefinition
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as BS8
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

myId :: Widget.Id
myId = Widget.Id ["goto-def"]

allowSearchTerm :: Text -> Bool
allowSearchTerm = const True

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

nameSearchTerm :: Name o -> Text
nameSearchTerm name =
    text <> collisionText textCol <> collisionText tagCol
    where
        collisionText Name.NoCollision = ""
        collisionText (Name.Collision i) = Text.pack (show i)
        collisionText Name.UnknownCollision = "?"
        (Name.TagText text textCol, tagCol) = Name.visible name

makeOptions ::
    ( MonadReader env m, HasTheme env, Applicative o
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, GuiState.HasCursor env
    ) =>
    m [Sugar.NameRef (Name g) o] ->
    SearchMenu.ResultsContext -> m (Menu.OptionList (Menu.Option m o))
makeOptions readGlobals (SearchMenu.ResultsContext searchTerm prefix)
    | Text.null searchTerm = pure mempty
    | otherwise =
        readGlobals <&> zip [(0::Int)..]
        <&> map withText
        <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
        <&> map (makeOption . snd)
        <&> (`Menu.OptionList` False)
    where
        withText (idx, nameRef) =
            (nameSearchTerm (nameRef ^. Sugar.nrName), (idx, nameRef))
        wrapOption optId mkRenedered =
            Menu.Option
            { Menu._oId = optId
            , Menu._oRender = mkRenedered
            , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
            }
        toPickResult = join Menu.PickResult
        toRenderedOption nameRef widget =
            Menu.RenderedOption
            { Menu._rWidget = widget
            , Menu._rPick =
                Widget.PreEvent
                { Widget._pDesc = "Goto"
                , Widget._pAction =
                    nameRef ^. Sugar.nrGotoDefinition
                    <&> WidgetIds.fromEntityId <&> toPickResult
                , Widget._pTextRemainder = ""
                }
            }
        makeOption (idx, nameRef) =
            GetVarEdit.makeSimpleView TextColors.definitionColor name optId
            <&> toRenderedOption nameRef
            & Reader.local (Element.animIdPrefix .~ Widget.toAnimId optId)
            & wrapOption optId
            where
                name = nameRef ^. Sugar.nrName
                optId = prefix `Widget.joinId` [BS8.pack (show idx)]

make ::
    ( MonadReader env m, Applicative o
    , HasTheme env, Element.HasAnimIdPrefix env, TextEdit.HasStyle env
    , Menu.HasConfig env, Hover.HasStyle env, GuiState.HasState env
    ) =>
    m [Sugar.NameRef (Name g) o] -> m (StatusBar.StatusWidget o)
make readGlobals =
    do
        selected <- GuiState.isSubCursor ?? myId
        color <- Lens.view (Theme.theme . Theme.hole . bgColorLens selected)
        let bgColor =
                Align.tValue %~
                Draw.backgroundColor (Widget.toAnimId myId <> ["bg"]) color
        let makeUnfocused =
                (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
                <*> TextView.makeLabel "  "
                <&> bgColor
        let makeSearchTermEdit _pick =
                SearchMenu.basicSearchTermEdit myId allowSearchTerm
                <&> Align.tValue . Lens.mapped %~ pure
                <&> bgColor
        menu <-
            if selected
            then do
                searchTermEventMap <-
                    SearchMenu.searchTermEditEventMap myId allowSearchTerm
                    <&> fmap pure
                -- TODO: DRY with other uses of search menu
                SearchMenu.make makeSearchTermEdit (makeOptions readGlobals)
                    Element.empty myId ?? Menu.Below
                    <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (<> searchTermEventMap)
            else makeUnfocused
        StatusBar.makeStatusWidget "Goto" menu
    where
        bgColorLens True = Theme.holeActiveSearchTermBGColor
        bgColorLens False = Theme.holeSearchTermBGColor
