-- | A goto-definition widget
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.CodeEdit.GotoDefinition
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import           GUI.Momentu ((/|/))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.I18N.UnicodeAlts (unicodeAlts)
import           Lamdu.Name (Name)
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Global o = Global
    { _globalIdx :: !Int
    , _globalPrefix :: !Text
    , _globalColor :: !(Lens.ALens' TextColors M.Color)
    , _globalName :: !Name
    , _globalOpen :: !(o Sugar.EntityId)
    }
Lens.makeLenses ''Global

-- TODO: This is redundant to injectSymbol, hard-code it and remove from languages json?
getTagPrefix :: Text -> Maybe Char
getTagPrefix searchTerm = searchTerm ^? Lens.ix 0 . Lens.filtered (`elem` ['\'', '.'])

allowSearchTerm :: Text -> Bool
allowSearchTerm text =
    Text.drop prefixLength text & Name.isValidSearchText
    where
        prefixLength = length (getTagPrefix text)

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

nameToText :: _ => Name -> m [Text]
nameToText name =
    Name.visible name <&>
    \(Name.TagText text textCol, tagCol) ->
    unicodeAlts text
    <&> (<> (collisionText textCol <> collisionText tagCol))
    where
        collisionText Name.NoCollision = ""
        collisionText (Name.Collision i) = Text.pack (show i)
        collisionText Name.UnknownCollision = "?"

toGlobal ::
    Text -> Lens.ALens' TextColors M.Color -> (a -> o Sugar.EntityId) -> Sugar.NameRef Name a -> Int ->
    Global o
toGlobal prefix color goto nameRef idx = Global idx prefix color (nameRef ^. Sugar.nrName) (goto (nameRef ^. Sugar.nrId))

{-# ANN makeOptions ("HLint: ignore Redundant <$>"::String) #-}
makeOptions ::
    ( Monad i, Has (Texts.Navigation Text) env, Has (Texts.Name Text) env
    , M.HasCursor env, M.HasElemIdPrefix env, Has Theme env, Has TextView.Style env
    , Has Dir.Layout env
    , Applicative o
    ) =>
    (Sugar.GotoDest -> o Sugar.EntityId) -> Sugar.Globals Name i ->
    SearchMenu.ResultsContext -> ReaderT env i (Menu.OptionList (Menu.Option (ReaderT env i) o))
makeOptions goto globals (SearchMenu.ResultsContext searchTerm prefix)
    | Text.null searchTerm =
        pure Menu.OptionList { Menu._olIsTruncated = False, Menu._olOptions = [] }
    | otherwise =
        do
            env <- Lens.view id
            let toRenderedOption go widget =
                    Menu.RenderedOption
                    { Menu._rWidget = widget
                    , Menu._rPick =
                        Widget.PreEvent
                        { Widget._pDesc = env ^. has . Texts.goto
                        , Widget._pAction =
                            go <&> WidgetIds.fromEntityId <&> toPickResult
                        , Widget._pTextRemainder = ""
                        }
                    }
            let makeOption global =
                    Menu.Option
                    { Menu._oId = optId
                    , Menu._oRender =
                        (Element.subElemId "." >>= TextView.make (global ^. globalPrefix))
                        /|/
                        GetVarEdit.makeSimpleView (global ^. globalColor) name optId
                        <&> toRenderedOption (global ^. globalOpen)
                        & local (M.elemIdPrefix .~ M.asElemId optId)
                    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
                    }
                    where
                        name = global ^. globalName
                        idx = global ^. globalIdx
                        optId = prefix <> M.asElemId idx
            globs <-
                case mTagPrefix of
                Just tagPrefix ->
                    globals ^. Sugar.globalTags
                    <&> Lens.mapped %~
                        toGlobal (Text.singleton tagPrefix) TextColors.baseColor (goto . Sugar.GoToTag)
                Nothing ->
                    (<>)
                    <$> (globals ^. Sugar.globalDefs <&> Lens.mapped %~ toGlobal "" TextColors.definitionColor (goto . Sugar.GoToDef))
                    <*> (globals ^. Sugar.globalNominals <&> Lens.mapped %~ toGlobal "" TextColors.nomColor (goto . Sugar.GoToNom))
                & lift
            Lens.imap (&) globs &
                traverse withTexts
                <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
                <&> map (makeOption . snd)
                <&> Menu.OptionList isTruncated
    where
        mTagPrefix = getTagPrefix searchTerm
        isTruncated = False
        withTexts global =
            nameToText (global ^. globalName) <&>
            \texts -> (texts <&> maybe id Text.cons mTagPrefix, global)
        toPickResult x = Menu.PickResult x (Just x)

make :: _ => (Sugar.GotoDest -> o Sugar.EntityId) -> Sugar.Globals Name i -> ReaderT env i (StatusBar.StatusWidget o)
make gotoPane globals =
    do
        goto <- Lens.view (has . Texts.goto)
        let onTermStyle x =
                x
                & SearchMenu.emptyStrings . Lens.mapped .~ goto
                & SearchMenu.bgColors . Lens.mapped .~ M.Color 0 0 0 0
        SearchMenu.make (SearchMenu.searchTermEdit WidgetIds.gotoDefId (pure . allowSearchTerm))
            (makeOptions gotoPane globals) M.empty WidgetIds.gotoDefId Menu.Below
            -- Avoid pre-events for go-to-definition
            <&> M.tValue . Widget.wFocused . Widget.fPreEvents .~ []
            & local (has . Theme.searchTerm %~ onTermStyle)
            <&>
            \searchWidget ->
            StatusBar.StatusWidget
            { StatusBar._widget = searchWidget
            , StatusBar._globalEventMap = mempty
            }
