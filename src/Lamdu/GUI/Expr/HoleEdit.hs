module Lamdu.GUI.Expr.HoleEdit
    ( make
    ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Hyper
import           GUI.Momentu (Responsive, noMods)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Config as Config
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.GUI.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.HoleOptEdit as HoleOptEdit
import           Lamdu.GUI.Expr.OptionEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    _ =>
    Annotated (ExprGui.Payload i o) # Const (Sugar.Hole Name i o) ->
    GuiM env i o (Responsive o)
make hole@(Ann (Const pl) _) =
    do
        searchTerm <- SearchMenu.readSearchTerm myId
        negativeNumberEventMap <-
            if searchTerm == "-"
            then ExprEventMap.makeLiteralNumberEventMap "-" ?? setToLiteral
            else pure mempty
        charEventMap <- ExprEventMap.makeLiteralCharEventMap searchTerm ?? setToLiteral
        env <- Lens.view id
        let innerHoleEventMap =
                -- Make space go to the hole inside a result
                E.keysEventMap [noMods ModKey.Key'Space]
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.nextEntry]) (pure ())
        let mkSearchTerm firstRes =
                SearchMenu.searchTermEdit myId (pure . ExprEventMap.allowedSearchTerm) firstRes <&>
                if searchTerm == ""
                then SearchMenu.termEditEventMap .~ mempty
                else
                    (SearchMenu.termWidget . Lens.mapped . Widget.eventMapMaker . Lens.mapped %~ blockChars) .
                    case firstRes of
                    Menu.PickFirstResult{} -> id
                    Menu.NoPickFirstResult ->
                        SearchMenu.termEditEventMap %~ blockChars
        (ExprEventMap.add options pl <&> (M.tValue %~))
            <*> ((maybeAddAnnotationPl pl <&> (M.tValue %~))
                <*> (SearchMenu.make mkSearchTerm (makeResults hole) M.empty myId ?? Menu.AnyPlace))
            & local (has . SearchMenu.emptyStrings . Lens.mapped .~ "_")
            <&> Responsive.fromWithTextPos
            <&> M.weakerEvents innerHoleEventMap
            <&> Widget.strongerEvents (negativeNumberEventMap <> charEventMap)
    where
        blockChars =
            E.emAllCharsHandler . traverse . E.chDocHandler . E.dhHandler . Lens.mapped
            %~ (<|> Just (pure mempty))
        setToLiteral = pl ^. Sugar.plActions . Sugar.setToLiteral
        myId = WidgetIds.fromExprPayload pl
        options =
            ExprEventMap.defaultOptions
            { ExprEventMap.addOperatorSetHoleState = Just (pl ^. Sugar.plEntityId)
            }

makeResults ::
    _ =>
    Annotated (ExprGui.Payload i o) # Const (Sugar.Hole Name i o) ->
    SearchMenu.ResultsContext ->
    GuiM env i o (Menu.OptionList (Menu.Option (GuiM env i o) o))
makeResults (Ann (Const pl) (Const hole)) ctx =
    do
        c <- Lens.view (Config.hasConfig . Config.completion . Config.completionResultCount)
        GuiM.im (hole ^. Sugar.holeOptions) <*>
            makeQuery (hole ^. Sugar.holeTagSuffixes) ctx
            >>= GuiM.im
            <&> take c
            <&> Lens.mapped %~
                makeResult HoleOptEdit.make ctx .
                -- Initialize the operator precedence of results.
                -- Without this results accept too many operators (and a test fails).
                (Sugar.optionExpr . annotation . Sugar.plParenInfo . Sugar.piMinOpPrec .~
                    pl ^. Sugar.plParenInfo . Sugar.piMinOpPrec)
            <&> Menu.OptionList isTruncated
    where
        -- TODO: Need to check whether we have more options
        isTruncated = False
