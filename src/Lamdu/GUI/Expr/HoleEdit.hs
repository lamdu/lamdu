module Lamdu.GUI.Expr.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Hyper
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Config as Config
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.GUI.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
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
        negativeNumberEventMap <-
            SearchMenu.readSearchTerm myId >>=
            \case
            "-" -> ExprEventMap.makeLiteralNumberEventMap "-" ?? pl ^. Sugar.plActions . Sugar.setToLiteral
            _ -> pure mempty
        env <- Lens.view id
        let innerHoleEventMap =
                -- Make space go to the hole inside a result
                E.keysEventMap [MetaKey MetaKey.noMods MetaKey.Key'Space]
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.nextEntry]) (pure ())
        (ExprEventMap.add options pl <&> (M.tValue %~))
            <*> ((maybeAddAnnotationPl pl <&> (M.tValue %~)) 
                <*> (SearchMenu.make
                        (SearchMenu.searchTermEdit myId (pure . ExprEventMap.allowedSearchTerm))
                        (makeResults hole) M.empty myId
                        ?? Menu.AnyPlace))
            & local (has . SearchMenu.emptyStrings . Lens.mapped .~ "_")
            <&> Responsive.fromWithTextPos
            <&> M.weakerEvents innerHoleEventMap
            <&> Widget.strongerEvents negativeNumberEventMap
    where
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
makeResults (Ann (Const pl) (Const hole)) ctx
    | ctx ^. SearchMenu.rSearchTerm == "" = pure Menu.OptionList { Menu._olIsTruncated = False, Menu._olOptions = [] }
    | otherwise =
        do
            c <- Lens.view (has . Config.completion . Config.completionResultCount)
            GuiM.im (hole ^. Sugar.holeOptions) <*>
                makeQuery ctx
                >>= GuiM.im
                <&> take c
                <&> Lens.mapped %~
                    makeResult GuiM.makeBinder ctx .
                    -- Initialize the operator precedence of results.
                    -- Without this results accept too many operators (and a test fails).
                    (Sugar.optionExpr . annotation . Sugar.plParenInfo . Sugar.piMinOpPrec .~
                        pl ^. Sugar.plParenInfo . Sugar.piMinOpPrec)
                <&> Menu.OptionList isTruncated
    where
        -- TODO: Need to check whether we have more options
        isTruncated = False
