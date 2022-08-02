module Test.Lamdu.Gui where

import           Control.Monad.Once (OnceT)
import           Control.Monad.Trans.FastWriter (runWriterT)
import qualified Control.Lens as Lens
import           Data.List (group, sort)
import qualified Data.Property as Property
import qualified Graphics.UI.GLFW as GLFW
import           GHC.Stack (prettyCallStack, callStack)
import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Main.Events (KeyEvent(..))
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (HasCursor(..), VirtualCursor(..))
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.Definition.Result (_DefRes)
import qualified Lamdu.GUI.Expr as ExpressionEdit
import qualified Lamdu.GUI.Expr.BinderEdit as BinderEdit
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Env (Env)
import           Test.Lamdu.Instances ()
import           Test.Lamdu.Sugar (convertWorkArea)

import           Test.Lamdu.Prelude

type T = Transaction
type SugarAnn = Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T ViewM))) Name
type WorkArea = Sugar.WorkArea SugarAnn Name (OnceT (T ViewM)) (T ViewM) (Sugar.Payload SugarAnn (T ViewM))

verifyLayers :: HasCallStack => String -> Element.LayeredImage -> Either String ()
verifyLayers msg view =
    case clashingIds of
    [] -> Right ()
    _ -> Left (prettyCallStack callStack <> "/" <> msg <> ": Clashing anim ids: " <> show clashingIds)
    where
        animIds = view ^.. Element.layers . traverse . Anim.frameImages . traverse . Anim.iAnimId
        clashingIds = sort animIds & group >>= tail

wideFocused :: Lens.Traversal' (Responsive f) (Widget.Surrounding -> Widget.Focused (f M.Update))
wideFocused = Responsive.rWide . Responsive.lWide . M.tValue . Widget.wState . Widget._StateFocused

makeGui ::
    HasCallStack =>
    String -> Env -> WorkArea -> OnceT (T ViewM) (Responsive (T ViewM))
makeGui afterDoc env workArea =
    do
        let defId =
                workArea ^?!
                Sugar.waPanes . traverse . Sugar.paneBody .
                Sugar._PaneDefinition . Sugar.drBody . Sugar._DefinitionBodyExpression .
                Sugar.deContent . annotation
                & WidgetIds.fromExprPayload
        let assocTagName = DataOps.assocTagName env
        gui <-
            Responsive.vbox <*>
            ( workArea ^..
                Sugar.waPanes . traverse
                & traverse CodeEdit.makePaneBodyEdit
                & GuiState.assignCursor WidgetIds.defaultCursor defId
            )
            & GuiM.run (workArea ^. Sugar.waOpenPane) assocTagName ExpressionEdit.make BinderEdit.make
                (Anchors.onGui (Property.mkProperty %~ lift) DbLayout.guiAnchors)
                env
            -- We ignore whether execute button was clicked
            <&> Widget.updates %~ fmap fst . runWriterT . (^. _DefRes)
        if Lens.has wideFocused gui
            then pure gui
            else error ("Red cursor after " ++ afterDoc ++ ": " ++ show (env ^. cursor))

focusedWidget ::
    HasCallStack =>
    String -> Responsive f -> Either String (Widget.Focused (f GuiState.Update))
focusedWidget msg gui =
    widget <$ verifyLayers msg (widget ^. Widget.fLayers)
    where
        widget = (gui ^?! wideFocused) (Widget.Surrounding 0 0 0 0)

makeFocusedWidget ::
    HasCallStack =>
    String -> Env -> WorkArea -> OnceT (T ViewM) (Widget.Focused (T ViewM GuiState.Update))
makeFocusedWidget msg env workArea =
    makeGui msg env workArea >>= either error pure . focusedWidget msg

mApplyEvent ::
    HasCallStack =>
    String -> Env -> VirtualCursor -> E.Event -> WorkArea -> OnceT (T ViewM) (Maybe GuiState.Update)
mApplyEvent msg env virtCursor event workArea =
    do
        w <- makeFocusedWidget msg env workArea
        let eventMap =
                (w ^. Widget.fEventMap)
                Widget.EventContext
                { Widget._eVirtualCursor = virtCursor
                , Widget._ePrevTextRemainder = ""
                }
        let r = E.lookup (Identity Nothing) event eventMap & runIdentity
        -- When trying to figure out which event is selected,
        -- this is a good place to "traceM (show (r ^? Lens._Just . E.dhDoc))"
        r ^? Lens._Just . E.dhHandler & sequenceA & lift

applyEventWith :: HasCallStack => String -> VirtualCursor -> E.Event -> Env -> OnceT (T ViewM) Env
applyEventWith msg virtCursor event env =
    do
        r <-
            convertWorkArea msg env
            >>= mApplyEvent msg env virtCursor event
            <&> fromMaybe (error msg)
            <&> (`GuiState.update` env)
        r `seq` pure r

dummyVirt :: VirtualCursor
dummyVirt = VirtualCursor (Rect 0 0)

simpleKeyEvent :: M.ModKey -> E.Event
simpleKeyEvent (M.ModKey mods key) =
    E.EventKey KeyEvent
    { keKey = key
    , keScanCode = 0 -- dummy
    , keModKeys = mods
    , keState = GLFW.KeyState'Pressed
    }
