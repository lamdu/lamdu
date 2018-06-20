{-# LANGUAGE ScopedTypeVariables #-}

module TestGui where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit(..))
import           Data.Functor.Identity (Identity(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (HasCursor(..), VirtualCursor(..))
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Event(..), KeyEvent(..))
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import           Lamdu.GUI.ExpressionEdit.BinderEdit (makeBinderBodyEdit)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import qualified Test.Lamdu.GuiEnv as GuiEnv
import           Test.Lamdu.Instances ()
import           Test.Lamdu.Sugar (convertWorkArea, testProgram)
import           Unsafe.Coerce (unsafeCoerce)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "gui-tests"
    [ testOpPrec
    , testFragmentSize
    ]

-- | Test for issue #410
-- https://trello.com/c/00mxkLRG/410-navigating-to-fragment-affects-layout
testFragmentSize :: Test
testFragmentSize =
    testCase "fragment-size" $
    GuiEnv.make >>=
    \baseEnv ->
    testProgram "simple-fragment.json" $
    \cache ->
    do
        workArea <- convertWorkArea cache
        let repl = workArea ^. Sugar.waRepl . Sugar.replExpr
        let makeWithEnv env =
                makeBinderBodyEdit repl
                & ExprGuiM.run ExpressionEdit.make DbLayout.guiAnchors env id
        guiCursorOnFrag <-
            baseEnv
            & cursor .~ WidgetIds.fromExprPayload (workArea ^?! Sugar.waRepl . Sugar.replExpr . Sugar.bbContent . Sugar._BinderExpr . Sugar.annotation)
            & makeWithEnv
        guiCursorElseWhere <- makeWithEnv baseEnv
        unless (guiCursorOnFrag ^. sz == guiCursorElseWhere ^. sz) (fail "fragment size inconsistent")
    where
        sz = Responsive.rWide . Align.tValue . Element.size

-- | Test for issue #375
-- https://trello.com/c/KFLJPNmO/375-operator-precedence-crosses-lambda-boundaries-add-test
testOpPrec :: Test
testOpPrec =
    testCase "apply-operator" $
    GuiEnv.make >>=
    \baseEnv ->
    testProgram "simple-lambda.json" $
    \cache ->
    do
        workArea <- convertWorkArea cache
        let holeId =
                workArea ^?! Sugar.waRepl . Sugar.replExpr .
                Sugar.bbContent . Sugar._BinderExpr .
                Sugar.body . Sugar._BodyLam . Sugar.lamFunc .
                Sugar.fBody . Sugar.bbContent . Sugar._BinderExpr .
                Sugar.annotation . Sugar.plEntityId
                & HoleWidgetIds.make
                & HoleWidgetIds.hidClosed
        let env = baseEnv & cursor .~ holeId
        gui <-
            workArea ^. Sugar.waRepl . Sugar.replExpr
            & makeBinderBodyEdit
            & ExprGuiM.run ExpressionEdit.make DbLayout.guiAnchors env id
        let mkFocused =
                gui ^?! Responsive.rWide . Align.tValue . Widget.wState . Widget._StateFocused
        let eventMap =
                (mkFocused (Widget.Surrounding 0 0 0 0) ^. Widget.fEventMap)
                Widget.EventContext
                { Widget._eVirtualCursor = VirtualCursor (Rect 0 0)
                , Widget._ePrevTextRemainder = ""
                }
        let eventKey =
                EventKey KeyEvent
                { keKey = GLFW.Key'7
                , keScanCode = 0 -- dummy
                , keModKeys = GLFW.ModifierKeys True False False False
                , keState = GLFW.KeyState'Pressed
                , keChar = Just '&'
                }
        _ <- runIdentity (E.lookup (Identity Nothing) eventKey eventMap) ^?! Lens._Just
        workArea' <- convertWorkArea cache
        unless (workAreaEq workArea workArea') (fail "bad operator precedence")

workAreaEq ::
    forall a m.
    Eq a =>
    Sugar.WorkArea (Name (T m)) (T m) (T m) a ->
    Sugar.WorkArea (Name (T m)) (T m) (T m) a ->
    Bool
workAreaEq x y =
    x' == unsafeCoerce y
    where
        x' = unsafeCoerce x :: Sugar.WorkArea (Name Unit) Unit Unit a
