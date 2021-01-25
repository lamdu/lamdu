{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Gui where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, _OnceT, evalOnceT)
import           Control.Monad.State (mapStateT)
import           Control.Monad.Unit (Unit(..))
import qualified Data.Map as Map
import qualified Data.Property as Property
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (Event(..))
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Main.Events (KeyEvent(..))
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (HasCursor(..), VirtualCursor(..))
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Export.JS as ExportJS
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import qualified Lamdu.GUI.Expr as ExpressionEdit
import qualified Lamdu.GUI.Expr.BinderEdit as BinderEdit
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           System.Directory (listDirectory)
import           Test.Lamdu.Code (readRepl)
import           Test.Lamdu.Env (Env)
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.Exec (runJS)
import           Test.Lamdu.Gui (verifyLayers)
import           Test.Lamdu.Instances ()
import           Test.Lamdu.Sugar (convertWorkArea, testProgram, testFresh)
import           Tests.Momentu (simpleKeyEvent)
import           Text.PrettyPrint (($+$))
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint.HughesPJClass (Pretty(..))
import           Unsafe.Coerce (unsafeCoerce)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "gui-tests"
    [ testOpPrec
    , testFragmentSize
    , testLambdaDelete
    , testNewTag
    , testPunCursor
    , testPrograms
    , testTagPanes
    , testWYTIWYS
    ]

replExpr ::
    Lens.Traversal' (Sugar.WorkArea v name i o a)
    (Sugar.Term v name i o # Annotated a)
replExpr = Sugar.waRepl . Sugar.replExpr . hVal . Sugar._BinderTerm

wideFocused :: Lens.Traversal' (Responsive f) (Widget.Surrounding -> Widget.Focused (f GuiState.Update))
wideFocused = Responsive.rWide . Align.tValue . Widget.wState . Widget._StateFocused

type SugarAnn = Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T ViewM))) Name

type WorkArea =
    Sugar.WorkArea SugarAnn
    Name (OnceT (T ViewM)) (T ViewM)
    (Sugar.Payload SugarAnn Name (OnceT (T ViewM)) (T ViewM), ExprGui.GuiPayload)

makeWorkArea :: Env -> OnceT (T ViewM) WorkArea
makeWorkArea env = convertWorkArea env <&> (fmap . fmap) (uncurry ExprGui.GuiPayload)

makeGui ::
    HasCallStack =>
    String -> Env -> WorkArea -> OnceT (T ViewM) (Responsive (T ViewM))
makeGui afterDoc env workArea =
    do
        let repl = workArea ^. Sugar.waRepl . Sugar.replExpr
        let replExprId = repl ^. SugarLens.binderResultExpr . _1 & WidgetIds.fromExprPayload
        let assocTagName = DataOps.assocTagName env
        gui <-
            do
                replGui <-
                    GuiM.makeBinder repl
                    & GuiState.assignCursor WidgetIds.replId replExprId
                paneGuis <-
                    workArea ^..
                    Sugar.waPanes . traverse
                    & traverse CodeEdit.makePaneBodyEdit
                Responsive.vbox ?? (replGui : paneGuis)
            & GuiM.run assocTagName ExpressionEdit.make BinderEdit.make
                (Anchors.onGui (Property.mkProperty %~ lift) DbLayout.guiAnchors)
                env evalOnceT
        if Lens.has wideFocused gui
            then pure gui
            else error ("Red cursor after " ++ afterDoc ++ ": " ++ show (env ^. cursor))

focusedWidget ::
    HasCallStack =>
    Responsive f -> Either String (Widget.Focused (f GuiState.Update))
focusedWidget gui =
    widget <$ verifyLayers (widget ^. Widget.fLayers)
    where
        widget = (gui ^?! wideFocused) (Widget.Surrounding 0 0 0 0)

makeFocusedWidget ::
    HasCallStack =>
    String -> Env -> WorkArea -> OnceT (T ViewM) (Widget.Focused (T ViewM GuiState.Update))
makeFocusedWidget afterDoc env workArea =
    makeGui afterDoc env workArea >>= either error pure . focusedWidget

mApplyEvent ::
    Env -> VirtualCursor -> Event -> WorkArea -> OnceT (T ViewM) (Maybe GuiState.Update)
mApplyEvent env virtCursor event workArea =
    do
        w <- makeFocusedWidget "mApplyEvent" env workArea
        let eventMap =
                (w ^. Widget.fEventMap)
                Widget.EventContext
                { Widget._eVirtualCursor = virtCursor
                , Widget._ePrevTextRemainder = ""
                }
        E.lookup (Identity Nothing) event eventMap
            & runIdentity
            <&> (^. E.dhHandler)
            & sequenceA & lift

applyEventWith :: String -> Env -> VirtualCursor -> Event -> OnceT (T ViewM) Env
applyEventWith msg env virtCursor event =
    makeWorkArea env
    >>= mApplyEvent env virtCursor event
    <&> fromMaybe (error msg)
    <&> (`GuiState.update` env)

applyEvent :: Env -> VirtualCursor -> Event -> OnceT (T ViewM) Env
applyEvent = applyEventWith "no event in applyEvent"

fromWorkArea ::
    Env ->
    Lens.ATraversal'
        ( Sugar.WorkArea (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
            (ExprGui.Payload (OnceT (T ViewM)) (T ViewM))
        ) a ->
    OnceT (T ViewM) a
fromWorkArea env path =
    convertWorkArea env <&> (fmap . fmap) (uncurry ExprGui.GuiPayload)
    <&> (^?! Lens.cloneTraversal path)

dummyVirt :: VirtualCursor
dummyVirt = VirtualCursor (Rect 0 0)

testTagPanes :: Test
testTagPanes =
    testCase "tag-panes" $
    Env.make >>=
    \baseEnv ->
    testProgram "ab.json" $
    do
        fromWorkArea baseEnv (replExpr . Sugar._BodyRecord . Sugar.cItems)
            >>= lift . sequence_ . (^.. traverse . Sugar.ciTag . Sugar.tagRefJumpTo . Lens._Just)
        () <$ (makeWorkArea baseEnv >>= makeFocusedWidget "opened tag panes" baseEnv)

-- | Test for issue #411
-- https://trello.com/c/IF6kY9AZ/411-deleting-lambda-parameter-red-cursor
testLambdaDelete :: Test
testLambdaDelete =
    testCase "delete-lambda" $
    Env.make >>=
    \baseEnv ->
    testProgram "simple-lambda.json" $
    do
        paramCursor <- topLevelLamParamCursor baseEnv
        let delEvent = MetaKey noMods GLFW.Key'Backspace & simpleKeyEvent
        env0 <- applyEvent (baseEnv & cursor .~ paramCursor) dummyVirt delEvent
        -- One delete replaces the param tag, next delete deletes param
        env1 <- applyEvent env0 dummyVirt delEvent
        _ <- makeWorkArea env1 >>= makeGui "" env1
        pure ()

topLevelLamParamCursor :: Env -> OnceT (T ViewM) WidgetId.Id
topLevelLamParamCursor env =
    fromWorkArea env
    (replExpr . Sugar._BodyLam . Sugar.lamFunc .
        Sugar.fParams . Sugar._Params . Lens.ix 0 . _2 .
        Sugar.piTag . Sugar.tagRefTag . Sugar.tagInstance)
    <&> WidgetIds.fromEntityId

-- | Test for regression in creating new tags when there are tags matching the search string.
-- (regression introduced at 2020.11.12 in 7bf691ce675f897)
testNewTag :: Test
testNewTag =
    testCase "new-tag" $
    Env.make >>=
    \baseEnv ->
    testProgram "simple-lambda.json" $
    do
        paramCursor <- topLevelLamParamCursor baseEnv
        env0 <- applyEvent (baseEnv & cursor .~ paramCursor) dummyVirt (EventChar 'f')
        let upEvent = MetaKey noMods GLFW.Key'Up & simpleKeyEvent
        env1 <- applyEvent env0 dummyVirt upEvent
        _ <- makeWorkArea env1 >>= makeGui "" env1
        pure ()

-- | Test for issue #410
-- https://trello.com/c/00mxkLRG/410-navigating-to-fragment-affects-layout
testFragmentSize :: Test
testFragmentSize =
    testCase "fragment-size" $
    Env.make >>=
    \baseEnv ->
    testProgram "simple-fragment.json" $
    do
        frag <-
            fromWorkArea baseEnv
            (Sugar.waRepl . Sugar.replExpr . annotation . _1)
        let env1 =
                baseEnv
                & cursor .~ WidgetIds.fromExprPayload frag
        guiCursorOnFrag <- makeWorkArea env1 >>= makeGui "" env1
        guiCursorElseWhere <- makeWorkArea baseEnv >>= makeGui "" baseEnv
        unless (guiCursorOnFrag ^. sz == guiCursorElseWhere ^. sz) (error "fragment size inconsistent")
    where
        sz = Responsive.rWide . Align.tValue . Element.size

-- | Test for issue #375
-- https://trello.com/c/KFLJPNmO/375-operator-precedence-crosses-lambda-boundaries-add-test
testOpPrec :: HasCallStack => Test
testOpPrec =
    testCase "apply-operator" $
    Env.make >>=
    \baseEnv ->
    testProgram "simple-lambda.json" $
    do
        holeId <-
            fromWorkArea baseEnv
            (replExpr . Sugar._BodyLam . Sugar.lamFunc .
             Sugar.fBody . annotation . _1 . Sugar.plEntityId)
            <&> WidgetIds.fromEntityId
        workArea <- convertWorkArea baseEnv
        _ <- applyEvent (baseEnv & cursor .~ holeId) dummyVirt (EventChar '&')
        workArea' <- convertWorkArea baseEnv
        unless (workAreaEq workArea workArea') (error "bad operator precedence")

-- | Test for
-- https://trello.com/c/uLlMpi5g/509-when-picking-record-tag-makes-the-field-pun-it-causes-red-cursor
testPunCursor :: Test
testPunCursor =
    testCase "pun-cursor" $
    Env.make >>=
    \baseEnv ->
    do
        tagId <-
            fromWorkArea baseEnv
            (Lens.cloneTraversal waRec . Sugar.cItems . traverse . Sugar.ciTag . Sugar.tagRefTag . Sugar.tagInstance)
        env0 <-
            applyEvent (baseEnv & cursor .~ WidgetIds.tagHoleId (WidgetIds.fromEntityId tagId))
            dummyVirt (EventChar 'x')
        env1 <- MetaKey noMods GLFW.Key'Enter & simpleKeyEvent & applyEvent env0 dummyVirt
        workArea <- makeWorkArea env1
        _ <- makeFocusedWidget "" env1 workArea
        workArea ^? Lens.cloneTraversal waRec . Sugar.cPunnedItems <&> length & pure
    & testProgram "rec-with-let.json"
    >>= assertEqual "Item should be punned" (Just 1)
    where
        waRec =
            Sugar.waRepl . Sugar.replExpr . hVal
            . Sugar._BinderLet . Sugar.lBody . hVal
            . Sugar._BinderTerm . Sugar._BodyRecord

workAreaEq ::
    forall a m v.
    Eq a =>
    Sugar.WorkArea v Name (OnceT (T m)) (T m) (Sugar.Payload v Name (OnceT (T m)) (T m), a) ->
    Sugar.WorkArea v Name (OnceT (T m)) (T m) (Sugar.Payload v Name (OnceT (T m)) (T m), a) ->
    Bool
workAreaEq x y =
    x' == unsafeCoerce y
    where
        x' =
            unsafeCoerce x :: Sugar.WorkArea () Name Unit Unit (Sugar.Payload () Name Unit Unit, a)

testKeyboardDirAndBack ::
    HasCallStack =>
    Env.Env -> VirtualCursor -> MetaKey -> MetaKey -> OnceT (T ViewM) ()
testKeyboardDirAndBack posEnv posVirt way back =
    do
        wa <- makeWorkArea posEnv
        mApplyEvent posEnv posVirt (simpleKeyEvent way) wa
            >>=
            \case
            Nothing -> pure ()
            Just updThere ->
                mApplyEvent
                (GuiState.update updThere posEnv)
                (updThere ^?! GuiState.uVirtualCursor . traverse)
                (simpleKeyEvent back) wa
                >>=
                \case
                Nothing -> error (show baseInfo <> " can't move back with cursor keys")
                Just updBack
                    | Lens.anyOf (GuiState.uCursor . traverse)
                    (`WidgetId.isSubId` (posEnv ^. cursor)) updBack & not ->
                        show baseInfo <> "moving back with cursor keys goes to different place: " <>
                        show (updBack ^. GuiState.uCursor)
                        & error
                Just{} -> pure ()
    where
        baseInfo =
            Pretty.text (show (posEnv ^. GuiState.cursor)) $+$
            pPrint way $+$ pPrint back <> ": "

data RectOrdering = Before | Undetermined | After
    deriving Eq

comparePositions :: Rect -> Rect -> RectOrdering
comparePositions r0 r1
    | r0 `Rect.rectWithin` r1 = error "TODO: After?"
    | r1 `Rect.rectWithin` r0 = error "TODO: Before?"
    | r0 ^. Rect.bottom < r1 ^. Rect.top  = Before
    | r1 ^. Rect.bottom < r0 ^. Rect.top  = After
    | r0 ^. Rect.right  < r1 ^. Rect.left = Before
    | r1 ^. Rect.right  < r0 ^. Rect.left = After
    | otherwise = Undetermined

testTabNavigation ::
    HasCallStack =>
    Env.Env -> VirtualCursor -> OnceT (T ViewM) ()
testTabNavigation env virtCursor =
    do
        w0 <- makeWorkArea env >>= makeFocusedWidget "mApplyEvent" env
        let eventMap =
                (w0 ^. Widget.fEventMap)
                Widget.EventContext
                { Widget._eVirtualCursor = virtCursor
                , Widget._ePrevTextRemainder = ""
                }
        let testDir (name, event, expected) =
                E.lookup (Identity Nothing) event eventMap
                    & runIdentity <&> (^. E.dhHandler) & sequenceA & lift
                >>=
                \case
                Nothing -> pure ()
                Just upd ->
                    do
                        let newEnv = GuiState.update upd env
                        w1 <-
                            makeWorkArea newEnv >>= makeFocusedWidget "testTabNavigation" newEnv
                        let p0 = w0 ^?! pos
                        let p1 = w1 ^?! pos
                        when (comparePositions p1 p0 /= expected) $
                            show (env ^. GuiState.cursor) <> ": " <> name <>
                            " did not move to expected direction"
                            & error
        traverse_ testDir dirs
    where
        pos = Widget.fFocalAreas . traverse
        dirs =
            [ ("tab", simpleKeyEvent (head Widget.strollAheadKeys), After)
            , ("shift-tab", simpleKeyEvent (head Widget.strollBackKeys), Before)
            ]

testConsistentKeyboardNavigation :: Env.Env -> VirtualCursor -> OnceT (T ViewM) ()
testConsistentKeyboardNavigation posEnv posVirt =
    do
        traverse_ (uncurry (testKeyboardDirAndBack posEnv posVirt))
            [ (k GLFW.Key'Up, k GLFW.Key'Down)
            , (k GLFW.Key'Down, k GLFW.Key'Up)
            , (k GLFW.Key'Left, k GLFW.Key'Right)
            , (k GLFW.Key'Right, k GLFW.Key'Left)
            ]
        testTabNavigation posEnv posVirt
    where
        k = MetaKey noMods

testActions ::
    HasCallStack =>
    Env.Env -> VirtualCursor -> OnceT (T ViewM) ()
testActions env virtCursor =
    do
        w <- makeWorkArea env >>= makeFocusedWidget "" env
        let eventMap =
                Widget.EventContext
                { Widget._eVirtualCursor = virtCursor
                , Widget._ePrevTextRemainder = ""
                } & w ^. Widget.fEventMap
        let events =
                mconcat
                [ eventMap ^@..
                    E.emKeyMap . traverse . docHandler <. E._Doesn'tWantClipboard
                , eventMap ^@..
                    E.emCharGroupHandlers . traverse . E.cgDocHandler . docHandler
                    <. Lens.taking 1 traverse
                , eventMap ^@..
                    E.emAllCharsHandler . traverse . E.chDocHandler . docHandler
                    >>= _2 exampleChars
                ]
        traverse_ testEvent events
    where
        exampleChars f = [f 'a', f '1', f '_', f '+'] ^.. traverse . Lens._Just
        testEvent (doc, event) =
            do
                newEnv <- event <&> (`GuiState.update` env) & lift
                makeWorkArea newEnv >>= makeGui (show doc <> " from " <> show (env ^. cursor)) newEnv
            & _OnceT %~ mapStateT (fmap fst . Transaction.fork) & void

docHandler ::
    (Lens.Indexable E.Doc p, Applicative f) =>
    p a (f a) -> E.DocHandler a ->
    f (E.DocHandler a)
docHandler = Lens.filteredBy E.dhDoc <. E.dhHandler

testActionsAndNavigation :: HasCallStack => Env -> VirtualCursor -> OnceT (T ViewM) ()
testActionsAndNavigation = testConsistentKeyboardNavigation <> testActions

testProgramGuiAtPos ::
    HasCallStack =>
    Env.Env -> Widget.EnterResult (T ViewM GuiState.Update) -> OnceT (T ViewM) ()
testProgramGuiAtPos baseEnv enter =
    do
        upd <- enter ^. Widget.enterResultEvent & lift
        testActionsAndNavigation (GuiState.update upd baseEnv)
            (VirtualCursor (enter ^. Widget.enterResultRect))

nubOn :: Ord k => (a -> k) -> [a] -> [a]
nubOn f xs = (xs <&> (\x -> (f x, x)) & Map.fromList) ^.. Lens.folded

programTest ::
    HasCallStack =>
    Env.Env -> FilePath -> IO ()
programTest baseEnv filename =
    testProgram filename $
    do
        baseGui <- makeWorkArea baseEnv >>= makeGui "" baseEnv
        let size = baseGui ^. Responsive.rWide . Align.tValue . Widget.wSize
        let narrowSize =
                (baseGui ^. Responsive.rNarrow) (Responsive.NarrowLayoutParams 0 False)
                ^. Align.tValue . Widget.wSize
        when (size ^. _1 < narrowSize ^. _1) (error "wide size is narrower than narrow!")
        w <- focusedWidget baseGui & either error pure
        case w ^. Widget.fMEnterPoint of
            Nothing ->
                VirtualCursor (w ^?! Widget.fFocalAreas . Lens.ix 0)
                & testActionsAndNavigation baseEnv
            Just enterPoint ->
                Vector2 <$> [0, 0.1 .. 1] <*> [0, 0.3 .. 1] <&> (* size)
                <&> enterPoint
                & nubOn (^. Widget.enterResultRect)
                & traverse_ (testProgramGuiAtPos baseEnv)

testPrograms :: Test
testPrograms =
    do
        baseEnv <- Env.make
        let testProg filename = programTest baseEnv filename & testCase filename
        listDirectory "test/programs"
            <&> filter (`notElem` skipped)
            <&> Lens.mapped %~ testProg
            <&> testGroup "program-tests"
        & buildTest
    where
        skipped =
            [ -- The tests import a program without first importing freshdb.
              -- This program, saved with an old codec (the first version),
              -- is not compatible with that
              "old-codec-factorial.json"
            ]

charEvent :: Char -> Event
charEvent ' ' = EventKey (KeyEvent GLFW.Key'Space 0 GLFW.KeyState'Pressed mempty)
charEvent x = EventChar x

applyActions :: String -> Env.Env -> OnceT (T ViewM) Env.Env
applyActions [] env = pure env
applyActions (x:xs) env =
    applyEventWith ("No char " <> show x) env dummyVirt (charEvent x)
    >>= applyActions xs

wytiwys :: String -> ByteString -> Test
wytiwys src result =
    Env.make
    >>= testFresh . (>> lift (readRepl >>= ExportJS.compile)) . applyActions src
    >>= runJS
    >>= assertEqual "Expected output" (result <> "\n")
    & testCase src

testWYTIWYS :: Test
testWYTIWYS =
    testGroup "WYTIWYS"
    [ wytiwys "1+1" "2"

    , wytiwys "2*(3+4)" "14"
    , wytiwys "2*(3+4" "14" -- Don't have to close paren

    , wytiwys "sum(1..10)" "45"
    , wytiwys "sum 1..10" "45" -- An Ergonomic WYTIWIS violation: types cause fragment
    , wytiwys "sum 1..10.map n*2" "90"
    , wytiwys "sum 1..10.map 2*(num+1)" "108"
    , wytiwys "sum 1..10.map 2*(num+1" "108"
    ]
