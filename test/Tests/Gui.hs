{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Gui where

import qualified Control.Lens as Lens
import           Control.Monad (foldM)
import           Control.Monad.Once (OnceT, _OnceT, evalOnceT)
import           Control.Monad.State (mapStateT)
import           Control.Monad.Unit (Unit(..))
import           Data.Char (isAscii)
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (Event(..))
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Main.Events (KeyEvent(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..), noMods, shift)
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
import           Lamdu.Data.Db.Layout (ViewM, runDbTransaction)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Export.JS as ExportJS
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import qualified Lamdu.GUI.Expr as ExpressionEdit
import qualified Lamdu.GUI.Expr.BinderEdit as BinderEdit
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           System.Directory (listDirectory)
import qualified System.Info as SysInfo
import           Test.Lamdu.Code (readRepl)
import           Test.Lamdu.Db (ramDB)
import           Test.Lamdu.Env (Env)
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.Exec (runJS)
import           Test.Lamdu.Gui (verifyLayers)
import           Test.Lamdu.Instances ()
import           Test.Lamdu.Sugar (convertWorkArea, testProgram)
import           Text.PrettyPrint (($+$))
import qualified Text.PrettyPrint as Pretty
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
    , testPunnedRecordAddField
    , testRecordPunAndAdd
    , testChooseTagAndAddNext
    ]

replExpr ::
    Lens.Traversal' (Sugar.WorkArea v name i o a)
    (Sugar.Term v name i o # Annotated a)
replExpr = Sugar.waRepl . Sugar.replExpr . hVal . Sugar.bBody . Sugar._BinderTerm

wideFocused :: Lens.Traversal' (Responsive f) (Widget.Surrounding -> Widget.Focused (f GuiState.Update))
wideFocused = Responsive.rWide . Align.tValue . Widget.wState . Widget._StateFocused

type SugarAnn = Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T ViewM))) Name

type WorkArea = Sugar.WorkArea SugarAnn Name (OnceT (T ViewM)) (T ViewM) (Sugar.Payload SugarAnn (T ViewM))

makeGui ::
    HasCallStack =>
    String -> Env -> WorkArea -> OnceT (T ViewM) (Responsive (T ViewM))
makeGui afterDoc env workArea =
    do
        let repl = workArea ^. Sugar.waRepl . Sugar.replExpr
        let replExprId = repl ^. SugarLens.binderResultExpr & WidgetIds.fromExprPayload
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
                env
        if Lens.has wideFocused gui
            then pure gui
            else error ("Red cursor after " ++ afterDoc ++ ": " ++ show (env ^. cursor))

convertAndMakeGui :: HasCallStack => String -> Env -> OnceT (T ViewM) (Responsive (T ViewM))
convertAndMakeGui afterDoc env = convertWorkArea afterDoc env >>= makeGui afterDoc env

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
        let r = E.lookup (Identity Nothing) event eventMap & runIdentity
        -- When trying to figure out which event is selected,
        -- this is a good place to "traceM (show (r ^? Lens._Just . E.dhDoc))"
        r ^? Lens._Just . E.dhHandler & sequenceA & lift

applyEventWith :: HasCallStack => String -> VirtualCursor -> Event -> Env -> OnceT (T ViewM) Env
applyEventWith msg virtCursor event env =
    do
        r <-
            convertWorkArea msg env
            >>= mApplyEvent env virtCursor event
            <&> fromMaybe (error msg)
            <&> (`GuiState.update` env)
        r `seq` pure r

applyEvent :: HasCallStack => VirtualCursor -> Event -> Env -> OnceT (T ViewM) Env
applyEvent v e = applyEventWith ("no event in applyEvent: " <> show e) v e

fromWorkArea ::
    Env ->
    Lens.ATraversal'
        ( Sugar.WorkArea (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
            (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name (OnceT (T ViewM))) Name) (T ViewM))
        ) a ->
    OnceT (T ViewM) a
fromWorkArea env path =
    convertWorkArea "" env
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
        fromWorkArea baseEnv (replExpr . Sugar._BodyRecord . Sugar.cList . Sugar.tlItems)
            >>= lift . sequence_ .
                (^.. Lens._Just . Sugar.tlHead . Sugar.tiTag . Sugar.tagRefJumpTo . Lens._Just)
        convertWorkArea "" baseEnv >>= makeFocusedWidget "opened tag panes" baseEnv & void

simpleKeyEvent :: ModKey -> E.Event
simpleKeyEvent (ModKey mods key) =
    EventKey KeyEvent
    { keKey = key
    , keScanCode = 0 -- dummy
    , keModKeys = mods
    , keState = GLFW.KeyState'Pressed
    }

-- | Test for issue #411
-- https://trello.com/c/IF6kY9AZ/411-deleting-lambda-parameter-red-cursor
testLambdaDelete :: HasCallStack => Test
testLambdaDelete =
    testCase "delete-lambda" $
    Env.make >>=
    \baseEnv ->
    testProgram "simple-lambda.json" $
    do
        paramCursor <- topLevelLamParamCursor baseEnv
        let delEvent = noMods GLFW.Key'Backspace & simpleKeyEvent
        baseEnv & cursor .~ paramCursor & applyEvent dummyVirt delEvent
            -- One delete replaces the param tag, next delete deletes param
            >>= applyEvent dummyVirt delEvent
            >>= convertAndMakeGui "" & void

-- | Test for regression in creating new tags when there are tags matching the search string.
-- (regression introduced at 2020.11.12 in 7bf691ce675f897)
testNewTag :: HasCallStack => Test
testNewTag =
    testCase "new-tag" $
    Env.make >>=
    \baseEnv ->
    testProgram "simple-lambda.json" $
    do
        paramCursor <- topLevelLamParamCursor baseEnv
        baseEnv & cursor .~ paramCursor & applyEvent dummyVirt (EventChar 'f')
            >>= applyEvent dummyVirt (simpleKeyEvent (noMods GLFW.Key'Up))
            >>= convertAndMakeGui "" & void

topLevelLamParamCursor :: Env -> OnceT (T ViewM) WidgetId.Id
topLevelLamParamCursor env =
    fromWorkArea env
    (replExpr . Sugar._BodyLam . Sugar.lamFunc .
        Sugar.fParams . Sugar._VarParam . _2 .
        Sugar.vpiTag . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance)
    <&> WidgetIds.fromEntityId

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
            (Sugar.waRepl . Sugar.replExpr . annotation)
        let env0 =
                baseEnv
                & cursor .~ WidgetIds.fromExprPayload frag
        guiCursorOnFrag <- convertAndMakeGui "" env0
        guiCursorElseWhere <- convertAndMakeGui "" baseEnv
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
             Sugar.fBody . annotation . Sugar.plEntityId)
            <&> WidgetIds.fromEntityId
        workArea <- convertWorkArea "" baseEnv
        _ <- baseEnv & cursor .~ holeId & applyEvent dummyVirt (EventChar '&')
        workArea' <- convertWorkArea "" baseEnv
        unless (workAreaEq workArea workArea') (error "bad operator precedence")

letBody :: Lens.Traversal' (Ann a # Sugar.Binder v n i o) (Ann a # Sugar.Binder v n i o)
letBody = hVal . Sugar.bBody . Sugar._BinderLet . Sugar.lBody

binderRec :: Lens.Traversal' (Ann a # Sugar.Binder v n i o) (Sugar.Composite v n i o # Ann a)
binderRec = hVal . Sugar.bBody . Sugar._BinderTerm . Sugar._BodyRecord

testPunnedRecordAddField :: HasCallStack => Test
testPunnedRecordAddField =
    testCase "punned-record-add-field" $
    Env.make >>=
    \baseEnv ->
    do
        punnedId <-
            fromWorkArea baseEnv
            ( Sugar.waRepl . Sugar.replExpr . letBody . letBody . binderRec
            . Sugar.cPunnedItems . traverse . Sugar.pvVar
            . annotation . Sugar.plEntityId
            )
        baseEnv & cursor .~ WidgetIds.tagHoleId (WidgetIds.fromEntityId punnedId)
            & applyEvent dummyVirt (simpleKeyEvent (noMods GLFW.Key'Comma))
            & void
    & testProgram "punned-fields.json"

testRecordPunAndAdd :: HasCallStack => Test
testRecordPunAndAdd =
    testCase "record-pun-and-add" $
    Env.make >>=
    \baseEnv ->
    do
        holeId <-
            fromWorkArea baseEnv
            ( Sugar.waRepl . Sugar.replExpr . letBody . binderRec
            . Sugar.cList . SugarLens.taggedListItems . Sugar.tiValue
            . annotation . Sugar.plEntityId
            )
        baseEnv & cursor .~ WidgetIds.fromEntityId holeId
            & applyEvent dummyVirt (EventChar 'x')
            >>= applyEvent dummyVirt (simpleKeyEvent (noMods GLFW.Key'Comma))
            >>= convertAndMakeGui ""
            & void
    & testProgram "before-punned-field.json"

testChooseTagAndAddNext :: HasCallStack => Test
testChooseTagAndAddNext =
    testCase "choose-tag-and-add-next" $
    Env.make >>=
    \baseEnv ->
    do
        tagId <-
            fromWorkArea baseEnv
            ( Sugar.waRepl . Sugar.replExpr . binderRec
            . Sugar.cList . SugarLens.taggedListItems . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagInstance
            )
        baseEnv & cursor .~ WidgetIds.tagHoleId (WidgetIds.fromEntityId tagId)
            & applyEvent dummyVirt (EventChar 'x')
            >>= applyEvent dummyVirt (simpleKeyEvent (noMods GLFW.Key'Comma))
            >>= convertAndMakeGui "" & void
    & testProgram "record.json"

-- | Test for
-- https://trello.com/c/uLlMpi5g/509-when-picking-record-tag-makes-the-field-pun-it-causes-red-cursor
testPunCursor :: HasCallStack => Test
testPunCursor =
    testCase "pun-cursor" $
    Env.make >>=
    \baseEnv ->
    do
        tagId <-
            fromWorkArea baseEnv
            ( Lens.cloneTraversal waRec . Sugar.cList . Sugar.tlItems
            . Lens._Just . Sugar.tlHead . Sugar.tiTag . Sugar.tagRefTag . Sugar.tagInstance
            )
        env0 <-
            baseEnv & cursor .~ WidgetIds.tagHoleId (WidgetIds.fromEntityId tagId)
            & applyEvent dummyVirt (EventChar 'x')
            >>= applyEvent dummyVirt (simpleKeyEvent (noMods GLFW.Key'Enter))
        workArea <- convertWorkArea "" env0
        _ <- makeFocusedWidget "" env0 workArea
        workArea ^? Lens.cloneTraversal waRec . Sugar.cPunnedItems <&> length & pure
    & testProgram "rec-with-let.json"
    >>= assertEqual "Item should be punned" (Just 1)
    where
        waRec = Sugar.waRepl . Sugar.replExpr . letBody . binderRec

workAreaEq ::
    forall m v.
    Sugar.WorkArea v Name (OnceT (T m)) (T m) (Sugar.Payload v (T m)) ->
    Sugar.WorkArea v Name (OnceT (T m)) (T m) (Sugar.Payload v (T m)) ->
    Bool
workAreaEq x y =
    x' == unsafeCoerce y
    where
        x' =
            unsafeCoerce x :: Sugar.WorkArea () Name Unit Unit (Sugar.Payload () Unit)

testKeyboardDirAndBack ::
    HasCallStack =>
    Env.Env -> VirtualCursor -> ModKey -> ModKey -> OnceT (T ViewM) ()
testKeyboardDirAndBack posEnv posVirt way back =
    do
        wa <- convertWorkArea baseInfo posEnv
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
                Nothing -> error (baseInfo <> " can't move back with cursor keys")
                Just updBack
                    | Lens.anyOf (GuiState.uCursor . traverse)
                    (`WidgetId.isSubId` (posEnv ^. cursor)) updBack & not ->
                        baseInfo <> "moving back with cursor keys goes to different place: " <>
                        show (updBack ^. GuiState.uCursor)
                        & error
                Just{} -> pure ()
    where
        pPrintModKey = Pretty.text . Text.unpack . MetaKey.format SysInfo.os
        baseInfo =
            Pretty.text (show (posEnv ^. GuiState.cursor)) $+$
            pPrintModKey way $+$ pPrintModKey back <> ": "
            & show

data RectOrdering = Before | Undetermined | After
    deriving Eq

comparePositions :: Rect -> Rect -> RectOrdering
comparePositions r0 r1
    | r0 `Rect.rectWithin` r1 = After
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
        w0 <- convertWorkArea "" env >>= makeFocusedWidget "mApplyEvent" env
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
                            convertWorkArea info newEnv
                            >>= makeFocusedWidget "testTabNavigation" newEnv
                        let p0 = w0 ^?! pos
                        let p1 = w1 ^?! pos
                        when (comparePositions p1 p0 /= expected) $
                            error $ info <> ": did not move to expected direction"
                where
                    info = show (env ^. GuiState.cursor) <> " " <> name
        traverse_ testDir dirs
    where
        pos = Widget.fFocalAreas . traverse
        dirs =
            -- TODO: Support tab changing the state,
            -- which means we need to maintain the cursor
            [ ("shift-tab", simpleKeyEvent (head Widget.strollBackKeys), Before)
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
        k = noMods

testActions ::
    HasCallStack =>
    Env.Env -> VirtualCursor -> OnceT (T ViewM) ()
testActions env virtCursor =
    do
        w <- convertWorkArea "" env >>= makeFocusedWidget "" env
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
            event <&> (`GuiState.update` env) & lift
            >>= convertAndMakeGui (show doc <> " from " <> show (env ^. cursor))
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
        baseGui <- convertAndMakeGui "" baseEnv
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
            , "builtins.json"
            ]

charEvent :: Char -> Event
charEvent ' ' = noMods GLFW.Key'Space & simpleKeyEvent
charEvent '\n' = noMods GLFW.Key'Enter & simpleKeyEvent
charEvent '\t' = noMods GLFW.Key'Tab & simpleKeyEvent
charEvent ',' = noMods GLFW.Key'Comma & simpleKeyEvent
charEvent '⌫' = noMods GLFW.Key'Backspace & simpleKeyEvent
charEvent '→' = noMods GLFW.Key'Right & simpleKeyEvent
charEvent '⇐' = shift GLFW.Key'Left & simpleKeyEvent
charEvent x = EventChar x

applyActions :: HasCallStack => Env.Env -> String -> OnceT (T ViewM) Env.Env
applyActions =
    flip (\x -> applyEventWith ("No char " <> show x) dummyVirt (charEvent x))
    & foldM

wytiwysDb :: HasCallStack => IO (Transaction.Store DbLayout.DbM) -> String -> ByteString -> Test
wytiwysDb mkDb src result =
    do
        env <- Env.make
        db <- mkDb
        do
            _ <- applyActions env src
            lift (readRepl >>= ExportJS.compile)
            & evalOnceT
            & runAction
            & runDbTransaction db
    >>= runJS
    >>= assertEqual "Expected output" (result <> "\n")
    & testCase (filter isAscii src)

testWYTIWYS :: HasCallStack => Test
testWYTIWYS =
    do
        mkDb <- ramDB ["data/freshdb.json"]
        let wytiwys = wytiwysDb mkDb
        testGroup "WYTIWYS"
            [ wytiwys "1+1" "2"

            , wytiwys "2*3+4" "10"
            , wytiwys "2*(3+4)" "14"
            , wytiwys "2*(3+4" "14" -- Don't have to close paren

            , wytiwys "sum (1..10)" "45" -- Debatable issue: Space is necessary here!
            , wytiwys "sum 1..10" "45" -- An Ergonomic WYTIWIS violation: types cause fragment
            , wytiwys "sum 1..10.map n*2" "90"
            , wytiwys "sum 1..10.map 2*num\n" "90" -- TODO: Would be better without requiring the enter at the end
            , wytiwys "sum 1..10.map 2*(num+1)" "108"
            , wytiwys "sum 1..10.map 2*(num+1" "108"

            , wytiwys "if 1=2:3\t4" "4" -- Type if-expressions without "else:"

            , wytiwys "sum 1..10.filter nu>5" "30"
            , wytiwys "sum 1..10.filter n>5" "30"
            , wytiwys "sum 1..10.filter 12<(num+1)*12" "45"

            , wytiwys "if {={:1\t2" "1" -- "{" expands to "{}"
            , wytiwys "let {val 1\trec.val\n" "1" -- "let " jumps straight to value of let

            , wytiwys "1..10.sort lhs>rhs)).item 2" "7" -- Close parens get out of lambda

            , wytiwys "{a 7,b 5}.a\n" "7"
            , wytiwys "{a 7,b 5}.a+2" "9"

            , wytiwys "if ⌫1+2" "3" -- Backspace after "if " deletes it

            , wytiwys "7+negate\n→4" "3"
            , wytiwys "1==2⇐⇐if 3\t4" "4"

            , wytiwys "if 'a'=='b'\t1\t2" "2"
            ] & pure
        & buildTest
