module Tests.Gui (test) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, _OnceT)
import           Control.Monad.State (mapStateT)
import           Control.Monad.Unit (Unit(..))
import           Data.Containers.ListUtils (nubOrdOn)
import qualified Data.Property as Property
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (Event(..))
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..), noMods)
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (HasCursor(..), VirtualCursor(..))
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import qualified Revision.Deltum.Transaction as Transaction
import           System.Directory (listDirectory)
import qualified System.Info as SysInfo
import           Test.Lamdu.Env (Env)
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.Gui
import           Test.Lamdu.Instances ()
import           Test.Lamdu.Sugar (convertWorkArea, testProgram)
import           Text.PrettyPrint (($+$))
import qualified Text.PrettyPrint as Pretty
import           Unsafe.Coerce (unsafeCoerce)

import           Test.Lamdu.Prelude

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
    , testPunnedRecordAddField
    , testRecordPunAndAdd
    , testChooseTagAndAddNext
    , testTwoDeletedDefs
    ]

defExprs :: Lens.Traversal' (Sugar.WorkArea v name i o a) (Annotated a # Sugar.Assignment v name i o)
defExprs =
    Sugar.waPanes . traverse . Sugar.paneBody . Sugar._PaneDefinition .
    Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent

defExprPlainBodies :: Lens.Traversal' (Sugar.WorkArea v name i o a) (Sugar.BinderBody v name i o # Annotated a)
defExprPlainBodies = defExprs . hVal . Sugar._BodyPlain . Sugar.apBody . Sugar.bBody

repl :: Lens.Traversal' (Sugar.WorkArea v name i o a) (Annotated a # Sugar.Term v name i o)
repl =
    defExprPlainBodies . Sugar._BinderTerm .
    -- When migrating to multirepl, repl exprs were placed in fragments
    -- (to avoid need to infer their type for the simple migration)
    Sugar._BodyFragment . Sugar.fExpr

replExpr :: Lens.Traversal' (Sugar.WorkArea v name i o a) (Sugar.Term v name i o # Annotated a)
replExpr = repl . hVal

convertAndMakeGui :: HasCallStack => String -> Env -> OnceT (T ViewM) (Responsive (T ViewM))
convertAndMakeGui afterDoc env = convertWorkArea afterDoc env >>= makeGui afterDoc env

applyEvent :: HasCallStack => VirtualCursor -> Event -> Env -> OnceT (T ViewM) Env
applyEvent v e = applyEventWith ("applyEvent: " <> show e) v e

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

testTwoDeletedDefs :: Test
testTwoDeletedDefs =
    testCase "two-deleted-defs" $
    Env.make >>=
    \baseEnv ->
    do
        convertWorkArea "" baseEnv
            <&> (^.. Sugar.waPanes . traverse . Sugar.paneDefinitionState . Property.pSet)
            >>= traverse_ (lift . (Sugar.DeletedDefinition &))
        convertWorkArea "" baseEnv >>= makeFocusedWidget "" baseEnv & void
    & testProgram "two-repls.json"

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
        Sugar.fParams . Sugar._LhsVar .
        Sugar.vTag . Sugar.oTag . Sugar.tagRefTag . Sugar.tagInstance)
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
        frag <- fromWorkArea baseEnv (repl . annotation)
        let env0 =
                baseEnv
                & cursor .~ WidgetIds.fromExprPayload frag
        guiCursorOnFrag <- convertAndMakeGui "" env0
        guiCursorElseWhere <- convertAndMakeGui "" baseEnv
        unless (guiCursorOnFrag ^. sz == guiCursorElseWhere ^. sz) (error "fragment size inconsistent")
    where
        sz = Responsive.rWide . Responsive.lWide . Align.tValue . Element.size

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

letBody :: Lens.Traversal' (Sugar.Binder v n i o # h) (h # Sugar.Binder v n i o)
letBody = Sugar.bBody . Sugar._BinderLet . Sugar.lBody

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
            ( defExprs . hVal . Sugar._BodyPlain . Sugar.apBody . letBody . hVal . letBody
            . binderRec . Sugar.cPunnedItems . traverse . Sugar.pvVar . annotation . Sugar.plEntityId
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
            ( defExprs . hVal . Sugar._BodyPlain . Sugar.apBody . letBody . binderRec
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
            ( defExprs . hVal . Sugar._BodyPlain . Sugar.apBody . Sugar.bBody . Sugar._BinderTerm . Sugar._BodyRecord
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
        waRec = defExprPlainBodies . Sugar._BinderLet . Sugar.lBody . binderRec

workAreaEq ::
    Sugar.WorkArea v Name (OnceT (T m)) (T m) (Sugar.Payload v (T m)) ->
    Sugar.WorkArea v Name (OnceT (T m)) (T m) (Sugar.Payload v (T m)) ->
    Bool
workAreaEq x y =
    x' == unsafeCoerce y
    where
        x' = unsafeCoerce x :: Sugar.WorkArea () Name Unit Unit (Sugar.Payload () Unit)

testKeyboardDirAndBack ::
    HasCallStack =>
    Env.Env -> VirtualCursor -> ModKey -> ModKey -> OnceT (T ViewM) ()
testKeyboardDirAndBack posEnv posVirt way back =
    do
        wa <- convertWorkArea baseInfo posEnv
        mApplyEvent "" posEnv posVirt (simpleKeyEvent way) wa
            >>=
            \case
            Nothing -> pure ()
            Just updThere ->
                mApplyEvent ""
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

programTest ::
    HasCallStack =>
    Env.Env -> FilePath -> IO ()
programTest baseEnv filename =
    testProgram filename $
    do
        baseGui <- convertAndMakeGui "" baseEnv
        let size = baseGui ^. Responsive.rWide . Responsive.lWide . Align.tValue . Widget.wSize
        let narrowSize =
                (baseGui ^. Responsive.rNarrow) (Responsive.NarrowLayoutParams 0 False)
                ^. Align.tValue . Widget.wSize
        when (size ^. _1 < narrowSize ^. _1) (error "wide size is narrower than narrow!")
        w <- focusedWidget "" baseGui & either error pure
        case w ^. Widget.fMEnterPoint of
            Nothing ->
                VirtualCursor (w ^?! Widget.fFocalAreas . Lens.ix 0)
                & testActionsAndNavigation baseEnv
            Just enterPoint ->
                Vector2 <$> [0, 0.1 .. 1] <*> [0, 0.3 .. 1] <&> (* size)
                <&> enterPoint
                & nubOrdOn (^. Widget.enterResultRect)
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
