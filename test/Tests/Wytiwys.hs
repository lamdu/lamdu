{-# LANGUAGE TemplateHaskell #-}

module Tests.Wytiwys (test) where

import qualified Control.Lens as Lens
import           Control.Monad (foldM)
import           Control.Monad.Once (OnceT, evalOnceT)
import           GUI.Momentu.EventMap (Event(..))
import           GUI.Momentu.Main.Events (KeyEvent(..))
import           GUI.Momentu.ModKey
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Data.Db.Layout (ViewM, runDbTransaction)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Export.JS as ExportJS
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (globalId)
import           Lamdu.VersionControl (runAction)
import qualified Revision.Deltum.Transaction as Transaction
import           Test.Lamdu.Db (ramDB)
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.Exec (runJS)
import           Test.Lamdu.Gui
import           Test.Lamdu.Instances ()
import           Test.Lamdu.Sugar (convertWorkArea)
import qualified Test.Tasty as Tasty

import           Test.Lamdu.Prelude


data Step = Step
    { _sEvent :: Event
    , _sHaveAction :: Bool
    , _sContext :: String
    }

Lens.makeLenses ''Step

charKey :: Char -> Maybe Key
charKey ' ' = Just GLFW.Key'Space
charKey '\n' = Just GLFW.Key'Enter
charKey '\t' = Just GLFW.Key'Tab
charKey ',' = Just GLFW.Key'Comma
charKey '⌫' = Just GLFW.Key'Backspace
charKey '→' = Just GLFW.Key'Right
charKey '↑' = Just GLFW.Key'Up
charKey '↓' = Just GLFW.Key'Down
charKey '←' = Just GLFW.Key'Left
charKey '`' = Just GLFW.Key'GraveAccent
charKey _ = Nothing

charEvent :: Char -> Event
charEvent x = charKey x & maybe (EventChar x) (simpleKeyEvent . noMods)

parseSteps :: String -> [Step]
parseSteps xs =
    zip [0..] xs & go
    where
        go [] = []
        go ((_,'⇧'):rest) = go rest & Lens.ix 0 . sEvent %~ addMod shiftMods
        go ((_,'⌥'):rest) = go rest & Lens.ix 0 . sEvent %~ addMod altMods
        go ((_,x):(i,'✗'):rest) = Step (charEvent x) False (take (i+1) xs) : go rest
        go ((i,x):rest) = Step (charEvent x) True (take (i+1) xs) : go rest

addMod :: _ -> Event -> Event
addMod mods (EventKey k)
    | newMods == oldMods = error "modifiers enabled twice?"
    | otherwise = EventKey k { keModKeys = newMods }
    where
        oldMods = keModKeys k
        newMods = oldMods <> mods
addMod mods (EventChar c) = keyToChar c & noMods & simpleKeyEvent & addMod mods
addMod _ _ = error "addMod expected key event"

-- Used to translate shortcuts expressed as "⌥L", "⌘X", etc to key events
keyToChar :: Char -> Key
keyToChar 'L' = GLFW.Key'L
keyToChar _ = error "TODO"

applyActions :: HasCallStack => Env.Env -> String -> OnceT (T ViewM) Env.Env
applyActions startEnv =
    foldM go startEnv . parseSteps
    where
        go env step
            | step ^. sHaveAction =
                applyEventWith (step ^. sContext) dummyVirt (step ^. sEvent) env
            | otherwise =
                env <$ eventShouldDoNothing (step ^. sContext) dummyVirt (step ^. sEvent) env

wytiwysCompile :: HasCallStack => IO (Transaction.Store DbLayout.DbM) -> String -> IO String
wytiwysCompile mkDb src =
    do
        env <- Env.make
        db <- mkDb
        do
            repl <- DataOps.newEmptyPublicDefinitionWithPane DbLayout.codeAnchors & lift
            newEnv <- applyActions env src
            _ <- convertWorkArea src newEnv >>= makeFocusedWidget src newEnv
            globalId repl & ExportJS.compile & lift
            & evalOnceT
            & runAction
            & runDbTransaction db

wytiwysDb :: HasCallStack => IO (Transaction.Store DbLayout.DbM) -> String -> ByteString -> TestTree
wytiwysDb mkDb src result =
    wytiwysCompile mkDb src
    >>= runJS
    >>= assertEqual "Expected output" (result <> "\n")
    & testCase (show src)

test :: HasCallStack => TestTree
test =
    Tasty.withResource (ramDB ["data/freshdb.json"]) mempty $
    \mkDb ->
    let wytiwys = wytiwysDb (join mkDb)
        wytiwys_ src = wytiwysCompile (join mkDb) src & void & testCase (show src)
    in
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

    , wytiwys "1..10.sort lhs>rhs))@ 2" "7" -- Close parens get out of lambda

    , wytiwys "{a 7,b 5}.a\n" "7"
    , wytiwys "{a 7,b 5}.a+2" "9"

    , wytiwys "if ⌫1+2" "3" -- Backspace after "if " deletes it

    , wytiwys "7+negate\n→4" "3"
    , wytiwys "1==2⇧←⇧←if 3\t4" "4"

    , wytiwys "if 'a'=='b'\t1\t2" "2"

    , wytiwys "===↑↓⌫⌫⌫1" "1"

    , wytiwys "if 'a=='a\n←←id\t3\t4" "3"

    , wytiwys "toArr repli 3000\t0⇧←⇧←.len\n" "3000"

    , wytiwys "1+↑2✗↓2" "3" -- When cursor is at fragment's search term the should "2" do nothing.

    , wytiwys_ "'a 1←←\n"

    , wytiwys_ "{x["
    , wytiwys_ "{x⌥L"

    -- Opening record key does nothing when adding first field.
    -- It would had been slightly better if it opened it
    -- but prior bug of deleting the whole record was much worse.
    , wytiwys_ "{x`✗"
    ]
