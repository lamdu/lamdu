module Tests.Wytiwys (test) where

import           Control.Monad (foldM)
import           Control.Monad.Once (OnceT, evalOnceT)
import           Data.Char (isAscii)
import           GUI.Momentu.EventMap (Event(..))
import           GUI.Momentu.ModKey (noMods, shift)
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Data.Db.Layout (ViewM, runDbTransaction)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Export.JS as ExportJS
import           Lamdu.Expr.IRef (globalId)
import           Lamdu.VersionControl (runAction)
import qualified Revision.Deltum.Transaction as Transaction
import           Test.Lamdu.Db (ramDB)
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.Exec (runJS)
import           Test.Lamdu.Gui
import           Test.Lamdu.Instances ()

import           Test.Lamdu.Prelude

charEvent :: Char -> Event
charEvent ' ' = noMods GLFW.Key'Space & simpleKeyEvent
charEvent '\n' = noMods GLFW.Key'Enter & simpleKeyEvent
charEvent '\t' = noMods GLFW.Key'Tab & simpleKeyEvent
charEvent ',' = noMods GLFW.Key'Comma & simpleKeyEvent
charEvent '⌫' = noMods GLFW.Key'Backspace & simpleKeyEvent
charEvent '→' = noMods GLFW.Key'Right & simpleKeyEvent
charEvent '↑' = noMods GLFW.Key'Up & simpleKeyEvent
charEvent '↓' = noMods GLFW.Key'Down & simpleKeyEvent
charEvent '←' = noMods GLFW.Key'Left & simpleKeyEvent
charEvent '«' = shift GLFW.Key'Left & simpleKeyEvent
charEvent x = EventChar x

applyActions :: HasCallStack => Env.Env -> String -> OnceT (T ViewM) Env.Env
applyActions env xs =
    zip [0..] xs
    & foldM (flip (\(i, x) -> applyEventWith (take (i+1) xs) dummyVirt (charEvent x))) env

wytiwysDb :: HasCallStack => IO (Transaction.Store DbLayout.DbM) -> String -> ByteString -> Test
wytiwysDb mkDb src result =
    do
        env <- Env.make
        db <- mkDb
        do
            repl <- DataOps.newEmptyPublicDefinitionWithPane DbLayout.codeAnchors & lift
            _ <- applyActions env src
            globalId repl & ExportJS.compile & lift
            & evalOnceT
            & runAction
            & runDbTransaction db
    >>= runJS
    >>= assertEqual "Expected output" (result <> "\n")
    & testCase (filter isAscii src)

test :: HasCallStack => Test
test =
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

            , wytiwys "1..10.sort lhs>rhs))@ 2" "7" -- Close parens get out of lambda

            , wytiwys "{a 7,b 5}.a\n" "7"
            , wytiwys "{a 7,b 5}.a+2" "9"

            , wytiwys "if ⌫1+2" "3" -- Backspace after "if " deletes it

            , wytiwys "7+negate\n→4" "3"
            , wytiwys "1==2««if 3\t4" "4"

            , wytiwys "if 'a'=='b'\t1\t2" "2"

            , wytiwys "===↑↓⌫⌫⌫1" "1"

            , wytiwys "if 'a=='a\n←←id\t3\t4" "3"
            ] & pure
        & buildTest
