module TestNearestHoles where

import qualified Control.Lens.Extended as Lens
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles(..))
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar
import qualified Test.Lamdu.SugarStubs as Stub

import           Test.Lamdu.Prelude

test :: Test
test =
    assertEqual "expected" expected result
    & testCase "nearest-holes"
    where
        hole = Stub.hole
        expr =
            Stub.litNum 1 `Stub.plus` hole `Stub.plus` Stub.litNum 2
            & NearestHoles.add SugarLens.exprPayloads
            & SugarLens.exprPayloads %~ (^. Sugar.plData . _2)
        result = expr ^.. SugarLens.exprPayloads . Lens.filteredByIndex (SugarLens._OfExpr . Sugar._BodyLiteral)
        holeId = hole ^. Sugar._Node . Sugar.ann . Sugar.plEntityId
        expected =
            [ NearestHoles Nothing (Just holeId)
            , NearestHoles (Just holeId) Nothing
            ]
