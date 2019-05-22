{-# LANGUAGE TupleSections #-}
module Test.Lamdu.Sugar where

import           AST (annotations)
import           Control.DeepSeq (NFData, deepseq)
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (getP)
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified GUI.Momentu.Direction as Dir
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Term (Val)
import           Lamdu.Data.Anchors (Code(..))
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM, codeAnchors, runDbTransaction)
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Debug as Debug
import           Lamdu.Expr.IRef (DefI, ValP)
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.GUI.CodeEdit.Load (loadWorkArea)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.I18N.Code as Texts
import           Lamdu.I18N.LangId (LangId)
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Config (Config(..))
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types as Sugar
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)
import           Test.Lamdu.Env (EvalResults)

import           Test.Lamdu.Prelude

type T = Transaction

data IsHidden = NotHidden | Hidden deriving (Show)

allEntityIds ::
    WorkArea name i o (Sugar.Payload name i o ExprGui.Payload) ->
    [(EntityId, IsHidden)]
allEntityIds workArea =
    (pls ^.. Lens.folded . plData . ExprGui.plHiddenEntityIds . Lens.folded
     <&> (, Hidden))
    <> (pls ^.. Lens.folded . plEntityId <&> (, NotHidden))
    where
        pls = workArea ^.. traverse

validateHiddenEntityIds ::
    WorkArea name i o (Sugar.Payload name i o ExprGui.Payload) -> Either String ()
validateHiddenEntityIds workArea
    | Set.null hiddenAndExplicit = Right ()
    | otherwise =
        show hiddenAndExplicit ++ " are both hidden and explicit entityIds"
        & Left
    where
        pls = workArea ^.. traverse
        explicitEntityIds = pls ^.. Lens.folded . plEntityId & Set.fromList
        hiddenEntityIds =
            pls ^.. Lens.folded . plData . ExprGui.plHiddenEntityIds . Lens.folded
            & Set.fromList
        hiddenAndExplicit = Set.intersection explicitEntityIds hiddenEntityIds

data WorkAreaLowLevel = WorkAreaLowLevel
    { wallRepl :: Def.Expr (Val (ValP ViewM))
    , wallPanes :: [Def.Definition (Val (ValP ViewM)) (DefI ViewM)]
    }

workAreaLowLevelValProps :: WorkAreaLowLevel -> [ValP ViewM]
workAreaLowLevelValProps (WorkAreaLowLevel r p) =
    defExprs ^.. Lens.folded . Def.expr . annotations
    where
        defExprs = r : p ^.. Lens.folded . Def.defBody . Def._BodyExpr

workAreaLowLevelEntityIds :: WorkAreaLowLevel -> Set EntityId
workAreaLowLevelEntityIds wall =
    workAreaLowLevelValProps wall
    <&> EntityId.ofValI . Property.value
    & Set.fromList

workAreaLowLevelLoad :: T ViewM WorkAreaLowLevel
workAreaLowLevelLoad =
    WorkAreaLowLevel
    <$> ExprLoad.defExpr (repl codeAnchors)
    <*> (getP (panes codeAnchors)
            >>= traverse (ExprLoad.def . (^. Anchors._PaneDefinition)))

validate ::
    NFData name =>
    WorkArea name (T fa) (T fb)
    (Sugar.Payload name (T fa) (T fb) ExprGui.Payload) ->
    T ViewM
    (WorkArea name (T fa) (T fb)
        (Sugar.Payload name (T fa) (T fb) ExprGui.Payload))
validate workArea
    | Map.null duplicateEntityIds =
        do
            wallEntityIds <- workAreaLowLevelLoad <&> workAreaLowLevelEntityIds
            let missing = wallEntityIds `Set.difference` sugarEntityIdsSet
            unless (Set.null missing) $ fail $
                show missing ++ " do not appear in any sugar entity ids"
            deepseq workArea -- make sure no "error" clauses are hiding within
                (validateHiddenEntityIds workArea)
                & either fail (\() -> pure workArea)
    | otherwise =
        fail ("duplicate entityIds: " <> show duplicateEntityIds)
    where
        duplicateEntityIds =
            sugarEntityIds <&> _2 %~ (:[])
            & Map.fromListWith (++) & Map.filter (not . null . tail)
        sugarEntityIds = allEntityIds workArea
        sugarEntityIdsSet = Set.fromList (sugarEntityIds <&> fst)

convertWorkArea ::
    ( HasCallStack
    , Has LangId env
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has Dir.Layout env
    , Has Debug.Monitors env
    , Has EvalResults env
    , Has Config env
    , Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env ->
    T ViewM
    (WorkArea (Name (T ViewM)) (T ViewM) (T ViewM)
        (Sugar.Payload (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload))
convertWorkArea env = loadWorkArea env codeAnchors >>= validate

testProgram :: FilePath -> T ViewM a -> IO a
testProgram program action =
    withDB ("test/programs/" <> program)
    (runDbTransaction ?? runAction action)

sugarConfig :: Config
sugarConfig =
    Config
    { _showAllAnnotations = False
    }
