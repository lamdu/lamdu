{-# LANGUAGE TemplateHaskell, TupleSections, TypeOperators #-}
module Test.Lamdu.Sugar where

import           Control.DeepSeq (NFData, deepseq)
import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, _OnceT)
import           Control.Monad.State (evalStateT)
import           Control.Monad.Transaction (getP)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GUI.Momentu.Direction as Dir
import           Hyper
import           Hyper.Type.Functor
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (Code(..))
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM, codeAnchors, runDbTransaction)
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Debug as Debug
import           Lamdu.Expr.IRef (DefI, HRef, iref)
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.I18N.Code as Texts
import           Lamdu.I18N.LangId (LangId)
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import           Lamdu.Sugar (sugarWorkArea)
import           Lamdu.Sugar.Config (Config(..))
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types as Sugar
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.IRef (IRef(..))
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)
import           Test.Lamdu.Env (EvalResults)

import           Test.Lamdu.Prelude

type T = Transaction

data IsHidden = NotHidden | Hidden deriving (Show)

allEntityIds ::
    WorkArea v name i o (Sugar.Payload v name i o, (t, [EntityId])) ->
    [(EntityId, IsHidden)]
allEntityIds workArea =
    (pls ^.. Lens.folded . _2 . _2 . Lens.folded
     <&> (, Hidden))
    <> (pls ^.. Lens.folded . _1 . plEntityId <&> (, NotHidden))
    where
        pls = workArea ^.. traverse

validateHiddenEntityIds ::
    WorkArea v name i o (Sugar.Payload v name i o, (t, [EntityId])) -> Either String ()
validateHiddenEntityIds workArea
    | Set.null hiddenAndExplicit = Right ()
    | otherwise =
        show hiddenAndExplicit ++ " are both hidden and explicit entityIds"
        & Left
    where
        pls = workArea ^.. traverse
        explicitEntityIds = pls ^.. Lens.folded . _1 . plEntityId & Set.fromList
        hiddenEntityIds =
            pls ^.. Lens.folded . _2 . _2 . Lens.folded
            & Set.fromList
        hiddenAndExplicit = Set.intersection explicitEntityIds hiddenEntityIds

data PaneLowLevel
    = PaneDefLowLevel (Def.Definition (Ann (HRef ViewM) # V.Term) (DefI ViewM))
    | PaneTagLowLevel T.Tag

Lens.makePrisms ''PaneLowLevel

data WorkAreaLowLevel = WorkAreaLowLevel
    { wallRepl :: Def.Expr (Ann (HRef ViewM) # V.Term)
    , wallPanes :: [PaneLowLevel]
    }

workAreaLowLevelEntityIds :: WorkAreaLowLevel -> Set EntityId
workAreaLowLevelEntityIds (WorkAreaLowLevel r p) =
    defExprs ^.. Lens.folded . Def.expr . hflipped
    >>= hfoldMap (\_ x -> [EntityId.EntityId (uuid (x ^. iref . _F))])
    & Set.fromList
    where
        defExprs = r : p ^.. Lens.folded . _PaneDefLowLevel . Def.defBody . Def._BodyExpr

loadPane :: Anchors.Pane ViewM -> T ViewM PaneLowLevel
loadPane (Anchors.PaneDefinition def) = ExprLoad.def def <&> PaneDefLowLevel
loadPane (Anchors.PaneTag tag) = PaneTagLowLevel tag & pure

workAreaLowLevelLoad :: T ViewM WorkAreaLowLevel
workAreaLowLevelLoad =
    WorkAreaLowLevel
    <$> ExprLoad.defExpr (repl codeAnchors)
    <*> (getP (panes codeAnchors) >>= traverse loadPane)

validate ::
    (NFData v, NFData t, NFData name) =>
    WorkArea v name (OnceT (T fa)) (T fb)
    (Sugar.Payload v name (OnceT (T fa)) (T fb), (t, [EntityId])) ->
    T ViewM
    ( WorkArea v name (OnceT (T fa)) (T fb)
        (Sugar.Payload v name (OnceT (T fa)) (T fb), (t, [EntityId]))
    )
validate workArea
    | Map.null duplicateEntityIds =
        do
            wallEntityIds <- workAreaLowLevelLoad <&> workAreaLowLevelEntityIds
            let missing = wallEntityIds `Set.difference` sugarEntityIdsSet
            unless (Set.null missing) $ error $
                show missing ++ " do not appear in any sugar entity ids"
            deepseq workArea -- make sure no "error" clauses are hiding within
                (validateHiddenEntityIds workArea)
                & either error (\() -> pure workArea)
    | otherwise =
        error ("duplicate entityIds: " <> show duplicateEntityIds)
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
    OnceT (T ViewM)
    ( WorkArea (Annotation (EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
        ( Sugar.Payload (Annotation (EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
        , (ParenInfo, [EntityId])
        )
    )
convertWorkArea env =
    (sugarWorkArea env codeAnchors >>= \x -> x (Tag.getTagName env) env)
    >>= lift . validate

testProgram :: FilePath -> OnceT (T ViewM) a -> IO a
testProgram program action =
    withDB ("test/programs/" <> program)
    (runDbTransaction ?? runAction (evalStateT (action ^. _OnceT) mempty))

sugarConfig :: Config
sugarConfig =
    Config
    { _showAllAnnotations = False
    , _sugarsEnabled = pure True
    }
