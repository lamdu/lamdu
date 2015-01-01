{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module InferAssert where

import AnnotatedExpr
import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (State, evalState, mapStateT)
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Formatting
import InferCombinators
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer (Infer(..))
import Lamdu.Infer.Error (Error)
import System.IO (hPutStrLn, stderr)
import Test.Framework (plusTestOptions)
import Test.Framework.Options (TestOptions'(..))
import Text.PrettyPrint.HughesPJ ((<+>))
import Text.PrettyPrint.HughesPJClass (pPrint)
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Update as Update
import qualified Test.Framework as TestFramework
import qualified Test.Framework.Providers.HUnit as HUnitProvider
import qualified Test.HUnit as HUnit

annotateTypes :: Val Type -> String
annotateTypes expr =
  expr
  & Lens.traverse %%~ fmap (:[]) . annotate
  & errorMessage
  & snd
  where
    annotate typ = addAnnotation ("Type: " ++ show typ)

type VarMap t = Map (T.Var t) (T.Var t)
type FreshSupply t = [T.Var t]
type VarState t = (VarMap t, FreshSupply t)

emptyVarState :: VarState t
emptyVarState = (Map.empty, map (fromString . (:[])) ['a'..])

type Canonizer = State (VarState Type, VarState (T.Composite T.Product))
runCanonizer :: Canonizer a -> a
runCanonizer = flip evalState (emptyVarState, emptyVarState)

canonizeTV :: T.Var t -> State (VarState t) (T.Var t)
canonizeTV tv =
  do  mtv <- Lens.use (_1 . Lens.at tv)
      case mtv of
        Just tv' -> return tv'
        Nothing ->
          do  tv' <- fresh
              _1 . Lens.at tv .= Just tv'
              return tv'
  where
    fresh =
      Lens.zoom _2 $
      do  (x:xs) <- State.get
          State.put xs
          return x

canonizeCompositeType ::
  T.Composite p -> State (VarState (T.Composite p)) (T.Composite p)
canonizeCompositeType =
  fmap f . go
  where
    f (fields, end) = foldr (uncurry T.CExtend) end (Map.toList fields)
    go (T.CExtend tag typ rest) = go rest <&> _1 %~ Map.insert tag typ
    go T.CEmpty = return (mempty, T.CEmpty)
    go (T.CVar ctv) =
      do  ctv' <- canonizeTV ctv
          return (mempty, T.CVar ctv')

canonizeType :: Type -> Canonizer Type
canonizeType (T.TVar tv) =
  T.TVar <$> Lens.zoom _1 (canonizeTV tv)
canonizeType (T.TRecord c) =
  Lens.zoom _2 (canonizeCompositeType c)
  <&> T.TRecord
  >>= T.nextLayer %%~ canonizeType
canonizeType t =
  t
  & T.nextLayer %%~ canonizeType

onInferError :: (Error -> Error) -> Infer a -> Infer a
onInferError f (Infer act) = Infer $ mapStateT (Lens._Left %~ f) act

-- An expr with both the actual infer result, and the expected result
data ExprTestPayload = ExprTestPayload
  { _etpExpectedType :: Type
  , _etpInferredType :: Type
  }

etpExpectedType :: Lens' ExprTestPayload Type
etpExpectedType f e =
  mk <$> f (_etpExpectedType e)
  where
    mk x = e { _etpExpectedType = x}

etpInferredType :: Lens' ExprTestPayload Type
etpInferredType f e =
  mk <$> f (_etpInferredType e)
  where
    mk x = e { _etpInferredType = x}

type ExprTest = Val ExprTestPayload
type ExprComplete = Val (Infer.Payload, Resumptions)

exprTestPayload :: (Infer.Payload, Resumptions) -> ExprTestPayload
exprTestPayload (pl, resumptions) = ExprTestPayload
  { _etpExpectedType = resumptions ^. rTyp
  , _etpInferredType = pl ^. Infer.plType
  }

canonizedExprTest :: ExprComplete -> ExprTest
canonizedExprTest =
  canonize etpExpectedType . canonize etpInferredType . fmap exprTestPayload
  where
    canonize t expr =
      expr
      & Lens.traversed . t %%~ canonizeType
      & runCanonizer

inferThrow :: Error -> Infer a
inferThrow = Infer . lift . Left

verifyExprComplete :: String -> ExprComplete -> Infer ()
verifyExprComplete msg expr
  | null errors = return ()
  | otherwise = inferThrow $ error $ msg ++ ": " ++ fullMsg
  where
    (errors, fullMsg) =
      errorMessage $ Lens.traverse verifyExprTestPayload $
      canonizedExprTest expr

verifyExprTestPayload :: ExprTestPayload -> AnnotationM [AnnotationIndex]
verifyExprTestPayload pl
  | i == e = return []
  | otherwise =
    fmap (:[]) . addAnnotation $ show $
    "Type mismatch:" <+> pPrint i <+> " (expected:" <+> pPrint e <+> ")"
  where
    i = pl ^. etpInferredType
    e = pl ^. etpExpectedType

inferVerifyExpr :: ExprWithResumptions -> Infer ExprComplete
inferVerifyExpr exprExpected =
  go (0 :: Int) =<< annotateErrors "initial infer: " (loadInferDef exprExpected)
  where
    msg count = "Resumption phase " ++ show count ++ " failed:\n"
    annotateErrors prefix = onInferError (error . (prefix ++) . show . pPrint)
    go count exprComplete = do
      verifyExprComplete (msg count) exprComplete
      (exprNextInput, (resumptionsExpected, resumptionsHappened)) <-
        annotateErrors (msg count) . runWriterT $
        handleResumption (verifyExprComplete (msg (count+1))) exprComplete
      case (resumptionsExpected, resumptionsHappened) of
        (Monoid.Any False, Monoid.Any False) -> return exprNextInput
        (Monoid.Any True, Monoid.Any False) ->
          error "NewInferred specified, but no subexpr has ResumeWith/ResumeOnSide"
        (_, _) -> go (count+1) exprNextInput

handleResumption ::
  (ExprComplete -> Infer ()) -> ExprComplete ->
  WriterT (Monoid.Any, Monoid.Any) Infer ExprComplete
handleResumption verifyInfersOnSide =
  lift . Update.inferredVal <=< go
  where
    recurseBody body pl = Val pl <$> Lens.traverse go body
    go (Val pl body) =
      case pl ^. _2 . rStep of
      ResumeWith newExpr -> do
        Writer.tell (Monoid.Any True, Monoid.Any True)
        lift $ loadInferInto (pl ^. _1) newExpr
      ResumeOnSide newExpr resumptions -> do
        Writer.tell (Monoid.Any True, Monoid.Any True)
        lift $ verifyInfersOnSide =<< Update.inferredVal =<<
          loadInferScope (pl ^. _1 . Infer.plScope) newExpr
        recurseBody body $ pl & _2 .~ resumptions
      NewInferred resumptions -> do
        Writer.tell (Monoid.Any True, Monoid.Any False)
        recurseBody body $ pl & _2 .~ resumptions
      Final ->
        recurseBody body pl

expectLeft ::
  String -> (l -> HUnit.Assertion) ->
  Either l (Val Type) -> HUnit.Assertion
expectLeft _ handleLeft (Left x) = handleLeft x
expectLeft msg _ (Right x) =
  error $ unwords
  [ "Error", msg, "expected.  Unexpected success encountered:"
  , "\n", x & UnescapedStr . annotateTypes & show
  ]

inferFailsAssertion ::
  String -> (Error -> Bool) -> Val () -> HUnit.Assertion
inferFailsAssertion errorName isExpectedError val =
  expectLeft errorName verifyError $ (fmap . fmap) (^. _1 . Infer.plType) $
  runNewContext $ loadInferDef val
  where
    verifyError err
      | isExpectedError err = return ()
      | otherwise =
        error $ errorName ++ " error expected, but got: " ++ show (pPrint err)

allowFailAssertion :: String -> HUnit.Assertion -> HUnit.Assertion
allowFailAssertion msg assertion =
  (assertion >> successOccurred) `E.catch`
  \(E.SomeException _) -> errorOccurred
  where
    successOccurred =
      hPutStrLn stderr . ansiAround ansiYellow $ "NOTE: doesn't fail. Remove AllowFail?"
    errorOccurred =
      hPutStrLn stderr . ansiAround ansiYellow $
      concat ["WARNING: Allowing failure (", msg, ") in:"]

defaultTestOptions :: TestOptions' Maybe
defaultTestOptions = mempty { topt_timeout = Just (Just 4000000) }

testCase :: TestFramework.TestName -> HUnit.Assertion -> TestFramework.Test
testCase name = plusTestOptions defaultTestOptions . HUnitProvider.testCase name

inferAssertion :: ExprWithResumptions -> HUnit.Assertion
inferAssertion =
  either (error . show . pPrint) (const (return ())) .
  runNewContext . inferVerifyExpr

testInfer :: String -> ExprWithResumptions -> TestFramework.Test
testInfer name = testCase name . inferAssertion

testInferAllowFail :: String -> String -> ExprWithResumptions -> TestFramework.Test
testInferAllowFail msg name expr =
  testCase name . allowFailAssertion msg $ inferAssertion expr
