{-# LANGUAGE TemplateHaskell #-}

module Editor.Data.Infer.Rules
  ( Rule(..)
  , makeAllRules, unionRules
  , runRuleClosure
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens ((^.))
import Control.Monad.Trans.State (State)
import Control.Monad.Trans.Writer (execWriter)
import Control.Monad.Unit (Unit(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (maybeToList)
import Data.Store.Guid (Guid)
import Editor.Data.Infer.Types
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Store.Guid as Guid
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data

type RuleFunction def = [RefExpression def] -> [(ExprRef, RefExpression def)]

type Origin2 = (Origin, Origin)
type Origin3 = (Origin, Origin, Origin)
mkOrigin2 :: State Origin Origin2
mkOrigin2 = (,) <$> mkOrigin <*> mkOrigin
mkOrigin3 :: State Origin Origin3
mkOrigin3 = (,,) <$> mkOrigin <*> mkOrigin <*> mkOrigin

data ExprLambdaWrapper = ExprLambda | ExprPi
derive makeBinary ''ExprLambdaWrapper

-- Boilerplate to work around lack of serialization of functions
-- Represents a serialization of RuleFunction:
data RuleClosure def
  = LambdaBodyTypeToPiResultTypeClosure (Guid, ExprRef) Origin2
  | PiToLambdaClosure (Guid, ExprRef, ExprRef) Origin3
  | CopyClosure ExprRef
  | LambdaParentToChildrenClosure (ExprLambdaWrapper, ExprRef, ExprRef)
  | LambdaChildrenToParentClosure (ExprLambdaWrapper, Guid, ExprRef) Origin
  | SetClosure [(ExprRef, RefExpression def)]
  | SimpleTypeClosure ExprRef Origin2
  | IntoApplyResultClosure (ExprLambdaWrapper, ExprRef, ExprRef)
  | IntoArgClosure (ExprLambdaWrapper, ExprRef)
  | IntoFuncResultTypeClosure (ExprLambdaWrapper, ExprRef)
  | ArgTypeToPiParamTypeClosure ExprRef Origin2
  | RigidArgApplyTypeToResultTypeClosure ExprRef Origin3
  | PiParamTypeToArgTypeClosure ExprRef
  | LambdaParamTypeToArgTypeClosure ExprRef
  | ArgTypeToLambdaParamTypeClosure ExprRef Origin
  | NonLambdaToApplyValueClosure ExprRef Origin
  | ApplyArgToFuncArgClosure ExprRef
derive makeBinary ''RuleClosure

data Rule def = Rule
  { ruleInputs :: [ExprRef]
  , _ruleCompute :: RuleClosure def
  }
derive makeBinary ''Rule

runRuleClosure :: Eq def => RuleClosure def -> RuleFunction def
runRuleClosure closure =
  case closure of
  LambdaBodyTypeToPiResultTypeClosure x o ->
    runLambdaBodyTypeToPiResultTypeClosure x o
  PiToLambdaClosure x o ->
    runPiToLambdaClosure x o
  CopyClosure x ->
    runCopyClosure x
  LambdaParentToChildrenClosure x ->
    runLambdaParentToChildrenClosure x
  LambdaChildrenToParentClosure x o ->
    runLambdaChildrenToParentClosure x o
  SetClosure x ->
    runSetClosure x
  SimpleTypeClosure x o ->
    runSimpleTypeClosure x o
  IntoApplyResultClosure x ->
    runIntoApplyResultClosure x
  IntoArgClosure x ->
    runIntoArgClosure x
  IntoFuncResultTypeClosure x ->
    runIntoFuncResultTypeClosure x
  ArgTypeToPiParamTypeClosure x o ->
    runArgTypeToPiParamTypeClosure x o
  RigidArgApplyTypeToResultTypeClosure x o ->
    runRigidArgApplyTypeToResultTypeClosure x o
  PiParamTypeToArgTypeClosure x ->
    runPiParamTypeToArgTypeClosure x
  LambdaParamTypeToArgTypeClosure x ->
    runLambdaParamTypeToArgTypeClosure x
  ArgTypeToLambdaParamTypeClosure x o ->
    runArgTypeToLambdaParamTypeClosure x o
  NonLambdaToApplyValueClosure x o ->
    runNonLambdaToApplyValueClosure x o
  ApplyArgToFuncArgClosure x ->
    runApplyArgToFuncArgClosure x

makeNodeRules :: Data.Expression def TypedValue -> State Origin [Rule def]
makeNodeRules (Data.Expression exprBody typedVal) =
  case fmap (Lens.view Data.ePayload) exprBody of
  Data.ExpressionPi lambda@(Data.Lambda _ _ resultType) ->
    (:) <$> setRule (tvType resultType) <*>
    onLambda ExprPi lambda
  Data.ExpressionLambda lambda@(Data.Lambda param _ body) ->
    (++) <$> lambdaRules param typedVal (tvType body) <*>
    onLambda ExprLambda lambda
  Data.ExpressionApply apply -> applyRules typedVal apply
  _ -> pure []
  where
    setRule ref = do
      o <- mkOrigin
      return . Rule [] $ SetClosure [(ref, setExpr o)]
    onLambda cons lam@(Data.Lambda _ paramType _) =
      (:) <$> setRule (tvType paramType) <*>
      lambdaStructureRules cons (tvVal typedVal) (fmap tvVal lam)

makeAllRules :: Data.Expression def TypedValue -> State Origin [Rule def]
makeAllRules expr =
  (:)
  <$> (ruleSimpleType . Lens.view Data.ePayload) expr
  <*> makeResumptionRules
  where
    makeResumptionRules =
      (++)
      <$> makeNodeRules expr
      <*> (fmap Foldable.concat . Traversable.mapM makeAllRules . Lens.view Data.eValue) expr

makeHole :: Origin -> RefExpression def
makeHole g = makeRefExpr g $ Data.ExpressionLeaf Data.Hole

setExpr :: Origin -> RefExpression def
setExpr g = makeRefExpr g $ Data.ExpressionLeaf Data.Set

intTypeExpr :: Origin -> RefExpression def
intTypeExpr g = makeRefExpr g $ Data.ExpressionLeaf Data.IntegerType

guidFromOrigin :: Origin -> Guid
guidFromOrigin origin = Guid.fromString $ show origin ++ "(orig)"

makePi :: Origin -> RefExpression def -> RefExpression def -> RefExpression def
makePi o paramType result =
  makeRefExpr o $ Data.makePi (guidFromOrigin o) paramType result

maybeLambda :: Data.ExpressionBody def a -> Maybe (Data.Lambda a)
maybeLambda (Data.ExpressionLambda x) = Just x
maybeLambda _ = Nothing

maybePi :: Data.ExpressionBody def a -> Maybe (Data.Lambda a)
maybePi (Data.ExpressionPi x) = Just x
maybePi _ = Nothing

exprLambdaCons :: ExprLambdaWrapper -> Data.Lambda expr -> Data.ExpressionBody def expr
exprLambdaCons ExprLambda = Data.ExpressionLambda
exprLambdaCons ExprPi = Data.ExpressionPi

exprLambdaUncons ::
  ExprLambdaWrapper ->
  Data.ExpressionBody def expr ->
  Maybe (Data.Lambda expr)
exprLambdaUncons ExprLambda = maybeLambda
exprLambdaUncons ExprPi = maybePi

runLambdaBodyTypeToPiResultTypeClosure :: (Guid, ExprRef) -> Origin2 -> RuleFunction def
runLambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) (o0, o1) ~[bodyTypeExpr] =
  [( lambdaTypeRef
   , makeRefExpr o0 $ Data.makePi param (makeHole o1) bodyTypeExpr
   )]

runPiToLambdaClosure :: (Guid, ExprRef, ExprRef) -> Origin3 -> RuleFunction def
runPiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) (o0, o1, o2) ~[Data.Expression piBody _] = do
  Data.Lambda piParam paramType resultType <- maybeToList $ maybePi piBody
  [ -- Pi result type -> Body type
    ( bodyTypeRef
    , subst piParam
      ( makeRefExpr o0
        (Data.makeParameterRef param)
      )
      resultType
    )
    , -- Pi param type -> Lambda param type
      ( lambdaValueRef
      , makeRefExpr o1 . Data.makeLambda piParam paramType $ makeHole o2
      )
    ]

lambdaRules :: Guid -> TypedValue -> ExprRef -> State Origin [Rule def]
lambdaRules param (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  sequence
  [ Rule [bodyTypeRef] . LambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) <$> mkOrigin2
  , Rule [lambdaTypeRef] . PiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) <$> mkOrigin3
  ]

runCopyClosure :: ExprRef -> RuleFunction def
runCopyClosure dest ~[srcExpr] = [(dest, srcExpr)]

unionRules :: ExprRef -> ExprRef -> [Rule def]
unionRules x y =
  [ Rule [x] $ CopyClosure y
  , Rule [y] $ CopyClosure x
  ]

-- Parent lambda to children
runLambdaParentToChildrenClosure :: (ExprLambdaWrapper, ExprRef, ExprRef) -> RuleFunction def
runLambdaParentToChildrenClosure (cons, paramTypeRef, resultRef) ~[expr] = do
  Data.Lambda _ paramTypeE resultE <-
    maybeToList . exprLambdaUncons cons $ expr ^. Data.eValue
  [(paramTypeRef, paramTypeE), (resultRef, resultE)]

-- Children of lambda to lambda parent
runLambdaChildrenToParentClosure :: (ExprLambdaWrapper, Guid, ExprRef) -> Origin -> RuleFunction def
runLambdaChildrenToParentClosure (cons, param, lamRef) o0 ~[paramTypeExpr, resultExpr] =
  [( lamRef
   , makeRefExpr o0 . exprLambdaCons cons $
     Data.Lambda param paramTypeExpr resultExpr
   )]

lambdaStructureRules :: ExprLambdaWrapper -> ExprRef -> Data.Lambda ExprRef -> State Origin [Rule def]
lambdaStructureRules cons lamRef (Data.Lambda param paramTypeRef resultRef) =
  sequence
  [ pure . Rule [lamRef] $
    LambdaParentToChildrenClosure (cons, paramTypeRef, resultRef)
  , -- Copy the structure from the children to the parent
    Rule [paramTypeRef, resultRef] .
    LambdaChildrenToParentClosure (cons, param, lamRef) <$> mkOrigin
  ]

runSetClosure :: [(ExprRef, RefExpression def)] -> RuleFunction def
runSetClosure outputs ~[] = outputs

subst ::
  Guid -> Data.Expression def a ->
  Data.Expression def a -> Data.Expression def a
subst from to expr =
  case expr ^. Data.eValue of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef paramRef))
    | paramRef == from -> to
  _ ->
    (Lens.over Data.eValue . fmap)
    (subst from to) expr

mergeToPiResult ::
  Eq def => RefExpression def -> RefExpression def -> RefExpression def
mergeToPiResult =
  fmap runIdentity .
  Data.matchExpression onMatch ((fmap . fmap) return onMismatch)
  where
    onMatch x _ = return x
    onMismatch destHole@(Data.Expression (Data.ExpressionLeaf Data.Hole) destPayload) src
      | IntSet.null substs = destHole
      | otherwise = fmap (Lens.set rplSubstitutedArgs substs) src
      where
        substs = destPayload ^. rplSubstitutedArgs
    -- TODO: This seems like it should report an error,
    -- verify/document that it is OK because the other direction of
    -- information flow will catch any error:
    onMismatch dest _ = dest

maybeApply :: Data.ExpressionBody def a -> Maybe (Data.Apply a)
maybeApply (Data.ExpressionApply x) = Just x
maybeApply _ = Nothing

runSimpleTypeClosure :: ExprRef -> Origin2 -> RuleFunction def
runSimpleTypeClosure typ (o0, o1) ~[valExpr] =
  case valExpr ^. Data.eValue of
  Data.ExpressionLeaf Data.Set -> [(typ, setExpr o0)]
  Data.ExpressionLeaf Data.IntegerType -> [(typ, setExpr o0)]
  Data.ExpressionLeaf (Data.LiteralInteger _) -> [(typ, intTypeExpr o0)]
  Data.ExpressionPi _ -> [(typ, setExpr o0)]
  Data.ExpressionLambda (Data.Lambda param paramType _) ->
    [( typ
     , makeRefExpr o0 . Data.makePi param paramType $
       makeHole o1
     )]
  _ -> []

ruleSimpleType :: TypedValue -> State Origin (Rule def)
ruleSimpleType (TypedValue val typ) = Rule [val] . SimpleTypeClosure typ <$> mkOrigin2

runIntoApplyResultClosure :: (ExprLambdaWrapper, ExprRef, ExprRef) -> RuleFunction def
runIntoApplyResultClosure (cons, applyRef, arg) ~[Data.Expression funcBody _, argExpr] = do
  Data.Lambda param _ result <- maybeToList $ exprLambdaUncons cons funcBody
  return
    ( applyRef
    , subst param
      ((Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.insert) (unExprRef arg) argExpr)
      result
    )

intoApplyResultRule :: ExprLambdaWrapper -> ExprRef -> ExprRef -> ExprRef -> (Rule def)
intoApplyResultRule cons applyRef func arg =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  Rule [func, arg] $ IntoApplyResultClosure (cons, applyRef, arg)

runIntoArgClosure :: Eq def => (ExprLambdaWrapper, ExprRef) -> RuleFunction def
runIntoArgClosure (cons, arg) ~[applyExpr, Data.Expression funcBody _] = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Data.Lambda param _ result <- maybeToList $ exprLambdaUncons cons funcBody
  mergeToArg param arg result applyExpr

intoArgRule :: ExprLambdaWrapper -> ExprRef -> ExprRef -> ExprRef -> (Rule def)
intoArgRule cons applyRef func arg =
  Rule [applyRef, func] $ IntoArgClosure (cons, arg)

runIntoFuncResultTypeClosure :: Eq def => (ExprLambdaWrapper, ExprRef) -> RuleFunction def
runIntoFuncResultTypeClosure (cons, func) ~[applyExpr, Data.Expression funcBody funcPl] = do
  Data.Lambda param paramT result <- maybeToList $ exprLambdaUncons cons funcBody
  return
    ( func
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      exprLambdaCons cons . Data.Lambda param paramT $
      mergeToPiResult result applyExpr
    )

intoFuncResultTypeRule :: ExprLambdaWrapper -> ExprRef -> ExprRef -> (Rule def)
intoFuncResultTypeRule cons applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  Rule [applyRef, func] $ IntoFuncResultTypeClosure (cons, func)

recurseSubstRules :: ExprLambdaWrapper -> ExprRef -> ExprRef -> ExprRef -> [Rule def]
recurseSubstRules cons applyRef func arg =
  [ intoApplyResultRule cons applyRef func arg
  , intoArgRule cons applyRef func arg
  , intoFuncResultTypeRule cons applyRef func
  ]

mergeToArg :: Eq def => Guid -> ExprRef -> RefExpression def -> RefExpression def -> [(ExprRef, RefExpression def)]
mergeToArg param arg =
  (fmap . fmap) (execWriter . Compose.unO) $
  Data.matchExpression onMatch onMismatch
  where
    unit = Compose.O (pure Unit)
    onMatch _ _ = unit
    onMismatch
      Data.Expression
      { Data._eValue = Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g)) } post
      | g == param =
        Compose.O . (fmap . const) Unit $ Writer.tell
        [( arg
         , (Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.delete) (unExprRef arg) post
         )]
    onMismatch _ _ = unit

runArgTypeToPiParamTypeClosure :: ExprRef -> Origin2 -> RuleFunction def
runArgTypeToPiParamTypeClosure funcTypeRef (o0, o1) ~[argTypeExpr] =
  [( funcTypeRef
   , makePi o0 argTypeExpr $ makeHole o1
   )]

argTypeToPiParamTypeRule :: Data.Apply TypedValue -> State Origin (Rule def)
argTypeToPiParamTypeRule (Data.Apply func arg) =
  -- ArgT => Pi ParamT
  Rule [tvType arg] . ArgTypeToPiParamTypeClosure (tvType func) <$> mkOrigin2

runRigidArgApplyTypeToResultTypeClosure :: ExprRef -> Origin3 -> RuleFunction def
runRigidArgApplyTypeToResultTypeClosure funcTypeRef (o0, o1, o2) ~[applyTypeExpr, argExpr] =
  case argExpr ^. Data.eValue of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par)) ->
    [ ( funcTypeRef
      , makePi o0 (makeHole o1) $
        subst par (makeHole o2) applyTypeExpr
      )
    ]
  _ -> []

rigidArgApplyTypeToResultTypeRule ::
  TypedValue -> Data.Apply TypedValue -> State Origin (Rule def)
rigidArgApplyTypeToResultTypeRule applyTv (Data.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  Rule [tvType applyTv, tvVal arg] .
  RigidArgApplyTypeToResultTypeClosure (tvType func) <$> mkOrigin3

runPiParamTypeToArgTypeClosure :: ExprRef -> RuleFunction def
runPiParamTypeToArgTypeClosure argTypeRef ~[Data.Expression funcTExpr _] = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Data.Lambda _ paramT _ <- maybeToList $ maybePi funcTExpr
  return (argTypeRef, paramT)

piParamTypeToArgTypeRule :: Data.Apply TypedValue -> (Rule def)
piParamTypeToArgTypeRule (Data.Apply func arg) =
  Rule [tvType func] $ PiParamTypeToArgTypeClosure (tvType arg)

runLambdaParamTypeToArgTypeClosure :: ExprRef -> RuleFunction def
runLambdaParamTypeToArgTypeClosure argTypeRef ~[Data.Expression funcExpr _] = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Data.Lambda _ paramT _ <- maybeToList $ maybeLambda funcExpr
  return (argTypeRef, paramT)

lambdaParamTypeToArgTypeRule :: Data.Apply TypedValue -> (Rule def)
lambdaParamTypeToArgTypeRule (Data.Apply func arg) =
  Rule [tvVal func] $ LambdaParamTypeToArgTypeClosure (tvType arg)

runArgTypeToLambdaParamTypeClosure :: ExprRef -> Origin -> RuleFunction def
runArgTypeToLambdaParamTypeClosure funcValRef o0 ~[Data.Expression funcExpr funcPl, argTExpr] = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Data.Lambda param _ _ <- maybeToList $ maybeLambda funcExpr
  return
    ( funcValRef
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      Data.makeLambda param argTExpr $ makeHole o0
    )

argTypeToLambdaParamTypeRule :: Data.Apply TypedValue -> State Origin (Rule def)
argTypeToLambdaParamTypeRule (Data.Apply func arg) =
  Rule [tvVal func, tvType arg] . ArgTypeToLambdaParamTypeClosure (tvVal func) <$> mkOrigin

runNonLambdaToApplyValueClosure :: ExprRef -> Origin -> RuleFunction def
runNonLambdaToApplyValueClosure applyValRef o0 ~[funcExpr, argExpr] =
  -- If func is surely not a lambda (a hole too could be a lambda).
  --
  -- Applies have a special case in the inferred value handling for
  -- redexes, and this rule is about detecting that an application is
  -- *not* a redex, so we can safely set the inferred value to the
  -- application itself.
  --
  -- Func Arg => Outer
  case funcExpr ^. Data.eValue of
  Data.ExpressionLambda _ -> []
  Data.ExpressionLeaf Data.Hole -> []
  _ ->
    [ ( applyValRef
      , makeRefExpr o0 $ Data.makeApply funcExpr argExpr
      )
    ]

nonLambdaToApplyValueRule :: TypedValue -> Data.Apply TypedValue -> State Origin (Rule def)
nonLambdaToApplyValueRule applyTv (Data.Apply func arg) =
  Rule [tvVal func, tvVal arg] .
  NonLambdaToApplyValueClosure (tvVal applyTv) <$> mkOrigin

runApplyArgToFuncArgClosure :: Eq def => ExprRef -> RuleFunction def
runApplyArgToFuncArgClosure argValRef ~[applyExpr, funcExpr] = maybeToList $ do
  -- If Func is same as in Apply,
  -- Apply-Arg => Arg
  Data.Apply aFunc aArg <- maybeApply $ applyExpr ^. Data.eValue
  _ <-
    Data.matchExpression
    ((const . const) (Just ()))
    ((const . const) Nothing)
    aFunc funcExpr
  return (argValRef, aArg)

applyArgToFuncArgRule ::
  TypedValue -> Data.Apply TypedValue -> Rule def
applyArgToFuncArgRule applyTv (Data.Apply func arg) =
  Rule [tvVal applyTv, tvVal func] $ ApplyArgToFuncArgClosure (tvVal arg)

applyRules :: TypedValue -> Data.Apply TypedValue -> State Origin [Rule def]
applyRules applyTv apply@(Data.Apply func arg) =
  -- TODO: make all of these functions have a standard signature and
  -- just apply them all to the same args?
  (++ pureRules) <$>
  sequence
  [ argTypeToPiParamTypeRule apply
  , rigidArgApplyTypeToResultTypeRule applyTv apply
  , argTypeToLambdaParamTypeRule apply
  , nonLambdaToApplyValueRule applyTv apply
  ]
  where
    pureRules =
      [ piParamTypeToArgTypeRule apply
      , lambdaParamTypeToArgTypeRule apply
      , applyArgToFuncArgRule applyTv apply
      ]
      ++ recurseSubstRules ExprPi
        (tvType applyTv) (tvType func) (tvVal arg)
      ++ recurseSubstRules ExprLambda
        (tvVal applyTv) (tvVal func) (tvVal arg)
