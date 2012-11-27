{-# LANGUAGE TemplateHaskell #-}

module Editor.Data.Infer.Rules
  ( Rule(..)
  , makeAllRules, makeResumptionRules
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
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data

type RuleFunction = [RefExpression] -> [(Ref, RefExpression)]

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
data RuleClosure
  = LambdaBodyTypeToPiResultTypeClosure (Guid, Ref) Origin2
  | PiToLambdaClosure (Guid, Ref, Ref) Origin3
  | CopyClosure Ref
  | LambdaParentToChildrenClosure (ExprLambdaWrapper, Ref, Ref)
  | LambdaChildrenToParentClosure (ExprLambdaWrapper, Guid, Ref) Origin
  | SetClosure [(Ref, RefExpression)]
  | SimpleTypeClosure Ref Origin2
  | IntoApplyResultClosure (ExprLambdaWrapper, Ref, Ref)
  | IntoArgClosure (ExprLambdaWrapper, Ref)
  | IntoFuncResultTypeClosure (ExprLambdaWrapper, Ref)
  | ArgTypeToPiParamTypeClosure Ref Origin2
  | RigidArgApplyTypeToResultTypeClosure Ref Origin3
  | PiParamTypeToArgTypeClosure Ref
  | LambdaParamTypeToArgTypeClosure Ref
  | ArgTypeToLambdaParamTypeClosure Ref Origin
  | NonLambdaToApplyValueClosure Ref Origin
  | ApplyArgToFuncArgClosure Ref
derive makeBinary ''RuleClosure

data Rule = Rule
  { ruleInputs :: [Ref]
  , _ruleCompute :: RuleClosure
  }
derive makeBinary ''Rule

runRuleClosure :: RuleClosure -> RuleFunction
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

makeNodeRules :: Data.Expression Data.DefinitionIRef InferNode -> State Origin [Rule]
makeNodeRules (Data.Expression exprBody (InferNode typedVal scope)) =
  case fmap (nRefs . Lens.view Data.ePayload) exprBody of
  Data.ExpressionPi lambda@(Data.Lambda _ _ resultType) ->
    (:) <$> setRule (tvType resultType) <*>
    onLambda ExprPi lambda
  Data.ExpressionLambda lambda@(Data.Lambda param _ body) ->
    (++) <$> lambdaRules param typedVal (tvType body) <*>
    onLambda ExprLambda lambda
  Data.ExpressionApply apply -> applyRules typedVal apply
  Data.ExpressionLeaf (Data.GetVariable var) -> pure $ do
    ref <- maybeToList $ Map.lookup var scope
    unionRules ref $ tvType typedVal
  _ -> pure []
  where
    setRule ref = do
      o <- mkOrigin
      return . Rule [] $ SetClosure [(ref, setExpr o)]
    onLambda cons lam@(Data.Lambda _ paramType _) =
      (:) <$> setRule (tvType paramType) <*>
      lambdaStructureRules cons (tvVal typedVal) (fmap tvVal lam)

makeResumptionRules :: Data.Expression Data.DefinitionIRef InferNode -> State Origin [Rule]
makeResumptionRules expr =
  (++)
  <$> makeNodeRules expr
  <*> (fmap Foldable.concat . Traversable.mapM makeAllRules . Lens.view Data.eValue) expr

makeAllRules :: Data.Expression Data.DefinitionIRef InferNode -> State Origin [Rule]
makeAllRules expr =
  (:)
  <$> (ruleSimpleType . nRefs . Lens.view Data.ePayload) expr
  <*> makeResumptionRules expr

makeHole :: Origin -> RefExpression
makeHole g = makeRefExpr g $ Data.ExpressionLeaf Data.Hole

setExpr :: Origin -> RefExpression
setExpr g = makeRefExpr g $ Data.ExpressionLeaf Data.Set

intTypeExpr :: Origin -> RefExpression
intTypeExpr g = makeRefExpr g $ Data.ExpressionLeaf Data.IntegerType

guidFromOrigin :: Origin -> Guid
guidFromOrigin origin = Guid.fromString $ show origin ++ "(orig)"

makePi :: Origin -> RefExpression -> RefExpression -> RefExpression
makePi o paramType result =
  makeRefExpr o $ Data.makePi (guidFromOrigin o) paramType result

maybeLambda :: Data.ExpressionBody Data.DefinitionIRef a -> Maybe (Data.Lambda a)
maybeLambda (Data.ExpressionLambda x) = Just x
maybeLambda _ = Nothing

maybePi :: Data.ExpressionBody Data.DefinitionIRef a -> Maybe (Data.Lambda a)
maybePi (Data.ExpressionPi x) = Just x
maybePi _ = Nothing

exprLambdaCons :: ExprLambdaWrapper -> Data.Lambda expr -> Data.ExpressionBody Data.DefinitionIRef expr
exprLambdaCons ExprLambda = Data.ExpressionLambda
exprLambdaCons ExprPi = Data.ExpressionPi

exprLambdaUncons ::
  ExprLambdaWrapper ->
  Data.ExpressionBody Data.DefinitionIRef expr ->
  Maybe (Data.Lambda expr)
exprLambdaUncons ExprLambda = maybeLambda
exprLambdaUncons ExprPi = maybePi

runLambdaBodyTypeToPiResultTypeClosure :: (Guid, Ref) -> Origin2 -> RuleFunction
runLambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) (o0, o1) ~[bodyTypeExpr] =
  [( lambdaTypeRef
   , makeRefExpr o0 $ Data.makePi param (makeHole o1) bodyTypeExpr
   )]

runPiToLambdaClosure :: (Guid, Ref, Ref) -> Origin3 -> RuleFunction
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

lambdaRules :: Guid -> TypedValue -> Ref -> State Origin [Rule]
lambdaRules param (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  sequence
  [ Rule [bodyTypeRef] . LambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) <$> mkOrigin2
  , Rule [lambdaTypeRef] . PiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) <$> mkOrigin3
  ]

runCopyClosure :: Ref -> RuleFunction
runCopyClosure dest ~[srcExpr] = [(dest, srcExpr)]

unionRules :: Ref -> Ref -> [Rule]
unionRules x y =
  [ Rule [x] $ CopyClosure y
  , Rule [y] $ CopyClosure x
  ]

-- Parent lambda to children
runLambdaParentToChildrenClosure :: (ExprLambdaWrapper, Ref, Ref) -> RuleFunction
runLambdaParentToChildrenClosure (cons, paramTypeRef, resultRef) ~[expr] = do
  Data.Lambda _ paramTypeE resultE <-
    maybeToList . exprLambdaUncons cons $ expr ^. Data.eValue
  [(paramTypeRef, paramTypeE), (resultRef, resultE)]

-- Children of lambda to lambda parent
runLambdaChildrenToParentClosure :: (ExprLambdaWrapper, Guid, Ref) -> Origin -> RuleFunction
runLambdaChildrenToParentClosure (cons, param, lamRef) o0 ~[paramTypeExpr, resultExpr] =
  [( lamRef
   , makeRefExpr o0 . exprLambdaCons cons $
     Data.Lambda param paramTypeExpr resultExpr
   )]

lambdaStructureRules :: ExprLambdaWrapper -> Ref -> Data.Lambda Ref -> State Origin [Rule]
lambdaStructureRules cons lamRef (Data.Lambda param paramTypeRef resultRef) =
  sequence
  [ pure . Rule [lamRef] $
    LambdaParentToChildrenClosure (cons, paramTypeRef, resultRef)
  , -- Copy the structure from the children to the parent
    Rule [paramTypeRef, resultRef] .
    LambdaChildrenToParentClosure (cons, param, lamRef) <$> mkOrigin
  ]

runSetClosure :: [(Ref, RefExpression)] -> RuleFunction
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
  RefExpression -> RefExpression -> RefExpression
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

runSimpleTypeClosure :: Ref -> Origin2 -> RuleFunction
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

ruleSimpleType :: TypedValue -> State Origin Rule
ruleSimpleType (TypedValue val typ) = Rule [val] . SimpleTypeClosure typ <$> mkOrigin2

runIntoApplyResultClosure :: (ExprLambdaWrapper, Ref, Ref) -> RuleFunction
runIntoApplyResultClosure (cons, applyRef, arg) ~[Data.Expression funcBody _, argExpr] = do
  Data.Lambda param _ result <- maybeToList $ exprLambdaUncons cons funcBody
  return
    ( applyRef
    , subst param
      ((Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.insert) (unRef arg) argExpr)
      result
    )

intoApplyResultRule :: ExprLambdaWrapper -> Ref -> Ref -> Ref -> Rule
intoApplyResultRule cons applyRef func arg =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  Rule [func, arg] $ IntoApplyResultClosure (cons, applyRef, arg)

runIntoArgClosure :: (ExprLambdaWrapper, Ref) -> RuleFunction
runIntoArgClosure (cons, arg) ~[applyExpr, Data.Expression funcBody _] = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Data.Lambda param _ result <- maybeToList $ exprLambdaUncons cons funcBody
  mergeToArg param arg result applyExpr

intoArgRule :: ExprLambdaWrapper -> Ref -> Ref -> Ref -> Rule
intoArgRule cons applyRef func arg =
  Rule [applyRef, func] $ IntoArgClosure (cons, arg)

runIntoFuncResultTypeClosure :: (ExprLambdaWrapper, Ref) -> RuleFunction
runIntoFuncResultTypeClosure (cons, func) ~[applyExpr, Data.Expression funcBody funcPl] = do
  Data.Lambda param paramT result <- maybeToList $ exprLambdaUncons cons funcBody
  return
    ( func
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      exprLambdaCons cons . Data.Lambda param paramT $
      mergeToPiResult result applyExpr
    )

intoFuncResultTypeRule :: ExprLambdaWrapper -> Ref -> Ref -> Rule
intoFuncResultTypeRule cons applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  Rule [applyRef, func] $ IntoFuncResultTypeClosure (cons, func)

recurseSubstRules :: ExprLambdaWrapper -> Ref -> Ref -> Ref -> [Rule]
recurseSubstRules cons applyRef func arg =
  [ intoApplyResultRule cons applyRef func arg
  , intoArgRule cons applyRef func arg
  , intoFuncResultTypeRule cons applyRef func
  ]

mergeToArg :: Guid -> Ref -> RefExpression -> RefExpression -> [(Ref, RefExpression)]
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
         , (Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.delete) (unRef arg) post
         )]
    onMismatch _ _ = unit

runArgTypeToPiParamTypeClosure :: Ref -> Origin2 -> RuleFunction
runArgTypeToPiParamTypeClosure funcTypeRef (o0, o1) ~[argTypeExpr] =
  [( funcTypeRef
   , makePi o0 argTypeExpr $ makeHole o1
   )]

argTypeToPiParamTypeRule :: Data.Apply TypedValue -> State Origin Rule
argTypeToPiParamTypeRule (Data.Apply func arg) =
  -- ArgT => Pi ParamT
  Rule [tvType arg] . ArgTypeToPiParamTypeClosure (tvType func) <$> mkOrigin2

runRigidArgApplyTypeToResultTypeClosure :: Ref -> Origin3 -> RuleFunction
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
  TypedValue -> Data.Apply TypedValue -> State Origin Rule
rigidArgApplyTypeToResultTypeRule applyTv (Data.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  Rule [tvType applyTv, tvVal arg] .
  RigidArgApplyTypeToResultTypeClosure (tvType func) <$> mkOrigin3

runPiParamTypeToArgTypeClosure :: Ref -> RuleFunction
runPiParamTypeToArgTypeClosure argTypeRef ~[Data.Expression funcTExpr _] = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Data.Lambda _ paramT _ <- maybeToList $ maybePi funcTExpr
  return (argTypeRef, paramT)

piParamTypeToArgTypeRule :: Data.Apply TypedValue -> Rule
piParamTypeToArgTypeRule (Data.Apply func arg) =
  Rule [tvType func] $ PiParamTypeToArgTypeClosure (tvType arg)

runLambdaParamTypeToArgTypeClosure :: Ref -> RuleFunction
runLambdaParamTypeToArgTypeClosure argTypeRef ~[Data.Expression funcExpr _] = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Data.Lambda _ paramT _ <- maybeToList $ maybeLambda funcExpr
  return (argTypeRef, paramT)

lambdaParamTypeToArgTypeRule :: Data.Apply TypedValue -> Rule
lambdaParamTypeToArgTypeRule (Data.Apply func arg) =
  Rule [tvVal func] $ LambdaParamTypeToArgTypeClosure (tvType arg)

runArgTypeToLambdaParamTypeClosure :: Ref -> Origin -> RuleFunction
runArgTypeToLambdaParamTypeClosure funcValRef o0 ~[Data.Expression funcExpr funcPl, argTExpr] = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Data.Lambda param _ _ <- maybeToList $ maybeLambda funcExpr
  return
    ( funcValRef
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      Data.makeLambda param argTExpr $ makeHole o0
    )

argTypeToLambdaParamTypeRule :: Data.Apply TypedValue -> State Origin Rule
argTypeToLambdaParamTypeRule (Data.Apply func arg) =
  Rule [tvVal func, tvType arg] . ArgTypeToLambdaParamTypeClosure (tvVal func) <$> mkOrigin

runNonLambdaToApplyValueClosure :: Ref -> Origin -> RuleFunction
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

nonLambdaToApplyValueRule :: TypedValue -> Data.Apply TypedValue -> State Origin Rule
nonLambdaToApplyValueRule applyTv (Data.Apply func arg) =
  Rule [tvVal func, tvVal arg] .
  NonLambdaToApplyValueClosure (tvVal applyTv) <$> mkOrigin

runApplyArgToFuncArgClosure :: Ref -> RuleFunction
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
  TypedValue -> Data.Apply TypedValue -> Rule
applyArgToFuncArgRule applyTv (Data.Apply func arg) =
  Rule [tvVal applyTv, tvVal func] $ ApplyArgToFuncArgClosure (tvVal arg)

applyRules :: TypedValue -> Data.Apply TypedValue -> State Origin [Rule]
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
