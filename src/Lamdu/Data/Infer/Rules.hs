{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer.Rules
  ( Rule(..)
  , makeForAll, makeForNode
  , union
  , runClosure
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens ((^.), (^..))
import Control.Monad (guard)
import Control.Monad.Trans.State (State)
import Control.Monad.Trans.Writer (execWriter)
import Control.Monad.Unit (Unit(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Functor.Identity (Identity(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse, sequenceA)
import Lamdu.Data.Infer.Types
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntSet as IntSet
import qualified Data.Monoid as Monoid
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expression

type RuleFunction def = [RefExpression def] -> [(ExprRef, RefExpression def)]

type Origin2 = (Origin, Origin)
type Origin3 = (Origin, Origin, Origin)
mkOrigin2 :: State Origin Origin2
mkOrigin2 = (,) <$> mkOrigin <*> mkOrigin
mkOrigin3 :: State Origin Origin3
mkOrigin3 = (,,) <$> mkOrigin <*> mkOrigin <*> mkOrigin

-- Boilerplate to work around lack of serialization of functions
-- Represents a serialization of RuleFunction:
data RuleClosure def
  = LambdaBodyTypeToPiResultTypeClosure (Guid, ExprRef) Origin2
  | PiToLambdaClosure (Guid, ExprRef, ExprRef) Origin3
  | CopyClosure ExprRef
  | LambdaParentToChildrenClosure (Expression.LambdaWrapper, ExprRef, ExprRef)
  | LambdaChildrenToParentClosure (Expression.LambdaWrapper, Guid, ExprRef) Origin
  | SetClosure [(ExprRef, RefExpression def)]
  | SimpleTypeClosure ExprRef Origin2
  | IntoApplyResultClosure (Expression.LambdaWrapper, ExprRef, ExprRef)
  | IntoArgClosure (Expression.LambdaWrapper, ExprRef)
  | IntoFuncResultTypeClosure (Expression.LambdaWrapper, ExprRef)
  | ArgTypeToPiParamTypeClosure ExprRef Origin2
  | RigidArgApplyTypeToResultTypeClosure ExprRef Origin3
  | RedexApplyTypeToResultTypeClosure ExprRef
  | PiParamTypeToArgTypeClosure ExprRef
  | LambdaParamTypeToArgTypeClosure ExprRef
  | ArgTypeToLambdaParamTypeClosure ExprRef Origin
  | NonLambdaToApplyValueClosure ExprRef Origin
  | ApplyToPartsClosure (Expression.Apply ExprRef)
derive makeBinary ''RuleClosure

data Rule def = Rule
  { ruleInputs :: [ExprRef]
  , _ruleCompute :: RuleClosure def
  }
derive makeBinary ''Rule

runClosure :: Eq def => RuleClosure def -> RuleFunction def
runClosure closure =
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
  RedexApplyTypeToResultTypeClosure x ->
    runRedexApplyTypeToResultTypeClosure x
  PiParamTypeToArgTypeClosure x ->
    runPiParamTypeToArgTypeClosure x
  LambdaParamTypeToArgTypeClosure x ->
    runLambdaParamTypeToArgTypeClosure x
  ArgTypeToLambdaParamTypeClosure x o ->
    runArgTypeToLambdaParamTypeClosure x o
  NonLambdaToApplyValueClosure x o ->
    runNonLambdaToApplyValueClosure x o
  ApplyToPartsClosure x ->
    runApplyToPartsClosure x

makeForNode :: Expression.Expression def TypedValue -> State Origin [Rule def]
makeForNode (Expression.Expression exprBody typedVal) =
  (:)
  <$> ruleSimpleType typedVal
  <*>
  case fmap (Lens.view Expression.ePayload) exprBody of
  Expression.BodyPi lambda@(Expression.Lambda _ _ resultType) ->
    (:) <$> setRule (tvType resultType) <*>
    onLambda Expression.LambdaWrapperPi lambda
  Expression.BodyLambda lambda@(Expression.Lambda param _ body) ->
    (++) <$> lambdaRules param typedVal (tvType body) <*>
    onLambda Expression.LambdaWrapperLambda lambda
  Expression.BodyApply apply -> applyRules typedVal apply
  _ -> pure []
  where
    setRule ref = do
      o <- mkOrigin
      return . Rule [] $ SetClosure [(ref, setExpr o)]
    onLambda cons lam@(Expression.Lambda _ paramType _) =
      (:) <$> setRule (tvType paramType) <*>
      lambdaStructureRules cons (tvVal typedVal) (fmap tvVal lam)

makeForAll :: Expression.Expression def TypedValue -> State Origin [Rule def]
makeForAll = fmap concat . traverse makeForNode . Expression.subExpressions

makeHole :: Origin -> RefExpression def
makeHole g = makeRefExpr g $ Expression.BodyLeaf Expression.Hole

setExpr :: Origin -> RefExpression def
setExpr g = makeRefExpr g $ Expression.BodyLeaf Expression.Set

intTypeExpr :: Origin -> RefExpression def
intTypeExpr g = makeRefExpr g $ Expression.BodyLeaf Expression.IntegerType

guidFromOrigin :: Origin -> Guid
guidFromOrigin origin = Guid.fromString $ show origin ++ "(orig)"

makePi :: Origin -> RefExpression def -> RefExpression def -> RefExpression def
makePi o paramType result =
  makeRefExpr o $ Expression.makePi (guidFromOrigin o) paramType result

runLambdaBodyTypeToPiResultTypeClosure :: (Guid, ExprRef) -> Origin2 -> RuleFunction def
runLambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) (o0, o1) ~[bodyTypeExpr] =
  [( lambdaTypeRef
   , makeRefExpr o0 $ Expression.makePi param (makeHole o1) bodyTypeExpr
   )]

runPiToLambdaClosure :: (Guid, ExprRef, ExprRef) -> Origin3 -> RuleFunction def
runPiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) (o0, o1, o2) ~[piBody] = do
  Expression.Lambda piParam paramType resultType <- piBody ^.. Expression.eBody . Expression.bodyPi
  [ -- Pi result type -> Body type
    ( bodyTypeRef
    , subst piParam
      ( makeRefExpr o0
        (Expression.makeParameterRef param)
      )
      resultType
    )
    , -- Pi param type -> Lambda param type
      ( lambdaValueRef
      , makeRefExpr o1 . Expression.makeLambda piParam paramType $ makeHole o2
      )
    ]

lambdaRules :: Guid -> TypedValue -> ExprRef -> State Origin [Rule def]
lambdaRules param (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  sequenceA
  [ Rule [bodyTypeRef] . LambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) <$> mkOrigin2
  , Rule [lambdaTypeRef] . PiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) <$> mkOrigin3
  ]

runCopyClosure :: ExprRef -> RuleFunction def
runCopyClosure dest ~[srcExpr] = [(dest, srcExpr)]

union :: ExprRef -> ExprRef -> [Rule def]
union x y =
  [ Rule [x] $ CopyClosure y
  , Rule [y] $ CopyClosure x
  ]

-- Parent lambda to children
runLambdaParentToChildrenClosure :: (Expression.LambdaWrapper, ExprRef, ExprRef) -> RuleFunction def
runLambdaParentToChildrenClosure (cons, paramTypeRef, resultRef) ~[expr] = do
  Expression.Lambda _ paramTypeE resultE <- expr ^.. Expression.eBody . Expression.lambdaWrapperPrism cons
  [(paramTypeRef, paramTypeE), (resultRef, resultE)]

-- Children of lambda to lambda parent
runLambdaChildrenToParentClosure :: (Expression.LambdaWrapper, Guid, ExprRef) -> Origin -> RuleFunction def
runLambdaChildrenToParentClosure (cons, param, lamRef) o0 ~[paramTypeExpr, resultExpr] =
  [( lamRef
   , makeRefExpr o0 . Lens.review (Expression.lambdaWrapperPrism cons) $
     Expression.Lambda param paramTypeExpr resultExpr
   )]

lambdaStructureRules :: Expression.LambdaWrapper -> ExprRef -> Expression.Lambda ExprRef -> State Origin [Rule def]
lambdaStructureRules cons lamRef (Expression.Lambda param paramTypeRef resultRef) =
  sequenceA
  [ pure . Rule [lamRef] $
    LambdaParentToChildrenClosure (cons, paramTypeRef, resultRef)
  , -- Copy the structure from the children to the parent
    Rule [paramTypeRef, resultRef] .
    LambdaChildrenToParentClosure (cons, param, lamRef) <$> mkOrigin
  ]

runSetClosure :: [(ExprRef, RefExpression def)] -> RuleFunction def
runSetClosure outputs ~[] = outputs

subst ::
  Guid -> Expression.Expression def a ->
  Expression.Expression def a -> Expression.Expression def a
subst from to expr
  | Lens.anyOf
    (Expression.eBody . Expression.bodyLeaf . Expression.getVariable . Expression.parameterRef)
    (== from) expr
  = to
  | otherwise = Lens.over (Expression.eBody . Lens.mapped) (subst from to) expr

mergeToPiResult ::
  Eq def => RefExpression def -> RefExpression def -> RefExpression def
mergeToPiResult =
  fmap runIdentity .
  Expression.matchExpression onMatch ((fmap . fmap) return onMismatch)
  where
    onMatch x _ = return x
    onMismatch dest src
      -- TODO: This seems like it should report an error,
      -- verify/document that it is OK because the other direction of
      -- information flow will catch any error:
      | notAHole dest = dest
      | not (IntSet.null substs) =
        Lens.set rplSubstitutedArgs substs <$> src
      | notAHole src =
        Lens.set rplRestrictedPoly (Monoid.Any True) <$> dest
      | otherwise = dest
      where
        substs = dest ^. Expression.ePayload . rplSubstitutedArgs
    notAHole = Lens.nullOf (Expression.eBody . Expression.bodyLeaf . Expression.hole)

runSimpleTypeClosure :: ExprRef -> Origin2 -> RuleFunction def
runSimpleTypeClosure typ (o0, o1) ~[valExpr] =
  case valExpr ^. Expression.eBody of
  Expression.BodyLeaf Expression.Set -> [(typ, setExpr o0)]
  Expression.BodyLeaf Expression.IntegerType -> [(typ, setExpr o0)]
  Expression.BodyLeaf (Expression.LiteralInteger _) -> [(typ, intTypeExpr o0)]
  Expression.BodyPi _ -> [(typ, setExpr o0)]
  Expression.BodyLambda (Expression.Lambda param paramType _) ->
    [( typ
     , makeRefExpr o0 . Expression.makePi param paramType $
       makeHole o1
     )]
  _ -> []

ruleSimpleType :: TypedValue -> State Origin (Rule def)
ruleSimpleType (TypedValue val typ) = Rule [val] . SimpleTypeClosure typ <$> mkOrigin2

runIntoApplyResultClosure :: (Expression.LambdaWrapper, ExprRef, ExprRef) -> RuleFunction def
runIntoApplyResultClosure (cons, applyRef, arg) ~[funcExpr, argExpr] = do
  Expression.Lambda param _ result <- funcExpr ^.. Expression.eBody . Expression.lambdaWrapperPrism cons
  return
    ( applyRef
    , subst param
      ((Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.insert) (unExprRef arg) argExpr) .
      -- TODO: Is this correct?
      Lens.set (Lens.mapped . rplRestrictedPoly) (Monoid.Any False) $
      result
    )

intoApplyResultRule :: Expression.LambdaWrapper -> ExprRef -> ExprRef -> ExprRef -> Rule def
intoApplyResultRule cons applyRef func arg =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  Rule [func, arg] $ IntoApplyResultClosure (cons, applyRef, arg)

runIntoArgClosure :: Eq def => (Expression.LambdaWrapper, ExprRef) -> RuleFunction def
runIntoArgClosure (cons, arg) ~[applyExpr, funcExpr] = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Expression.Lambda param _ result <- funcExpr ^.. Expression.eBody . Expression.lambdaWrapperPrism cons
  mergeToArg param arg result applyExpr

intoArgRule :: Expression.LambdaWrapper -> ExprRef -> ExprRef -> ExprRef -> Rule def
intoArgRule cons applyRef func arg =
  Rule [applyRef, func] $ IntoArgClosure (cons, arg)

runIntoFuncResultTypeClosure :: Eq def => (Expression.LambdaWrapper, ExprRef) -> RuleFunction def
runIntoFuncResultTypeClosure (cons, func) ~[applyExpr, Expression.Expression funcBody funcPl] = do
  Expression.Lambda param paramT result <- funcBody ^.. Expression.lambdaWrapperPrism cons
  return
    ( func
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      Lens.review (Expression.lambdaWrapperPrism cons) . Expression.Lambda param paramT $
      mergeToPiResult result applyExpr
    )

intoFuncResultTypeRule :: Expression.LambdaWrapper -> ExprRef -> ExprRef -> Rule def
intoFuncResultTypeRule cons applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  Rule [applyRef, func] $ IntoFuncResultTypeClosure (cons, func)

recurseSubstRules :: Expression.LambdaWrapper -> ExprRef -> ExprRef -> ExprRef -> [Rule def]
recurseSubstRules cons applyRef func arg =
  [ intoApplyResultRule cons applyRef func arg
  , intoArgRule cons applyRef func arg
  , intoFuncResultTypeRule cons applyRef func
  ]

-- param, (dest)argRef, func result, applyExpr
mergeToArg :: Eq def => Guid -> ExprRef -> RefExpression def -> RefExpression def -> [(ExprRef, RefExpression def)]
mergeToArg param arg =
  (fmap . fmap) (execWriter . Compose.unO) $
  Expression.matchExpression onMatch onMismatch
  where
    unit = Compose.O (pure Unit)
    onMatch _ _ = unit
    onMismatch expr post
      | Lens.anyOf
        (Expression.eBody . Expression.bodyLeaf . Expression.getVariable . Expression.parameterRef)
        (== param) expr
      = Compose.O . (fmap . const) Unit $ Writer.tell
        [( arg
         , (Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.delete) (unExprRef arg) .
           -- TODO: Is this correct?
           Lens.set (Lens.mapped . rplRestrictedPoly) (Monoid.Any False) $
           post
         )]
      | otherwise = unit

runArgTypeToPiParamTypeClosure :: ExprRef -> Origin2 -> RuleFunction def
runArgTypeToPiParamTypeClosure funcTypeRef (o0, o1) ~[argTypeExpr] =
  [( funcTypeRef
   , makePi o0 argTypeExpr $ makeHole o1
   )]

argTypeToPiParamTypeRule :: Expression.Apply TypedValue -> State Origin (Rule def)
argTypeToPiParamTypeRule (Expression.Apply func arg) =
  -- ArgT => Pi ParamT
  Rule [tvType arg] . ArgTypeToPiParamTypeClosure (tvType func) <$> mkOrigin2

runRigidArgApplyTypeToResultTypeClosure :: ExprRef -> Origin3 -> RuleFunction def
runRigidArgApplyTypeToResultTypeClosure funcTypeRef (o0, o1, o2) ~[applyTypeExpr, argExpr] = do
  par <-
    argExpr ^..
    Expression.eBody . Expression.bodyLeaf . Expression.getVariable . Expression.parameterRef
  return
    ( funcTypeRef
    , makePi o0 (makeHole o1) $
      subst par (makeHole o2) applyTypeExpr
    )

rigidArgApplyTypeToResultTypeRule ::
  TypedValue -> Expression.Apply TypedValue -> State Origin (Rule def)
rigidArgApplyTypeToResultTypeRule applyTv (Expression.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  Rule [tvType applyTv, tvVal arg] .
  RigidArgApplyTypeToResultTypeClosure (tvType func) <$> mkOrigin3

runRedexApplyTypeToResultTypeClosure :: ExprRef -> RuleFunction def
runRedexApplyTypeToResultTypeClosure funcTypeRef
  ~[applyTypeExpr, Expression.Expression funcExpr funcPl] = do
  Expression.Lambda paramGuid paramType _ <- funcExpr ^.. Expression.bodyLambda
  return
    ( funcTypeRef
    , makeRefExpr (Lens.view rplOrigin funcPl) $
      Expression.makePi paramGuid paramType applyTypeExpr
    )

redexApplyTypeToResultTypeRule :: TypedValue -> TypedValue -> Rule def
redexApplyTypeToResultTypeRule applyTv funcTv =
  Rule [tvType applyTv, tvVal funcTv] $
  RedexApplyTypeToResultTypeClosure (tvType funcTv)

runPiParamTypeToArgTypeClosure :: ExprRef -> RuleFunction def
runPiParamTypeToArgTypeClosure argTypeRef ~[Expression.Expression funcTExpr _] = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Expression.Lambda _ paramT _ <- funcTExpr ^.. Expression.bodyPi
  return (argTypeRef, paramT)

piParamTypeToArgTypeRule :: Expression.Apply TypedValue -> Rule def
piParamTypeToArgTypeRule (Expression.Apply func arg) =
  Rule [tvType func] $ PiParamTypeToArgTypeClosure (tvType arg)

runLambdaParamTypeToArgTypeClosure :: ExprRef -> RuleFunction def
runLambdaParamTypeToArgTypeClosure argTypeRef ~[Expression.Expression funcExpr _] = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Expression.Lambda _ paramT _ <- funcExpr ^.. Expression.bodyLambda
  return (argTypeRef, paramT)

lambdaParamTypeToArgTypeRule :: Expression.Apply TypedValue -> Rule def
lambdaParamTypeToArgTypeRule (Expression.Apply func arg) =
  Rule [tvVal func] $ LambdaParamTypeToArgTypeClosure (tvType arg)

runArgTypeToLambdaParamTypeClosure :: ExprRef -> Origin -> RuleFunction def
runArgTypeToLambdaParamTypeClosure funcValRef o0 ~[Expression.Expression funcExpr funcPl, argTExpr] = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Expression.Lambda param _ _ <- funcExpr ^.. Expression.bodyLambda
  return
    ( funcValRef
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      Expression.makeLambda param argTExpr $ makeHole o0
    )

argTypeToLambdaParamTypeRule :: Expression.Apply TypedValue -> State Origin (Rule def)
argTypeToLambdaParamTypeRule (Expression.Apply func arg) =
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
  case funcExpr ^. Expression.eBody of
  Expression.BodyLambda _ -> []
  Expression.BodyLeaf Expression.Hole -> []
  _ ->
    [ ( applyValRef
      , makeRefExpr o0 $ Expression.makeApply funcExpr argExpr
      )
    ]

nonLambdaToApplyValueRule :: TypedValue -> Expression.Apply TypedValue -> State Origin (Rule def)
nonLambdaToApplyValueRule applyTv (Expression.Apply func arg) =
  Rule [tvVal func, tvVal arg] .
  NonLambdaToApplyValueClosure (tvVal applyTv) <$> mkOrigin

runApplyToPartsClosure :: Eq def => Expression.Apply ExprRef -> RuleFunction def
runApplyToPartsClosure refs ~[applyExpr, funcExpr] = do
  -- If definitely not a redex (func also not a hole)
  -- Apply-Arg => Arg
  -- Apply-Func => Func
  guard $ Lens.nullOf (Expression.eBody . Expression.bodyLambda) funcExpr
  guard $ Lens.nullOf (Expression.eBody . Expression.bodyLeaf . Expression.hole) funcExpr
  Expression.Apply aFunc aArg <- applyExpr ^.. Expression.eBody . Expression.bodyApply
  [(refs ^. Expression.applyFunc, aFunc), (refs ^. Expression.applyArg, aArg)]

applyToPartsRule ::
  TypedValue -> Expression.Apply TypedValue -> Rule def
applyToPartsRule applyTv parts@(Expression.Apply func _) =
  Rule [tvVal applyTv, tvVal func] . ApplyToPartsClosure $ tvVal <$> parts

applyRules :: TypedValue -> Expression.Apply TypedValue -> State Origin [Rule def]
applyRules applyTv apply@(Expression.Apply func arg) =
  -- TODO: make all of these functions have a standard signature and
  -- just apply them all to the same args?
  (++ pureRules) <$>
  sequenceA
  [ argTypeToPiParamTypeRule apply
  , rigidArgApplyTypeToResultTypeRule applyTv apply
  , argTypeToLambdaParamTypeRule apply
  , nonLambdaToApplyValueRule applyTv apply
  ]
  where
    pureRules =
      [ piParamTypeToArgTypeRule apply
      , redexApplyTypeToResultTypeRule applyTv func
      , lambdaParamTypeToArgTypeRule apply
      , applyToPartsRule applyTv apply
      ]
      ++ recurseSubstRules Expression.LambdaWrapperPi
        (tvType applyTv) (tvType func) (tvVal arg)
      ++ recurseSubstRules Expression.LambdaWrapperLambda
        (tvVal applyTv) (tvVal func) (tvVal arg)
