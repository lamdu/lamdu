{-# LANGUAGE TemplateHaskell, PatternGuards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Data.Expression.Infer.Rules
  ( Rule(..)
  , makeForAll, makeForNode
  , union
  , runRule
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(..))
import Control.Lens ((^.), (^..), (&), (%~), (.~))
import Control.Monad (guard)
import Control.Monad.Trans.State (State)
import Control.Monad.Trans.Writer (execWriter)
import Control.Monad.Unit (Unit(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Data.Traversable (traverse, sequenceA)
import Lamdu.Data.Expression.Infer.Types
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntSet as IntSet
import qualified Data.List.Assoc as AssocList
import qualified Data.Monoid as Monoid
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type RuleResult def = [(ExprRef, RefExpression def)]

type Origin2 = (Origin, Origin)
type Origin3 = (Origin, Origin, Origin)
mkOrigin2 :: State Origin Origin2
mkOrigin2 = (,) <$> mkOrigin <*> mkOrigin
mkOrigin3 :: State Origin Origin3
mkOrigin3 = (,,) <$> mkOrigin <*> mkOrigin <*> mkOrigin

type RefExpression2 def = (RefExpression def, RefExpression def)
type RefExpression3 def = (RefExpression def, RefExpression def, RefExpression def)

-- Boilerplate to work around lack of serialization of functions
-- Represents a serialization of RuleFunction:
data Rule def a
  = LambdaBodyTypeToPiResultType (Guid, ExprRef) a Origin2
  | PiToLambda (Guid, ExprRef, ExprRef) a Origin3
  | RecordValToType ([Expression.FieldTag], ExprRef) [a] Origin
  | RecordTypeToFieldTypes [(Expression.FieldTag, ExprRef)] a
  | RecordTypeToGetFieldType ExprRef (a, a)
  | GetFieldTypeToRecordFieldType ExprRef (a, a, a) Origin2
  | Copy ExprRef a
  | LambdaParentToChildren (Expression.Lambda ExprRef) a
  | LambdaChildrenToParent (Expression.Kind, Guid, ExprRef) (a, a) Origin
  | RecordParentToChildren (Expression.Record ExprRef) a
  | RecordChildrenToParent (Expression.Kind, [Expression.FieldTag], ExprRef) [a] Origin
  | SetRule [(ExprRef, RefExpression def)]
  | SimpleType ExprRef a Origin2
  | IntoApplyResult (Expression.Kind, ExprRef, ExprRef) (a, a)
  | IntoArg (Expression.Kind, ExprRef) (a, a)
  | IntoFuncResultType (Expression.Kind, ExprRef) (a, a)
  | ArgTypeToPiParamType ExprRef a Origin2
  | RigidArgApplyTypeToResultType ExprRef (a, a) Origin3
  | RedexApplyTypeToResultType ExprRef (a, a)
  | PiParamTypeToArgType ExprRef a
  | LambdaParamTypeToArgType ExprRef a
  | ArgTypeToLambdaParamType ExprRef (a, a) Origin
  | NonLambdaToApplyValue ExprRef (a, a) Origin
  | ApplyToParts (Expression.Apply ExprRef) (a, a)
  deriving (Functor, Foldable, Traversable)

derive makeBinary ''Rule
derive makeNFData ''Rule

runRule :: Eq def => Rule def (RefExpression def) -> RuleResult def
runRule rule =
  case rule of
  LambdaBodyTypeToPiResultType x e o ->
    runLambdaBodyTypeToPiResultType x e o
  PiToLambda x e o ->
    runPiToLambda x e o
  RecordValToType x e o ->
    runRecordValToType x e o
  RecordTypeToFieldTypes x e ->
    runRecordTypeToFieldTypes x e
  RecordTypeToGetFieldType x e ->
    runRecordTypeToGetFieldType x e
  GetFieldTypeToRecordFieldType x e o ->
    runGetFieldTypeToRecordFieldType x e o
  Copy x e ->
    runCopy x e
  LambdaParentToChildren x e ->
    runLambdaParentToChildren x e
  LambdaChildrenToParent x e o ->
    runLambdaChildrenToParent x e o
  RecordParentToChildren x e ->
    runRecordParentToChildren x e
  RecordChildrenToParent x e o ->
    runRecordChildrenToParent x e o
  SetRule x ->
    runSetRule x
  SimpleType x e o ->
    runSimpleType x e o
  IntoApplyResult x e ->
    runIntoApplyResult x e
  IntoArg x e ->
    runIntoArg x e
  IntoFuncResultType x e ->
    runIntoFuncResultType x e
  ArgTypeToPiParamType x e o ->
    runArgTypeToPiParamType x e o
  RigidArgApplyTypeToResultType x e o ->
    runRigidArgApplyTypeToResultType x e o
  RedexApplyTypeToResultType x e ->
    runRedexApplyTypeToResultType x e
  PiParamTypeToArgType x e ->
    runPiParamTypeToArgType x e
  LambdaParamTypeToArgType x e ->
    runLambdaParamTypeToArgType x e
  ArgTypeToLambdaParamType x e o ->
    runArgTypeToLambdaParamType x e o
  NonLambdaToApplyValue x e o ->
    runNonLambdaToApplyValue x e o
  ApplyToParts x e ->
    runApplyToParts x e

makeForNode :: Expression.Expression def TypedValue -> State Origin [Rule def ExprRef]
makeForNode (Expression.Expression exprBody typedVal) =
  (:)
  <$> ruleSimpleType typedVal
  <*>
  case Lens.view Expression.ePayload <$> exprBody of
  Expression.BodyLam lambda ->
    (++) <$> lamKindRules lambda <*> onLambda lambda
  Expression.BodyApply apply -> applyRules typedVal apply
  Expression.BodyRecord record ->
    (++)
    <$> recordKindRules record
    <*> recordStructureRules (tvVal typedVal) (fmap tvVal record)
  Expression.BodyGetField getField ->
    getFieldRules typedVal . tvType $ getField ^. Expression.getFieldRecord
    -- TODO: GetField Structure rules
  -- Leafs need no additional rules beyond the commonal simpleTypeRule
  Expression.BodyLeaf _ -> pure []
  where
    recordKindRules (Expression.Record Expression.Type fields) =
      mapM (setRule . tvType . snd) fields
    recordKindRules (Expression.Record Expression.Val fields) =
      recordValueRules (tvType typedVal) $ fields & Lens.mapped . Lens._2 %~ tvType
    lamKindRules (Expression.Lambda Expression.Type _ _ body) =
      fmap (:[]) . setRule $ tvType body
    lamKindRules (Expression.Lambda Expression.Val param _ body) =
      lambdaRules param typedVal (tvType body)
    onLambda lam =
      (:) <$> setRule (tvType (lam ^. Expression.lambdaParamType)) <*>
      lambdaStructureRules (tvVal typedVal) (fmap tvVal lam)
    setRule ref = do
      o <- mkOrigin
      return $ SetRule [(ref, setExpr o)]

makeForAll :: Expression.Expression def TypedValue -> State Origin [Rule def ExprRef]
makeForAll = fmap concat . traverse makeForNode . ExprUtil.subExpressions

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
  makeRefExpr o $ ExprUtil.makePi (guidFromOrigin o) paramType result

runLambdaBodyTypeToPiResultType :: (Guid, ExprRef) -> RefExpression def -> Origin2 -> RuleResult def
runLambdaBodyTypeToPiResultType (param, lambdaTypeRef) bodyTypeExpr (o0, o1) =
  [( lambdaTypeRef
   , makeRefExpr o0 $ ExprUtil.makePi param (makeHole o1) bodyTypeExpr
   )]

runPiToLambda :: (Guid, ExprRef, ExprRef) -> RefExpression def -> Origin3 -> RuleResult def
runPiToLambda (param, lambdaValueRef, bodyTypeRef) piBody (o0, o1, o2) = do
  Expression.Lambda Expression.Type piParam paramType resultType <-
    piBody ^.. Expression.eBody . Expression._BodyLam
  [ -- Pi result type -> Body type
    ( bodyTypeRef
    , subst piParam
      ( makeRefExpr o0
        (Lens.review ExprUtil.bodyParameterRef param)
      )
      resultType
    )
    , -- Pi param type -> Lambda param type
      ( lambdaValueRef
      , makeRefExpr o1 . ExprUtil.makeLambda piParam paramType $ makeHole o2
      )
    ]

lambdaRules :: Guid -> TypedValue -> ExprRef -> State Origin [Rule def ExprRef]
lambdaRules param (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  sequenceA
  [ LambdaBodyTypeToPiResultType (param, lambdaTypeRef) bodyTypeRef <$> mkOrigin2
  , PiToLambda (param, lambdaValueRef, bodyTypeRef) lambdaTypeRef <$> mkOrigin3
  ]

runRecordValToType :: ([Expression.FieldTag], ExprRef) -> [RefExpression def] -> Origin -> RuleResult def
runRecordValToType (fields, recordTypeRef) fieldTypeExprs o0 =
  [ ( recordTypeRef
    , makeRefExpr o0 . Expression.BodyRecord . Expression.Record Expression.Type $
      zip fields fieldTypeExprs
    )
  ]

runRecordTypeToFieldTypes :: [(Expression.FieldTag, ExprRef)] -> RefExpression def -> RuleResult def
runRecordTypeToFieldTypes fieldTypeRefs recordTypeExpr = do
  Expression.Record _ fieldTypeExprs <-
    recordTypeExpr ^.. Expression.eBody . Expression._BodyRecord
  maybe [] (map snd) $ AssocList.match (,) fieldTypeRefs fieldTypeExprs

getFieldTag :: Lens.Traversal' (Expression.Expression def a) Expression.FieldTag
getFieldTag = Expression.eBody . Expression._BodyGetField . Expression.getFieldTag

runRecordTypeToGetFieldType :: ExprRef -> RefExpression2 def -> RuleResult def
runRecordTypeToGetFieldType getFieldTypeRef (recordTypeExpr, getFieldValExpr) = do
  fieldTag <- getFieldValExpr ^.. getFieldTag
  case recordTypeExpr ^. Expression.eBody of
    Expression.BodyRecord (Expression.Record Expression.Type fields)
      | Just fieldType <- lookup fieldTag fields
        -> [(getFieldTypeRef, fieldType)]
    _ -> []

runGetFieldTypeToRecordFieldType :: ExprRef -> RefExpression3 def -> Origin2 -> RuleResult def
runGetFieldTypeToRecordFieldType recordTypeRef (recordTypeExpr, getFieldValExpr, getFieldTypeExpr) (o0, o1) = do
  fieldTag <- getFieldValExpr ^.. getFieldTag
  let
    recordTypeExample =
      makeRefExpr o0 . Expression.BodyRecord . Expression.Record Expression.Type $
      [ (fieldTag, makeRefExpr o1 (Expression.BodyLeaf Expression.Hole)) ]
    makeError = [(recordTypeRef, recordTypeExample)]
  case recordTypeExpr ^. Expression.eBody of
    Expression.BodyLeaf Expression.Hole -> []
    Expression.BodyRecord (Expression.Record Expression.Type fields)
      | Lens.notNullOf Expression._FieldTag fieldTag &&
        any ((fieldTag ==) . fst) fields ->
        [ ( recordTypeRef
          , recordTypeExpr &
            Expression.eBody . Expression._BodyRecord .
            Expression.recordFields . AssocList.at fieldTag .~
            getFieldTypeExpr
          )]
      | any (matchFieldTag fieldTag . fst) fields -> []
    _ -> makeError

matchFieldTag :: Expression.FieldTag -> Expression.FieldTag -> Bool
matchFieldTag Expression.FieldTagHole _ = True
matchFieldTag _ Expression.FieldTagHole = True
matchFieldTag (Expression.FieldTag x) (Expression.FieldTag y) = x == y

getFieldRules :: TypedValue -> ExprRef -> State Origin [Rule def ExprRef]
getFieldRules (TypedValue valRef typeRef) recordTypeRef =
  sequenceA
  [ pure $ RecordTypeToGetFieldType typeRef (recordTypeRef, valRef)
  , GetFieldTypeToRecordFieldType recordTypeRef (recordTypeRef, valRef, typeRef) <$> mkOrigin2
  ]

recordValueRules :: ExprRef -> [(Expression.FieldTag, ExprRef)] -> State Origin [Rule def ExprRef]
recordValueRules recTypeRef fieldTypeRefs =
  sequenceA
  [ RecordValToType (fields, recTypeRef) typeRefs <$> mkOrigin
  , pure $ RecordTypeToFieldTypes fieldTypeRefs recTypeRef
  ]
  where
    (fields, typeRefs) = unzip fieldTypeRefs

runCopy :: ExprRef -> RefExpression def -> RuleResult def
runCopy dest srcExpr = [(dest, srcExpr)]

union :: ExprRef -> ExprRef -> [Rule def ExprRef]
union x y =
  [ Copy y x
  , Copy x y
  ]

-- Parent lambda to children
runLambdaParentToChildren :: Expression.Lambda ExprRef -> RefExpression def -> RuleResult def
runLambdaParentToChildren (Expression.Lambda _ _ paramTypeRef resultRef) expr = do
  Expression.Lambda _ _ paramTypeE resultE <-
    expr ^.. Expression.eBody . Expression._BodyLam
  [(paramTypeRef, paramTypeE), (resultRef, resultE)]

-- Children of lambda to lambda parent
runLambdaChildrenToParent :: (Expression.Kind, Guid, ExprRef) -> RefExpression2 def -> Origin -> RuleResult def
runLambdaChildrenToParent (k, param, lamRef) (paramTypeExpr, resultExpr) o0 =
  [( lamRef
   , makeRefExpr o0 . Expression.BodyLam $
     Expression.Lambda k param paramTypeExpr resultExpr
   )]

lambdaStructureRules :: ExprRef -> Expression.Lambda ExprRef -> State Origin [Rule def ExprRef]
lambdaStructureRules lamRef lam@(Expression.Lambda k param paramTypeRef resultRef) =
  sequenceA
  [ pure $ LambdaParentToChildren lam lamRef
  , -- Copy the structure from the children to the parent
    LambdaChildrenToParent (k, param, lamRef) (paramTypeRef, resultRef) <$> mkOrigin
  ]

runRecordParentToChildren :: Expression.Record ExprRef -> RefExpression def -> RuleResult def
runRecordParentToChildren (Expression.Record _ fieldRefs) expr = do
  Expression.Record _ fieldExprs <-
    expr ^.. Expression.eBody . Expression._BodyRecord
  maybe [] (map snd) $ AssocList.match (,) fieldRefs fieldExprs

runRecordChildrenToParent ::
  (Expression.Kind, [Expression.FieldTag], ExprRef) -> [RefExpression def] -> Origin -> RuleResult def
runRecordChildrenToParent (k, fields, recRef) fieldExprs o0 =
  [( recRef
   , makeRefExpr o0 . Expression.BodyRecord .
     Expression.Record k $ zip fields fieldExprs
   )]

recordStructureRules :: ExprRef -> Expression.Record ExprRef -> State Origin [Rule def ExprRef]
recordStructureRules recRef rec@(Expression.Record k fields) =
  sequenceA
  [ pure $ RecordParentToChildren rec recRef
  , RecordChildrenToParent (k, keys, recRef) valRefs <$> mkOrigin
  ]
  where
    (keys, valRefs) = unzip fields

runSetRule :: [(ExprRef, RefExpression def)] -> RuleResult def
runSetRule outputs = outputs

subst ::
  Guid -> Expression.Expression def a ->
  Expression.Expression def a -> Expression.Expression def a
subst from to expr
  | Lens.anyOf
    (Expression.eBody . ExprUtil.bodyParameterRef)
    (== from) expr
  = to
  | otherwise = expr & Expression.eBody . Lens.traversed %~ subst from to

mergeToPiResult ::
  Eq def => RefExpression def -> RefExpression def -> RefExpression def
mergeToPiResult =
  fmap runIdentity .
  ExprUtil.matchExpression onMatch ((fmap . fmap) return onMismatch)
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
    notAHole = Lens.nullOf (Expression.eBody . Expression._BodyLeaf . Expression._Hole)

runSimpleType :: ExprRef -> RefExpression def -> Origin2 -> RuleResult def
runSimpleType typ valExpr (o0, o1) =
  case valExpr ^. Expression.eBody of
  Expression.BodyLeaf Expression.Set -> simpleType
  Expression.BodyLeaf Expression.IntegerType -> simpleType
  Expression.BodyLam (Expression.Lambda Expression.Type _ _ _) -> simpleType
  Expression.BodyRecord (Expression.Record Expression.Type _) -> simpleType
  Expression.BodyRecord (Expression.Record Expression.Val _) ->
    -- The rule to copy inferred types of fields to the inferred type
    -- of the whole record requiers dereferencing the inferred types
    -- of the field exprs which is impossible in this context. This is
    -- handled in the recordValueRules
    []
  Expression.BodyLeaf (Expression.LiteralInteger _) -> [(typ, intTypeExpr o0)]
  Expression.BodyLam
    (Expression.Lambda Expression.Val param paramType _) ->
    [( typ
     , makeRefExpr o0 . ExprUtil.makePi param paramType $
       makeHole o1
     )]
  -- All type information that can be deduced from these depends on
  -- external information which is not used in the simple type rules.
  Expression.BodyLeaf Expression.GetVariable {} -> []
  Expression.BodyLeaf Expression.Hole {} -> []
  Expression.BodyApply {} -> []
  Expression.BodyGetField {} -> []
  where
    simpleType = [(typ, setExpr o0)]

ruleSimpleType :: TypedValue -> State Origin (Rule def ExprRef)
ruleSimpleType (TypedValue val typ) = SimpleType typ val <$> mkOrigin2

runIntoApplyResult :: (Expression.Kind, ExprRef, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoApplyResult (k, applyRef, arg) (funcExpr, argExpr) = do
  Expression.Lambda bk param _ result <-
    funcExpr ^.. Expression.eBody . Expression._BodyLam
  guard $ k == bk
  return
    ( applyRef
    , subst param
      (argExpr & Lens.traversed . rplSubstitutedArgs %~ IntSet.insert (unExprRef arg)) .
      -- TODO: Is this correct?
      Lens.set (Lens.traversed . rplRestrictedPoly) (Monoid.Any False) $
      result
    )

intoApplyResultRule :: Expression.Kind -> ExprRef -> ExprRef -> ExprRef -> Rule def ExprRef
intoApplyResultRule k applyRef func arg =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  IntoApplyResult (k, applyRef, arg) (func, arg)

runIntoArg :: Eq def => (Expression.Kind, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoArg (k, arg) (applyExpr, funcExpr) = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Expression.Lambda bk param _ result <-
    funcExpr ^.. Expression.eBody . Expression._BodyLam
  guard $ bk == k
  mergeToArg param arg result applyExpr

intoArgRule :: Expression.Kind -> ExprRef -> ExprRef -> ExprRef -> Rule def ExprRef
intoArgRule k applyRef func arg =
  IntoArg (k, arg) (applyRef, func)

runIntoFuncResultType :: Eq def => (Expression.Kind, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoFuncResultType (k, func) (applyExpr, Expression.Expression funcBody funcPl) = do
  Expression.Lambda kb param paramT result <- funcBody ^.. Expression._BodyLam
  guard $ k == kb
  return
    ( func
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      Expression.BodyLam . Expression.Lambda k param paramT $
      mergeToPiResult result applyExpr
    )

intoFuncResultTypeRule :: Expression.Kind -> ExprRef -> ExprRef -> Rule def ExprRef
intoFuncResultTypeRule k applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  IntoFuncResultType (k, func) (applyRef, func)

recurseSubstRules :: Expression.Kind -> ExprRef -> ExprRef -> ExprRef -> [Rule def ExprRef]
recurseSubstRules k applyRef func arg =
  [ intoApplyResultRule k applyRef func arg
  , intoArgRule k applyRef func arg
  , intoFuncResultTypeRule k applyRef func
  ]

-- param, (dest)argRef, func result, applyExpr
mergeToArg :: Eq def => Guid -> ExprRef -> RefExpression def -> RefExpression def -> [(ExprRef, RefExpression def)]
mergeToArg param arg =
  (fmap . fmap) (execWriter . Compose.unO) $
  ExprUtil.matchExpression onMatch onMismatch
  where
    unit = Compose.O (pure Unit)
    onMatch _ _ = unit
    onMismatch expr post
      | Lens.anyOf
        (Expression.eBody . ExprUtil.bodyParameterRef)
        (== param) expr
      = Compose.O . (fmap . const) Unit $ Writer.tell
        [( arg
         , post
           & Lens.traversed . rplSubstitutedArgs %~ IntSet.delete (unExprRef arg)
           -- TODO: Is this correct?
           & Lens.traversed . rplRestrictedPoly .~ Monoid.Any False
         )]
      | otherwise = unit

runArgTypeToPiParamType :: ExprRef -> RefExpression def -> Origin2 -> RuleResult def
runArgTypeToPiParamType funcTypeRef argTypeExpr (o0, o1) =
  [( funcTypeRef
   , makePi o0 argTypeExpr $ makeHole o1
   )]

argTypeToPiParamTypeRule :: Expression.Apply TypedValue -> State Origin (Rule def ExprRef)
argTypeToPiParamTypeRule (Expression.Apply func arg) =
  -- ArgT => Pi ParamT
  ArgTypeToPiParamType (tvType func) (tvType arg) <$> mkOrigin2

runRigidArgApplyTypeToResultType :: ExprRef -> RefExpression2 def -> Origin3 -> RuleResult def
runRigidArgApplyTypeToResultType funcTypeRef (applyTypeExpr, argExpr) (o0, o1, o2) = do
  par <-
    argExpr ^..
    Expression.eBody . Expression._BodyLeaf . Expression._GetVariable . Expression._ParameterRef
  return
    ( funcTypeRef
    , makePi o0 (makeHole o1) $
      subst par (makeHole o2) applyTypeExpr
    )

rigidArgApplyTypeToResultTypeRule ::
  TypedValue -> Expression.Apply TypedValue -> State Origin (Rule def ExprRef)
rigidArgApplyTypeToResultTypeRule applyTv (Expression.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  RigidArgApplyTypeToResultType (tvType func) (tvType applyTv, tvVal arg) <$> mkOrigin3

runRedexApplyTypeToResultType :: ExprRef -> RefExpression2 def -> RuleResult def
runRedexApplyTypeToResultType funcTypeRef
  (applyTypeExpr, Expression.Expression funcExpr funcPl) = do
  Expression.Lambda Expression.Val paramGuid paramType _ <-
    funcExpr ^.. Expression._BodyLam
  return
    ( funcTypeRef
    , makeRefExpr (Lens.view rplOrigin funcPl) $
      ExprUtil.makePi paramGuid paramType applyTypeExpr
    )

redexApplyTypeToResultTypeRule :: TypedValue -> TypedValue -> Rule def ExprRef
redexApplyTypeToResultTypeRule applyTv funcTv =
  RedexApplyTypeToResultType (tvType funcTv) (tvType applyTv, tvVal funcTv)

runPiParamTypeToArgType :: ExprRef -> RefExpression def -> RuleResult def
runPiParamTypeToArgType argTypeRef (Expression.Expression funcTExpr _) = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Expression.Lambda Expression.Type _ paramT _ <- funcTExpr ^.. Expression._BodyLam
  return (argTypeRef, paramT)

piParamTypeToArgTypeRule :: Expression.Apply TypedValue -> Rule def ExprRef
piParamTypeToArgTypeRule (Expression.Apply func arg) =
  PiParamTypeToArgType (tvType arg) (tvType func)

runLambdaParamTypeToArgType :: ExprRef -> RefExpression def -> RuleResult def
runLambdaParamTypeToArgType argTypeRef (Expression.Expression funcExpr _) = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Expression.Lambda Expression.Val _ paramT _ <-
    funcExpr ^.. Expression._BodyLam
  return (argTypeRef, paramT)

lambdaParamTypeToArgTypeRule :: Expression.Apply TypedValue -> Rule def ExprRef
lambdaParamTypeToArgTypeRule (Expression.Apply func arg) =
  LambdaParamTypeToArgType (tvType arg) (tvVal func)

runArgTypeToLambdaParamType :: ExprRef -> RefExpression2 def -> Origin -> RuleResult def
runArgTypeToLambdaParamType funcValRef (Expression.Expression funcExpr funcPl, argTExpr) o0 = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Expression.Lambda Expression.Val param _ _ <-
    funcExpr ^.. Expression._BodyLam
  return
    ( funcValRef
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      ExprUtil.makeLambda param argTExpr $ makeHole o0
    )

argTypeToLambdaParamTypeRule :: Expression.Apply TypedValue -> State Origin (Rule def ExprRef)
argTypeToLambdaParamTypeRule (Expression.Apply func arg) =
  ArgTypeToLambdaParamType (tvVal func) (tvVal func, tvType arg) <$> mkOrigin

runNonLambdaToApplyValue :: ExprRef -> RefExpression2 def -> Origin -> RuleResult def
runNonLambdaToApplyValue applyValRef (funcExpr, argExpr) o0 =
  -- If func is surely not a lambda (a hole too could be a lambda).
  --
  -- Applies have a special case in the inferred value handling for
  -- redexes, and this rule is about detecting that an application is
  -- *not* a redex, so we can safely set the inferred value to the
  -- application itself.
  --
  -- Func Arg => Outer
  case funcExpr ^. Expression.eBody of
  Expression.BodyLam (Expression.Lambda Expression.Val _ _ _) -> []
  Expression.BodyLeaf Expression.Hole -> []
  _ ->
    [ ( applyValRef
      , makeRefExpr o0 $ ExprUtil.makeApply funcExpr argExpr
      )
    ]

nonLambdaToApplyValueRule :: TypedValue -> Expression.Apply TypedValue -> State Origin (Rule def ExprRef)
nonLambdaToApplyValueRule applyTv (Expression.Apply func arg) =
  NonLambdaToApplyValue (tvVal applyTv) (tvVal func, tvVal arg) <$> mkOrigin

runApplyToParts :: Eq def => Expression.Apply ExprRef -> RefExpression2 def -> RuleResult def
runApplyToParts refs (applyExpr, funcExpr) = do
  -- If definitely not a redex (func also not a hole)
  -- Apply-Arg => Arg
  -- Apply-Func => Func
  guard $ Lens.nullOf
    (Expression.eBody . Expression._BodyLam . Expression.lambdaKind . Expression._Val)
    funcExpr
  guard $ Lens.nullOf (Expression.eBody . Expression._BodyLeaf . Expression._Hole) funcExpr
  Expression.Apply aFunc aArg <- applyExpr ^.. Expression.eBody . Expression._BodyApply
  [(refs ^. Expression.applyFunc, aFunc), (refs ^. Expression.applyArg, aArg)]

applyToPartsRule ::
  TypedValue -> Expression.Apply TypedValue -> Rule def ExprRef
applyToPartsRule applyTv parts@(Expression.Apply func _) =
  ApplyToParts (tvVal <$> parts) (tvVal applyTv, tvVal func)

applyRules :: TypedValue -> Expression.Apply TypedValue -> State Origin [Rule def ExprRef]
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
      ++ recurseSubstRules Expression.Type
        (tvType applyTv) (tvType func) (tvVal arg)
      ++ recurseSubstRules Expression.Val
        (tvVal applyTv) (tvVal func) (tvVal arg)
