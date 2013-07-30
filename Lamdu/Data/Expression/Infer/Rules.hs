{-# LANGUAGE TemplateHaskell, PatternGuards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Data.Expression.Infer.Rules
  ( Rule(..)
  , makeForAll, makeForNode
  , union
  , runRule
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(..))
import Control.Lens (LensLike')
import Control.Lens.Operators
import Control.Monad (guard)
import Control.Monad.Trans.Writer (execWriter)
import Control.Monad.Unit (Unit(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (maybeToList)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable, traverse)
import Lamdu.Data.Expression.Infer.Types
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type RuleResult def = [(ExprRef, RefExpression def)]

type RefExpression2 def = (RefExpression def, RefExpression def)
type RefExpression3 def = (RefExpression def, RefExpression def, RefExpression def)

-- Boilerplate to work around lack of serialization of functions
-- Represents a serialization of RuleFunction:
data Rule def a
  = LambdaBodyTypeToPiResultType (Guid, ExprRef) a
  | PiToLambda (Guid, ExprRef, ExprRef) a
  | RecordValToType ExprRef [(a, a)]
  | RecordTypeToGetFieldType ExprRef (a, a)
  | GetFieldTypeToRecordFieldType ExprRef (a, a, a)
  | Copy ExprRef a
  | ParentToChildren (Expr.Body def ExprRef) a
  | ChildrenToParent ExprRef (Expr.Body def a)
  | SetterRule [(ExprRef, RefExpression def)]
  | SimpleType ExprRef a
  | IntoApplyResult (Expr.Kind, ExprRef, ExprRef) (a, a)
  | IntoArg (Expr.Kind, ExprRef) (a, a)
  | IntoFuncResultType (Expr.Kind, ExprRef) (a, a)
  | ArgTypeToPiParamType ExprRef a
  | RigidArgApplyTypeToResultType ExprRef (a, a)
  | RedexApplyTypeToResultType ExprRef (a, a)
  | PiParamTypeToArgType ExprRef a
  | LambdaParamTypeToArgType ExprRef a
  | ArgTypeToLambdaParamType ExprRef (a, a)
  | NonLambdaToApplyValue ExprRef (a, a, a)
  | ApplyToParts (Expr.Apply ExprRef) (a, a)
  | VerifyTagRule ExprRef a
  | DisallowTagTypeForApply ExprRef a
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

derive makeBinary ''Rule
derive makeNFData ''Rule

runRule :: Eq def => Rule def (RefExpression def) -> RuleResult def
runRule rule =
  case rule of
  LambdaBodyTypeToPiResultType x e ->
    runLambdaBodyTypeToPiResultType x e
  PiToLambda x e ->
    runPiToLambda x e
  RecordValToType x e ->
    runRecordValToType x e
  RecordTypeToGetFieldType x e ->
    runRecordTypeToGetFieldType x e
  GetFieldTypeToRecordFieldType x e ->
    runGetFieldTypeToRecordFieldType x e
  Copy x e ->
    runCopy x e
  ParentToChildren x e ->
    runParentToChildren x e
  ChildrenToParent x e ->
    runChildrenToParent x e
  SetterRule x ->
    runSetterRule x
  SimpleType x e ->
    runSimpleType x e
  IntoApplyResult x e ->
    runIntoApplyResult x e
  IntoArg x e ->
    runIntoArg x e
  IntoFuncResultType x e ->
    runIntoFuncResultType x e
  ArgTypeToPiParamType x e ->
    runArgTypeToPiParamType x e
  RigidArgApplyTypeToResultType x e ->
    runRigidArgApplyTypeToResultType x e
  RedexApplyTypeToResultType x e ->
    runRedexApplyTypeToResultType x e
  PiParamTypeToArgType x e ->
    runPiParamTypeToArgType x e
  LambdaParamTypeToArgType x e ->
    runLambdaParamTypeToArgType x e
  ArgTypeToLambdaParamType x e ->
    runArgTypeToLambdaParamType x e
  NonLambdaToApplyValue x e ->
    runNonLambdaToApplyValue x e
  ApplyToParts x e ->
    runApplyToParts x e
  VerifyTagRule x e ->
    runVerifyTagRule x e
  DisallowTagTypeForApply x e ->
    runDisallowTagTypeForApply x e

childrenToParentRules ::
  ExprRef -> Expr.Body def a -> [Rule def a]
childrenToParentRules _ Expr.BodyApply {} = []
childrenToParentRules valRef bodyWithRefs =
  [ChildrenToParent valRef bodyWithRefs]

-- Forbid composite apply, tag type
tagRules :: Expr.Expression def TypedValue -> [Rule def ExprRef]
tagRules tagExpr =
  [ VerifyTagRule tagRef tagRef
  , SetterRule [(tvType pl, makeRefExpr $ Expr.BodyLeaf Expr.TagType)]
  ]
  where
    pl = tagExpr ^. Expr.ePayload
    tagRef = tvVal pl

makeForNode :: Expr.Expression def TypedValue -> [Rule def ExprRef]
makeForNode (Expr.Expression exprBody typedVal) =
  concat
  [ [ruleSimpleType typedVal]
  , childrenToParentRules (tvVal typedVal) bodyWithValRefs
  , [ParentToChildren bodyWithValRefs (tvVal typedVal)]
  , case exprBody of
    Expr.BodyLam lambda ->
      onLambda (pl <$> lambda) : lamKindRules (pl <$> lambda)
    Expr.BodyApply apply -> applyRules typedVal $ pl <$> apply
    Expr.BodyRecord record ->
      concatMap tagRules (record ^.. Expr.recordFields . traverse . Lens._1) ++
      recordKindRules (pl <$> record)
    Expr.BodyGetField (Expr.GetField record fieldTag) ->
      getFieldRules (tvType typedVal) fieldTag (tvType (pl record))
      -- TODO: GetField Structure rules
    -- Leafs need no additional rules beyond the commonal simpleTypeRule
    Expr.BodyLeaf _ -> []
  ]
  where
    pl = (^. Expr.ePayload)
    bodyWithValRefs = tvVal . pl <$> exprBody
    recordKindRules (Expr.Record Expr.KType fields) =
      map (setterRule . tvType . snd) fields
    recordKindRules (Expr.Record Expr.KVal fields) =
      recordValueRules (tvType typedVal) $
      fields
      & Lens.mapped . Lens._1 %~ tvVal
      & Lens.mapped . Lens._2 %~ tvType
    lamKindRules (Expr.Lam Expr.KType _ _ body) =
      [setterRule (tvType body)]
    lamKindRules (Expr.Lam Expr.KVal param _ body) =
      lambdaRules param typedVal (tvType body)
    onLambda lam = setterRule . tvType $ lam ^. Expr.lamParamType
    setterRule ref = SetterRule [(ref, typeExpr)]

makeForAll :: Expr.Expression def TypedValue -> [Rule def ExprRef]
makeForAll = concatMap makeForNode . ExprUtil.subExpressions

holeRefExpr :: RefExpression def
holeRefExpr = makeRefExpr $ Expr.BodyLeaf Expr.Hole

typeExpr :: RefExpression def
typeExpr = makeRefExpr $ ExprLens.bodyType # ()

intTypeExpr :: RefExpression def
intTypeExpr = makeRefExpr $ Expr.BodyLeaf Expr.IntegerType

tagTypeExpr :: RefExpression def
tagTypeExpr = makeRefExpr $ Expr.BodyLeaf Expr.TagType

makePi :: RefExpression def -> RefExpression def -> RefExpression def
makePi paramType result =
  makeRefExpr $ ExprUtil.makePi (Guid.fromString "dummyGuid") paramType result

runLambdaBodyTypeToPiResultType :: (Guid, ExprRef) -> RefExpression def -> RuleResult def
runLambdaBodyTypeToPiResultType (param, lambdaTypeRef) bodyTypeExpr =
  [( lambdaTypeRef
   , makeRefExpr $ ExprUtil.makePi param holeRefExpr bodyTypeExpr
   )]

runPiToLambda :: (Guid, ExprRef, ExprRef) -> RefExpression def -> RuleResult def
runPiToLambda (param, lambdaValueRef, bodyTypeRef) piBody = do
  -- TODO: Use exprKindedLam
  Expr.Lam Expr.KType piParam paramType resultType <-
    piBody ^.. ExprLens.exprLam
  [ -- Pi result type -> Body type
    ( bodyTypeRef
    , ExprUtil.substGetPar piParam
      (makeRefExpr (ExprLens.bodyParameterRef # param))
      resultType
    )
    , -- Pi param type -> Lambda param type
      ( lambdaValueRef
      , makeRefExpr $ ExprUtil.makeLambda piParam paramType holeRefExpr
      )
    ]

lambdaRules :: Guid -> TypedValue -> ExprRef -> [Rule def ExprRef]
lambdaRules param (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  [ LambdaBodyTypeToPiResultType (param, lambdaTypeRef) bodyTypeRef
  , PiToLambda (param, lambdaValueRef, bodyTypeRef) lambdaTypeRef
  ]

runRecordValToType :: ExprRef -> [RefExpression2 def] -> RuleResult def
runRecordValToType recordTypeRef fields =
  [ ( recordTypeRef
    , makeRefExpr . Expr.BodyRecord $
      Expr.Record Expr.KType fields
    )
  ]

recordFields :: Lens.Traversal' (Expr.Expression def a) (Expr.Expression def a, Expr.Expression def a)
recordFields = ExprLens.exprRecord . Expr.recordFields . traverse

recordField ::
  Applicative f => Guid ->
  LensLike' f (Expr.Expression def a) (Expr.Expression def a, Expr.Expression def a)
recordField guid =
  recordFields .
  Lens.filtered
  ((== Just guid) . (^? Lens._1 . ExprLens.exprTag))

runRecordTypeToGetFieldType :: ExprRef -> RefExpression2 def -> RuleResult def
runRecordTypeToGetFieldType getFieldTypeRef (recordTypeExpr, fieldTag) = do
  guid <- fieldTag ^.. ExprLens.exprTag
  (_, fieldType) <- recordTypeExpr ^.. recordField guid
  [(getFieldTypeRef, fieldType)]

runGetFieldTypeToRecordFieldType :: ExprRef -> RefExpression3 def -> RuleResult def
runGetFieldTypeToRecordFieldType recordTypeRef (recordTypeExpr, fieldTag, getFieldTypeExpr) =
  case fieldTag ^. Expr.eBody of
  Expr.BodyLeaf Expr.Hole -> verifyRecordWithField
  Expr.BodyLeaf (Expr.Tag guid) -> putTypeIntoRecordField guid
  _ -> makeError
  where
    recordTypeExample =
      makeRefExpr . Expr.BodyRecord . Expr.Record Expr.KType $
      [ (fieldTag, makeRefExpr (Expr.BodyLeaf Expr.Hole)) ]
    makeError = [(recordTypeRef, recordTypeExample)]
    recordTypeIsHole =
      [ ( recordTypeRef
        , recordTypeExpr & Expr.ePayload . rplRestrictedPoly . Lens.unwrapped .~ True
        )
      ]
    verifyRecordWithField =
      case recordTypeExpr ^. Expr.eBody of
      Expr.BodyLeaf Expr.Hole -> recordTypeIsHole
      Expr.BodyRecord (Expr.Record Expr.KType (_:_)) -> []
      _ -> makeError
    putTypeIntoRecordField guid =
      case recordTypeExpr ^. Expr.eBody of
      Expr.BodyLeaf Expr.Hole -> recordTypeIsHole
      _ | Lens.notNullOf (recordField guid) recordTypeExpr ->
          [ ( recordTypeRef
            , recordTypeExpr & recordField guid . Lens._2 .~ getFieldTypeExpr
            )
          ]
        | Lens.notNullOf (recordFields . Lens._1 . ExprLens.exprHole) recordTypeExpr -> []
        | otherwise -> makeError

getFieldRules :: ExprRef -> Expr.Expression def TypedValue -> ExprRef -> [Rule def ExprRef]
getFieldRules getFieldTypeRef tagExpr recordTypeRef =
  [ RecordTypeToGetFieldType getFieldTypeRef (recordTypeRef, tagValRef)
  , GetFieldTypeToRecordFieldType recordTypeRef (recordTypeRef, tagValRef, getFieldTypeRef)
  ] ++ tagRules tagExpr
  where
    tagValRef = tvVal $ tagExpr ^. Expr.ePayload

recordValueRules :: ExprRef -> [(ExprRef, ExprRef)] -> [Rule def ExprRef]
recordValueRules recTypeRef fieldTypeRefs =
  [ RecordValToType recTypeRef fieldTypeRefs
  , ParentToChildren (Expr.BodyRecord (Expr.Record Expr.KType fieldTypeRefs)) recTypeRef
  ]

runCopy :: ExprRef -> RefExpression def -> RuleResult def
runCopy dest srcExpr = [(dest, srcExpr)]

union :: ExprRef -> ExprRef -> [Rule def ExprRef]
union x y =
  [ Copy y x
  , Copy x y
  ]

-- Parent lambda to children
runParentToChildren ::
  Eq def => Expr.Body def ExprRef -> RefExpression def ->
  RuleResult def
runParentToChildren Expr.BodyApply {} _ = []
runParentToChildren childrenRefs expr = do
  bodyRules <-
    maybeToList . ExprUtil.matchBodyDeprecated
     matchLamResult (,)
    ((const . const) True) childrenRefs $
    expr ^. Expr.eBody
  Foldable.toList bodyRules
  where
    matchLamResult aGuid _bGuid x y = (aGuid, (x, y))

runChildrenToParent :: ExprRef -> Expr.Body def (RefExpression def) -> RuleResult def
runChildrenToParent destRef bodyWithExprs = [(destRef, makeRefExpr bodyWithExprs)]

runSetterRule :: [(ExprRef, RefExpression def)] -> RuleResult def
runSetterRule outputs = outputs

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
        (rplSubstitutedArgs .~ substs) <$> src
      | notAHole src =
        (rplRestrictedPoly . Lens.unwrapped .~ True) <$> dest
      | otherwise = dest
      where
        substs = dest ^. Expr.ePayload . rplSubstitutedArgs
    notAHole = Lens.nullOf ExprLens.exprHole

runSimpleType :: ExprRef -> RefExpression def -> RuleResult def
runSimpleType typ valExpr =
  case valExpr ^. Expr.eBody of
  Expr.BodyLeaf Expr.Type -> simpleType
  Expr.BodyLeaf Expr.IntegerType -> simpleType
  Expr.BodyLeaf Expr.TagType -> simpleType
  Expr.BodyLeaf Expr.LiteralInteger {} -> [(typ, intTypeExpr)]
  Expr.BodyLeaf Expr.GetVariable {} -> []
  Expr.BodyLeaf Expr.Hole {} -> []
  Expr.BodyLeaf Expr.Tag {} -> [(typ, tagTypeExpr)]
  Expr.BodyLam (Expr.Lam Expr.KType _ _ _) -> simpleType
  Expr.BodyRecord (Expr.Record Expr.KType _) -> simpleType
  Expr.BodyRecord (Expr.Record Expr.KVal _) ->
    -- The rule to copy inferred types of fields to the inferred type
    -- of the whole record requiers dereferencing the inferred types
    -- of the field exprs which is impossible in this context. This is
    -- handled in the recordValueRules
    []
  Expr.BodyLam
    (Expr.Lam Expr.KVal param paramType _) ->
    [( typ
     , makeRefExpr . ExprUtil.makePi param paramType $
       holeRefExpr
     )]
  -- All type information that can be deduced from these depends on
  -- external information which is not used in the simple type rules.
  Expr.BodyApply {} -> []
  Expr.BodyGetField {} -> []
  where
    simpleType = [(typ, typeExpr)]

ruleSimpleType :: TypedValue -> Rule def ExprRef
ruleSimpleType (TypedValue val typ) = SimpleType typ val

-- PreSubst with Subst => PostSubst
-- (func, arg) -> apply
runIntoApplyResult :: (Expr.Kind, ExprRef, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoApplyResult (k, applyRef, arg) (funcExpr, argExpr) = do
  -- TODO: Use exprKindedLam
  Expr.Lam bk param _ result <- funcExpr ^.. ExprLens.exprLam
  guard $ k == bk
  return
    ( applyRef
    , ExprUtil.substGetPar param
      (argExpr & Lens.traversed . rplSubstitutedArgs %~ IntSet.insert (unExprRef arg)) .
      -- TODO: Is this correct?
      (Lens.traversed . rplRestrictedPoly . Lens.unwrapped .~ False) $
      result
    )

runIntoArg :: Eq def => (Expr.Kind, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoArg (k, arg) (applyExpr, funcExpr) = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  -- TODO: Use exprKindedLam
  Expr.Lam bk param _ result <- funcExpr ^.. ExprLens.exprLam
  guard $ bk == k
  mergeToArg param arg result applyExpr

-- Propagate data from Apply's to the Func where appropriate.
-- (Not on non-substituted holes)
-- apply -> func result
runIntoFuncResultType :: Eq def => (Expr.Kind, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoFuncResultType (k, func) (applyExpr, funcExpr) = do
  Expr.Lam kb param paramT result <- funcExpr ^.. Expr.eBody . Expr._BodyLam
  guard $ k == kb
  return
    ( func
    , makeRefExpr .
      Expr.BodyLam . Expr.Lam k param paramT $
      mergeToPiResult result applyExpr
    )

-- param, (dest)argRef, func result, applyExpr
mergeToArg :: Eq def => Guid -> ExprRef -> RefExpression def -> RefExpression def -> [(ExprRef, RefExpression def)]
mergeToArg param arg =
  (fmap . fmap) (execWriter . Compose.unO) $
  ExprUtil.matchExpression onMatch onMismatch
  where
    unit = Compose.O (pure Unit)
    onMatch _ _ = unit
    onMismatch expr post
      | Lens.anyOf ExprLens.exprParameterRef (== param) expr
      = Compose.O . (fmap . const) Unit $ Writer.tell
        [( arg
         , post
           & Lens.traversed . rplSubstitutedArgs %~ IntSet.delete (unExprRef arg)
           -- TODO: Is this correct?
           & Lens.traversed . rplRestrictedPoly . Lens.unwrapped .~ False
         )]
      | otherwise = unit

-- ArgT => Pi ParamT
runArgTypeToPiParamType :: ExprRef -> RefExpression def -> RuleResult def
runArgTypeToPiParamType funcTypeRef argTypeExpr =
  [( funcTypeRef
   , makePi argTypeExpr holeRefExpr
   )]

-- Rigid value: An expression whose sole information content comes
-- from a GetVar. IOW: All of its information is universally
-- quantified.
rigidValue :: Expr.Expression def a -> Bool
rigidValue e = case e ^. Expr.eBody of
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef _)) -> True
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef _)) -> False
  -- A lambda is a black box, don't want to break and enter into it to
  -- figure out whether it has information content
  Expr.BodyLam (Expr.Lam Expr.KVal _ _ _) -> False
  -- An apply is of a function, which is a black box (see above).
  Expr.BodyApply (Expr.Apply _ _) -> False
  Expr.BodyRecord (Expr.Record Expr.KVal fields) -> all (rigidValue . snd) fields
  Expr.BodyGetField (Expr.GetField record _) -> rigidValue record
  Expr.BodyLeaf Expr.Hole -> False
  Expr.BodyLeaf (Expr.LiteralInteger _) -> False
  -- Can't really use a tag (at least yet...)
  Expr.BodyLeaf (Expr.Tag _) -> True
  -- Types have no (runtime) information content
  Expr.BodyLeaf Expr.Type -> True
  Expr.BodyLeaf Expr.IntegerType -> True
  Expr.BodyLeaf Expr.TagType -> True
  Expr.BodyLam (Expr.Lam Expr.KType _ _ _) -> True
  Expr.BodyRecord (Expr.Record Expr.KType _) -> True

-- If Arg is rigid
-- ApplyT (Susbt Arg with Hole) => ResultT
--
-- Rationale: When a function f of type FT is applied on a rigid
-- parameter p, and we use its result as a certain type T, there's
-- no way that FT is dependent on p, because in the context of T,
-- any information in p is universally quantified. Parts of FT may
-- still be dependent on p, but the part that matches T mustn't be.
runRigidArgApplyTypeToResultType :: Eq def => ExprRef -> RefExpression2 def -> RuleResult def
runRigidArgApplyTypeToResultType funcTypeRef (applyTypeExpr, argExpr) = do
  guard $ rigidValue argExpr
  return
    ( funcTypeRef
    , makePi holeRefExpr $
      ExprUtil.subst (Lens.filtered (ExprUtil.couldEq argExpr)) holeRefExpr applyTypeExpr
    )

runRedexApplyTypeToResultType :: ExprRef -> RefExpression2 def -> RuleResult def
runRedexApplyTypeToResultType funcTypeRef (applyTypeExpr, funcExpr) = do
  Expr.Lam Expr.KVal paramGuid paramType _ <-
    funcExpr ^.. Expr.eBody . Expr._BodyLam
  return
    ( funcTypeRef
    , makeRefExpr $
      ExprUtil.makePi paramGuid paramType applyTypeExpr
    )

runPiParamTypeToArgType :: ExprRef -> RefExpression def -> RuleResult def
runPiParamTypeToArgType argTypeRef (Expr.Expression funcTExpr _) = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Expr.Lam Expr.KType _ paramT _ <- funcTExpr ^.. Expr._BodyLam
  return (argTypeRef, paramT)

runLambdaParamTypeToArgType :: ExprRef -> RefExpression def -> RuleResult def
runLambdaParamTypeToArgType argTypeRef (Expr.Expression funcExpr _) = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Expr.Lam Expr.KVal _ paramT _ <-
    funcExpr ^.. Expr._BodyLam
  return (argTypeRef, paramT)

runArgTypeToLambdaParamType :: ExprRef -> RefExpression2 def -> RuleResult def
runArgTypeToLambdaParamType funcValRef (funcExpr, argTExpr) = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Expr.Lam Expr.KVal param _ _ <- funcExpr ^.. Expr.eBody . Expr._BodyLam
  return
    ( funcValRef
    , makeRefExpr $
      ExprUtil.makeLambda param argTExpr holeRefExpr
    )

runNonLambdaToApplyValue :: ExprRef -> RefExpression3 def -> RuleResult def
runNonLambdaToApplyValue applyValRef (funcExpr, argExpr, applyTypExpr) =
  -- If func is surely not a lambda (a hole too could be a lambda).
  --
  -- Applies have a special case in the inferred value handling for
  -- redexes, and this rule is about detecting that an application is
  -- *not* a redex, so we can safely set the inferred value to the
  -- application itself.
  --
  -- Func Arg => Outer
  case funcExpr ^. Expr.eBody of
  Expr.BodyLam (Expr.Lam Expr.KVal _ _ _) -> []
  Expr.BodyLeaf Expr.Hole -> []
  _ -> do
    guard $ ExprUtil.isTypeConstructorType applyTypExpr
    return
      ( applyValRef
      , makeRefExpr $ ExprUtil.makeApply funcExpr argExpr
      )

runApplyToParts :: Eq def => Expr.Apply ExprRef -> RefExpression2 def -> RuleResult def
runApplyToParts refs (applyExpr, funcExpr) = do
  -- If definitely not a redex (func also not a hole)
  -- Apply-Arg => Arg
  -- Apply-Func => Func
  -- TODO: Use exprKindedLam
  guard $ Lens.nullOf (ExprLens.exprLam . Expr.lamKind . Expr._KVal) funcExpr
  guard $ Lens.nullOf ExprLens.exprHole funcExpr
  Expr.Apply aFunc aArg <- applyExpr ^.. ExprLens.exprApply
  [(refs ^. Expr.applyFunc, aFunc), (refs ^. Expr.applyArg, aArg)]

runVerifyTagRule :: ExprRef -> RefExpression def -> RuleResult def
runVerifyTagRule tagRef tagExpr =
  case tagExpr ^. Expr.eBody of
  Expr.BodyLeaf Expr.Hole -> []
  Expr.BodyLeaf Expr.Tag {} -> []
  _ -> makeError
  where
    makeError =
      [ ( tagRef
        , makeRefExpr . Expr.BodyLeaf . Expr.Tag $
          Guid.fromString "EXAMPLE"
        )
      ]

runDisallowTagTypeForApply :: ExprRef -> RefExpression def -> RuleResult def
runDisallowTagTypeForApply applyValRef applyTypeExpr = do
  _ <- applyTypeExpr ^.. ExprLens.exprTagType
  makeError
  where
    makeError =
      [ ( applyValRef
        , makeRefExpr . Expr.BodyApply $
          Expr.Apply holeRefExpr holeRefExpr
        )
      ]

applyRules :: TypedValue -> Expr.Apply TypedValue -> [Rule def ExprRef]
applyRules applyTv apply@(Expr.Apply func arg) =
  -- TODO: make all of these functions have a standard signature and
  -- just apply them all to the same args?
  [ PiParamTypeToArgType (tvType arg) (tvType func)
  , RedexApplyTypeToResultType (tvType func) (tvType applyTv, tvVal func)
  , LambdaParamTypeToArgType (tvType arg) (tvVal func)
  , ApplyToParts (tvVal <$> apply) (tvVal applyTv, tvVal func)
  , ArgTypeToPiParamType (tvType func) (tvType arg)
  , RigidArgApplyTypeToResultType (tvType func) (tvType applyTv, tvVal arg)
  , ArgTypeToLambdaParamType (tvVal func) (tvVal func, tvType arg)
  , NonLambdaToApplyValue (tvVal applyTv) (tvVal func, tvVal arg, tvType applyTv)
  , DisallowTagTypeForApply (tvVal applyTv) (tvType applyTv)
  ]
  ++ recurseSubstRules Expr.KType (tvType applyTv) (tvType func) (tvVal arg)
  ++ recurseSubstRules Expr.KVal (tvVal applyTv) (tvVal func) (tvVal arg)
  where
    recurseSubstRules k applyRef funcRef argValRef =
      [ IntoApplyResult (k, applyRef, argValRef) (funcRef, argValRef)
      , IntoArg (k, argValRef) (applyRef, funcRef)
      , IntoFuncResultType (k, funcRef) (applyRef, funcRef)
      ]
