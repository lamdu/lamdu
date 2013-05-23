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
import Control.Monad.Trans.State (State)
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
import Data.Traversable (Traversable, traverse, sequenceA)
import Lamdu.Data.Expression.Infer.Types
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Monoid as Monoid
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
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
  | RecordValToType ExprRef [(a, a)] Origin
  | RecordTypeToGetFieldType ExprRef (a, a)
  | GetFieldTypeToRecordFieldType ExprRef (a, a, a) Origin2
  | Copy ExprRef a
  | ParentToChildren (Expr.Body def ExprRef) a
  | ChildrenToParent ExprRef (Expr.Body def a) Origin
  | SetRule [(ExprRef, RefExpression def)]
  | SimpleType ExprRef a Origin2
  | IntoApplyResult (Expr.Kind, ExprRef, ExprRef) (a, a)
  | IntoArg (Expr.Kind, ExprRef) (a, a)
  | IntoFuncResultType (Expr.Kind, ExprRef) (a, a)
  | ArgTypeToPiParamType ExprRef a Origin2
  | RigidArgApplyTypeToResultType ExprRef (a, a) Origin3
  | RedexApplyTypeToResultType ExprRef (a, a)
  | PiParamTypeToArgType ExprRef a
  | LambdaParamTypeToArgType ExprRef a
  | ArgTypeToLambdaParamType ExprRef (a, a) Origin
  | NonLambdaToApplyValue ExprRef (a, a) Origin
  | ApplyToParts (Expr.Apply ExprRef) (a, a)
  | VerifyTagRule ExprRef a Origin
  | DisallowTagTypeForApply ExprRef a Origin3
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
  RecordTypeToGetFieldType x e ->
    runRecordTypeToGetFieldType x e
  GetFieldTypeToRecordFieldType x e o ->
    runGetFieldTypeToRecordFieldType x e o
  Copy x e ->
    runCopy x e
  ParentToChildren x e ->
    runParentToChildren x e
  ChildrenToParent x e o ->
    runChildrenToParent x e o
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
  VerifyTagRule x e o ->
    runVerifyTagRule x e o
  DisallowTagTypeForApply x e o ->
    runDisallowTagTypeForApply x e o

childrenToParentRules ::
  ExprRef -> Expr.Body def a -> State Origin [Rule def a]
childrenToParentRules _ Expr.BodyApply {} = pure []
childrenToParentRules valRef bodyWithRefs =
  (: []) . ChildrenToParent valRef bodyWithRefs <$> mkOrigin

-- Forbid composite apply, tag type
tagRules :: Expr.Expression def TypedValue -> State Origin [Rule def ExprRef]
tagRules tagExpr =
  fmap concat $
  sequenceA
  [ (:[]) . VerifyTagRule tagRef tagRef <$> mkOrigin
  , do
      o <- mkOrigin
      return [SetRule [(tvType pl, makeRefExpr o $ Expr.BodyLeaf Expr.TagType)]]
  ]
  where
    pl = tagExpr ^. Expr.ePayload
    tagRef = tvVal pl

makeForNode :: Expr.Expression def TypedValue -> State Origin [Rule def ExprRef]
makeForNode (Expr.Expression exprBody typedVal) =
  fmap concat $
  sequenceA
  [ (: []) <$> ruleSimpleType typedVal
  , childrenToParentRules (tvVal typedVal) bodyWithValRefs
  , pure [ParentToChildren bodyWithValRefs (tvVal typedVal)]
  , case exprBody of
    Expr.BodyLam lambda ->
      (:) <$> onLambda (pls lambda) <*> lamKindRules (pls lambda)
    Expr.BodyApply apply -> applyRules typedVal $ pls apply
    Expr.BodyRecord record ->
      (++)
      <$> (fmap concat . traverse tagRules)
          (record ^.. Expr.recordFields . traverse . Lens._1)
      <*> recordKindRules (pls record)
    Expr.BodyGetField (Expr.GetField record fieldTag) ->
      getFieldRules (tvType typedVal) fieldTag (tvType (pl record))
      -- TODO: GetField Structure rules
    -- Leafs need no additional rules beyond the commonal simpleTypeRule
    Expr.BodyLeaf _ -> pure []
  ]
  where
    pl = (^. Expr.ePayload)
    pls x = pl <$> x
    bodyWithValRefs = tvVal <$> pls exprBody
    recordKindRules (Expr.Record Expr.Type fields) =
      mapM (setRule . tvType . snd) fields
    recordKindRules (Expr.Record Expr.Val fields) =
      recordValueRules (tvType typedVal) $
      fields
      & Lens.mapped . Lens._1 %~ tvVal
      & Lens.mapped . Lens._2 %~ tvType
    lamKindRules (Expr.Lambda Expr.Type _ _ body) =
      fmap (:[]) . setRule $ tvType body
    lamKindRules (Expr.Lambda Expr.Val param _ body) =
      lambdaRules param typedVal (tvType body)
    onLambda lam = setRule . tvType $ lam ^. Expr.lambdaParamType
    setRule ref = do
      o <- mkOrigin
      return $ SetRule [(ref, setExpr o)]

makeForAll :: Expr.Expression def TypedValue -> State Origin [Rule def ExprRef]
makeForAll = fmap concat . traverse makeForNode . ExprUtil.subExpressions

makeHole :: Origin -> RefExpression def
makeHole g = makeRefExpr g $ Expr.BodyLeaf Expr.Hole

setExpr :: Origin -> RefExpression def
setExpr g = makeRefExpr g $ Expr.BodyLeaf Expr.Set

intTypeExpr :: Origin -> RefExpression def
intTypeExpr g = makeRefExpr g $ Expr.BodyLeaf Expr.IntegerType

tagTypeExpr :: Origin -> RefExpression def
tagTypeExpr g = makeRefExpr g $ Expr.BodyLeaf Expr.TagType

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
  Expr.Lambda Expr.Type piParam paramType resultType <-
    piBody ^.. Expr.eBody . Expr._BodyLam
  [ -- Pi result type -> Body type
    ( bodyTypeRef
    , ExprUtil.subst piParam
      ( makeRefExpr o0
        (Lens.review ExprLens.bodyParameterRef param)
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

runRecordValToType :: ExprRef -> [RefExpression2 def] -> Origin -> RuleResult def
runRecordValToType recordTypeRef fields o0 =
  [ ( recordTypeRef
    , makeRefExpr o0 . Expr.BodyRecord $
      Expr.Record Expr.Type fields
    )
  ]

recordFields :: Lens.Traversal' (Expr.Expression def a) (Expr.Expression def a, Expr.Expression def a)
recordFields = Expr.eBody . Expr._BodyRecord . Expr.recordFields . traverse

recordField ::
  Applicative f => Guid ->
  LensLike' f (Expr.Expression def a) (Expr.Expression def a, Expr.Expression def a)
recordField guid =
  recordFields .
  Lens.filtered
  ((== Just guid) . (^? Lens._1 . Expr.eBody . Expr._BodyLeaf . Expr._Tag))

runRecordTypeToGetFieldType :: ExprRef -> RefExpression2 def -> RuleResult def
runRecordTypeToGetFieldType getFieldTypeRef (recordTypeExpr, fieldTag) = do
  guid <- fieldTag ^.. Expr.eBody . Expr._BodyLeaf . Expr._Tag
  (_, fieldType) <- recordTypeExpr ^.. recordField guid
  [(getFieldTypeRef, fieldType)]

runGetFieldTypeToRecordFieldType :: ExprRef -> RefExpression3 def -> Origin2 -> RuleResult def
runGetFieldTypeToRecordFieldType recordTypeRef (recordTypeExpr, fieldTag, getFieldTypeExpr) (o0, o1) =
  case fieldTag ^. Expr.eBody of
  Expr.BodyLeaf Expr.Hole -> verifyRecordWithField
  Expr.BodyLeaf (Expr.Tag guid) -> putTypeIntoRecordField guid
  _ -> makeError
  where
    recordTypeExample =
      makeRefExpr o0 . Expr.BodyRecord . Expr.Record Expr.Type $
      [ (fieldTag, makeRefExpr o1 (Expr.BodyLeaf Expr.Hole)) ]
    makeError = [(recordTypeRef, recordTypeExample)]
    recordTypeIsHole =
      [ ( recordTypeRef
        , recordTypeExpr & Expr.ePayload . rplRestrictedPoly .~ Monoid.Any True
        )
      ]
    verifyRecordWithField =
      case recordTypeExpr ^. Expr.eBody of
      Expr.BodyLeaf Expr.Hole -> recordTypeIsHole
      Expr.BodyRecord (Expr.Record Expr.Type (_:_)) -> []
      _ -> makeError
    putTypeIntoRecordField guid =
      case recordTypeExpr ^. Expr.eBody of
      Expr.BodyLeaf Expr.Hole -> recordTypeIsHole
      _ | Lens.notNullOf (recordField guid) recordTypeExpr ->
          [ ( recordTypeRef
            , recordTypeExpr & recordField guid . Lens._2 .~ getFieldTypeExpr
            )
          ]
        | Lens.notNullOf (recordFields . Lens._1 . Expr.eBody . ExprLens.bodyHole) recordTypeExpr -> []
        | otherwise -> makeError

getFieldRules :: ExprRef -> Expr.Expression def TypedValue -> ExprRef -> State Origin [Rule def ExprRef]
getFieldRules getFieldTypeRef tagExpr recordTypeRef =
  fmap concat $ sequenceA
  [ pure [RecordTypeToGetFieldType getFieldTypeRef (recordTypeRef, tagValRef)]
  , (: []) . GetFieldTypeToRecordFieldType recordTypeRef (recordTypeRef, tagValRef, getFieldTypeRef) <$> mkOrigin2
  , tagRules tagExpr
  ]
  where
    tagValRef = tvVal $ tagExpr ^. Expr.ePayload

recordValueRules :: ExprRef -> [(ExprRef, ExprRef)] -> State Origin [Rule def ExprRef]
recordValueRules recTypeRef fieldTypeRefs =
  sequenceA
  [ RecordValToType recTypeRef fieldTypeRefs <$> mkOrigin
  , pure $ ParentToChildren (Expr.BodyRecord (Expr.Record Expr.Type fieldTypeRefs)) recTypeRef
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
    maybeToList . ExprUtil.matchBody
    ((const . const) (,)) (,)
    ((const . const) True) childrenRefs $
    expr ^. Expr.eBody
  Foldable.toList bodyRules

runChildrenToParent :: ExprRef -> Expr.Body def (RefExpression def) -> Origin -> RuleResult def
runChildrenToParent destRef bodyWithExprs o0 = [(destRef, makeRefExpr o0 bodyWithExprs)]

runSetRule :: [(ExprRef, RefExpression def)] -> RuleResult def
runSetRule outputs = outputs

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
        substs = dest ^. Expr.ePayload . rplSubstitutedArgs
    notAHole = Lens.nullOf (Expr.eBody . ExprLens.bodyHole)

runSimpleType :: ExprRef -> RefExpression def -> Origin2 -> RuleResult def
runSimpleType typ valExpr (o0, o1) =
  case valExpr ^. Expr.eBody of
  Expr.BodyLeaf Expr.Set -> simpleType
  Expr.BodyLeaf Expr.IntegerType -> simpleType
  Expr.BodyLeaf Expr.TagType -> [(typ, setExpr o0)]
  Expr.BodyLeaf Expr.LiteralInteger {} -> [(typ, intTypeExpr o0)]
  Expr.BodyLeaf Expr.GetVariable {} -> []
  Expr.BodyLeaf Expr.Hole {} -> []
  Expr.BodyLeaf Expr.Tag {} -> [(typ, tagTypeExpr o0)]
  Expr.BodyLam (Expr.Lambda Expr.Type _ _ _) -> simpleType
  Expr.BodyRecord (Expr.Record Expr.Type _) -> simpleType
  Expr.BodyRecord (Expr.Record Expr.Val _) ->
    -- The rule to copy inferred types of fields to the inferred type
    -- of the whole record requiers dereferencing the inferred types
    -- of the field exprs which is impossible in this context. This is
    -- handled in the recordValueRules
    []
  Expr.BodyLam
    (Expr.Lambda Expr.Val param paramType _) ->
    [( typ
     , makeRefExpr o0 . ExprUtil.makePi param paramType $
       makeHole o1
     )]
  -- All type information that can be deduced from these depends on
  -- external information which is not used in the simple type rules.
  Expr.BodyApply {} -> []
  Expr.BodyGetField {} -> []
  where
    simpleType = [(typ, setExpr o0)]

ruleSimpleType :: TypedValue -> State Origin (Rule def ExprRef)
ruleSimpleType (TypedValue val typ) = SimpleType typ val <$> mkOrigin2

runIntoApplyResult :: (Expr.Kind, ExprRef, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoApplyResult (k, applyRef, arg) (funcExpr, argExpr) = do
  Expr.Lambda bk param _ result <-
    funcExpr ^.. Expr.eBody . Expr._BodyLam
  guard $ k == bk
  return
    ( applyRef
    , ExprUtil.subst param
      (argExpr & Lens.traversed . rplSubstitutedArgs %~ IntSet.insert (unExprRef arg)) .
      -- TODO: Is this correct?
      Lens.set (Lens.traversed . rplRestrictedPoly) (Monoid.Any False) $
      result
    )

intoApplyResultRule :: Expr.Kind -> ExprRef -> ExprRef -> ExprRef -> Rule def ExprRef
intoApplyResultRule k applyRef func arg =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  IntoApplyResult (k, applyRef, arg) (func, arg)

runIntoArg :: Eq def => (Expr.Kind, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoArg (k, arg) (applyExpr, funcExpr) = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Expr.Lambda bk param _ result <-
    funcExpr ^.. Expr.eBody . Expr._BodyLam
  guard $ bk == k
  mergeToArg param arg result applyExpr

intoArgRule :: Expr.Kind -> ExprRef -> ExprRef -> ExprRef -> Rule def ExprRef
intoArgRule k applyRef func arg =
  IntoArg (k, arg) (applyRef, func)

runIntoFuncResultType :: Eq def => (Expr.Kind, ExprRef) -> RefExpression2 def -> RuleResult def
runIntoFuncResultType (k, func) (applyExpr, Expr.Expression funcBody funcPl) = do
  Expr.Lambda kb param paramT result <- funcBody ^.. Expr._BodyLam
  guard $ k == kb
  return
    ( func
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      Expr.BodyLam . Expr.Lambda k param paramT $
      mergeToPiResult result applyExpr
    )

intoFuncResultTypeRule :: Expr.Kind -> ExprRef -> ExprRef -> Rule def ExprRef
intoFuncResultTypeRule k applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  IntoFuncResultType (k, func) (applyRef, func)

recurseSubstRules :: Expr.Kind -> ExprRef -> ExprRef -> ExprRef -> [Rule def ExprRef]
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
        (Expr.eBody . ExprLens.bodyParameterRef)
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

argTypeToPiParamTypeRule :: Expr.Apply TypedValue -> State Origin (Rule def ExprRef)
argTypeToPiParamTypeRule (Expr.Apply func arg) =
  -- ArgT => Pi ParamT
  ArgTypeToPiParamType (tvType func) (tvType arg) <$> mkOrigin2

runRigidArgApplyTypeToResultType :: ExprRef -> RefExpression2 def -> Origin3 -> RuleResult def
runRigidArgApplyTypeToResultType funcTypeRef (applyTypeExpr, argExpr) (o0, o1, o2) = do
  par <-
    argExpr ^..
    Expr.eBody . Expr._BodyLeaf . Expr._GetVariable . Expr._ParameterRef
  return
    ( funcTypeRef
    , makePi o0 (makeHole o1) $
      ExprUtil.subst par (makeHole o2) applyTypeExpr
    )

rigidArgApplyTypeToResultTypeRule ::
  TypedValue -> Expr.Apply TypedValue -> State Origin (Rule def ExprRef)
rigidArgApplyTypeToResultTypeRule applyTv (Expr.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  RigidArgApplyTypeToResultType (tvType func) (tvType applyTv, tvVal arg) <$> mkOrigin3

runRedexApplyTypeToResultType :: ExprRef -> RefExpression2 def -> RuleResult def
runRedexApplyTypeToResultType funcTypeRef
  (applyTypeExpr, Expr.Expression funcExpr funcPl) = do
  Expr.Lambda Expr.Val paramGuid paramType _ <-
    funcExpr ^.. Expr._BodyLam
  return
    ( funcTypeRef
    , makeRefExpr (Lens.view rplOrigin funcPl) $
      ExprUtil.makePi paramGuid paramType applyTypeExpr
    )

redexApplyTypeToResultTypeRule :: TypedValue -> TypedValue -> Rule def ExprRef
redexApplyTypeToResultTypeRule applyTv funcTv =
  RedexApplyTypeToResultType (tvType funcTv) (tvType applyTv, tvVal funcTv)

runPiParamTypeToArgType :: ExprRef -> RefExpression def -> RuleResult def
runPiParamTypeToArgType argTypeRef (Expr.Expression funcTExpr _) = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Expr.Lambda Expr.Type _ paramT _ <- funcTExpr ^.. Expr._BodyLam
  return (argTypeRef, paramT)

piParamTypeToArgTypeRule :: Expr.Apply TypedValue -> Rule def ExprRef
piParamTypeToArgTypeRule (Expr.Apply func arg) =
  PiParamTypeToArgType (tvType arg) (tvType func)

runLambdaParamTypeToArgType :: ExprRef -> RefExpression def -> RuleResult def
runLambdaParamTypeToArgType argTypeRef (Expr.Expression funcExpr _) = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Expr.Lambda Expr.Val _ paramT _ <-
    funcExpr ^.. Expr._BodyLam
  return (argTypeRef, paramT)

lambdaParamTypeToArgTypeRule :: Expr.Apply TypedValue -> Rule def ExprRef
lambdaParamTypeToArgTypeRule (Expr.Apply func arg) =
  LambdaParamTypeToArgType (tvType arg) (tvVal func)

runArgTypeToLambdaParamType :: ExprRef -> RefExpression2 def -> Origin -> RuleResult def
runArgTypeToLambdaParamType funcValRef (Expr.Expression funcExpr funcPl, argTExpr) o0 = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Expr.Lambda Expr.Val param _ _ <-
    funcExpr ^.. Expr._BodyLam
  return
    ( funcValRef
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      ExprUtil.makeLambda param argTExpr $ makeHole o0
    )

argTypeToLambdaParamTypeRule :: Expr.Apply TypedValue -> State Origin (Rule def ExprRef)
argTypeToLambdaParamTypeRule (Expr.Apply func arg) =
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
  case funcExpr ^. Expr.eBody of
  Expr.BodyLam (Expr.Lambda Expr.Val _ _ _) -> []
  Expr.BodyLeaf Expr.Hole -> []
  _ ->
    [ ( applyValRef
      , makeRefExpr o0 $ ExprUtil.makeApply funcExpr argExpr
      )
    ]

nonLambdaToApplyValueRule :: TypedValue -> Expr.Apply TypedValue -> State Origin (Rule def ExprRef)
nonLambdaToApplyValueRule applyTv (Expr.Apply func arg) =
  NonLambdaToApplyValue (tvVal applyTv) (tvVal func, tvVal arg) <$> mkOrigin

runApplyToParts :: Eq def => Expr.Apply ExprRef -> RefExpression2 def -> RuleResult def
runApplyToParts refs (applyExpr, funcExpr) = do
  -- If definitely not a redex (func also not a hole)
  -- Apply-Arg => Arg
  -- Apply-Func => Func
  guard $ Lens.nullOf
    (Expr.eBody . Expr._BodyLam . Expr.lambdaKind . Expr._Val)
    funcExpr
  guard $ Lens.nullOf (Expr.eBody . Expr._BodyLeaf . Expr._Hole) funcExpr
  Expr.Apply aFunc aArg <- applyExpr ^.. Expr.eBody . Expr._BodyApply
  [(refs ^. Expr.applyFunc, aFunc), (refs ^. Expr.applyArg, aArg)]

runVerifyTagRule :: ExprRef -> RefExpression def -> Origin -> RuleResult def
runVerifyTagRule tagRef tagExpr o0 =
  case tagExpr ^. Expr.eBody of
  Expr.BodyLeaf Expr.Hole -> []
  Expr.BodyLeaf Expr.Tag {} -> []
  _ -> makeError
  where
    makeError =
      [ ( tagRef
        , makeRefExpr o0 . Expr.BodyLeaf . Expr.Tag $
          Guid.fromString "EXAMPLE"
        )
      ]

runDisallowTagTypeForApply :: ExprRef -> RefExpression def -> Origin3 -> RuleResult def
runDisallowTagTypeForApply applyValRef applyTypeExpr (o0, o1, o2) = do
  _ <- applyTypeExpr ^.. Expr.eBody . Expr._BodyLeaf . Expr._TagType
  makeError
  where
    makeError =
      [ ( applyValRef
        , makeRefExpr o0 . Expr.BodyApply $
          Expr.Apply (hole o1) (hole o2)
        )
      ]
    hole o = makeRefExpr o $ Expr.BodyLeaf Expr.Hole

applyToPartsRule ::
  TypedValue -> Expr.Apply TypedValue -> Rule def ExprRef
applyToPartsRule applyTv parts@(Expr.Apply func _) =
  ApplyToParts (tvVal <$> parts) (tvVal applyTv, tvVal func)

-- Apply's type may not be TagType
disallowTagTypeForApplyRule :: TypedValue -> State Origin (Rule def ExprRef)
disallowTagTypeForApplyRule (TypedValue valRef typRef) =
  DisallowTagTypeForApply valRef typRef <$> mkOrigin3

applyRules :: TypedValue -> Expr.Apply TypedValue -> State Origin [Rule def ExprRef]
applyRules applyTv apply@(Expr.Apply func arg) =
  -- TODO: make all of these functions have a standard signature and
  -- just apply them all to the same args?
  (++ pureRules) <$>
  sequenceA
  [ argTypeToPiParamTypeRule apply
  , rigidArgApplyTypeToResultTypeRule applyTv apply
  , argTypeToLambdaParamTypeRule apply
  , nonLambdaToApplyValueRule applyTv apply
  , disallowTagTypeForApplyRule applyTv
  ]
  where
    pureRules =
      [ piParamTypeToArgTypeRule apply
      , redexApplyTypeToResultTypeRule applyTv func
      , lambdaParamTypeToArgTypeRule apply
      , applyToPartsRule applyTv apply
      ]
      ++ recurseSubstRules Expr.Type
        (tvType applyTv) (tvType func) (tvVal arg)
      ++ recurseSubstRules Expr.Val
        (tvVal applyTv) (tvVal func) (tvVal arg)
