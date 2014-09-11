{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.Sugar.Convert
  ( convertDefI
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.IRef (DefIM)
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Monad (ConvertM, Context(..))
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import System.Random (RandomGen)
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.List as ConvertList
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.InputExpr as InputExpr
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes
import qualified Trash

onMatchingSubexprs ::
  MonadA m => (a -> m ()) -> (a -> Val () -> Bool) -> Val a -> m ()
onMatchingSubexprs action predicate =
  Lens.itraverseOf_ (ExprLens.subExprPayloads . Lens.ifiltered (flip predicate))
  (const action)

toHole :: MonadA m => Stored m -> T m ()
toHole = void . DataOps.setToHole

isGetParamOf :: V.Var -> Val a -> Bool
isGetParamOf = Lens.anyOf ExprLens.valVar . (==)

isGetFieldParam :: V.Var -> T.Tag -> Val a -> Bool
isGetFieldParam expectedRecordVar expectedTag =
  p . (^? ExprLens.valGetField)
  where
    p Nothing = False
    p (Just (V.GetField record tag)) =
      expectedTag == tag &&
      Lens.anyOf ExprLens.valVar (== expectedRecordVar) record

deleteParamRef ::
  MonadA m => V.Var -> Val (Stored m) -> T m ()
deleteParamRef = onMatchingSubexprs toHole . const . isGetParamOf

deleteFieldParamRef ::
  MonadA m => V.Var -> T.Tag -> Val (Stored m) -> T m ()
deleteFieldParamRef param tag =
  onMatchingSubexprs toHole . const $ isGetFieldParam param tag

lambdaWrap :: MonadA m => Stored m -> T m Guid
lambdaWrap stored =
  f <$> DataOps.lambdaWrap stored
  where
    f (newParam, newLamI) =
      Guid.combine (ExprIRef.valIGuid newLamI) $
      Trash.guidOfVar newParam

fakeExample :: V.Var -> ExpressionP name0 m0 (Payload m1 ())
fakeExample var =
  Expression (BodyAtom "NotImplemented") Payload
  { _plGuid = Guid.augment "EXAMPLE" $ Trash.guidOfVar var
  , _plInferredTypes = []
  , _plActions = Nothing
  , _plData = ()
  }

mkPositionalFuncParamActions ::
  MonadA m => V.Var -> Stored m -> Val (Stored m) ->
  FuncParamActions MStoredName m
mkPositionalFuncParamActions param lambdaProp body =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        SugarInfer.replaceWith lambdaProp $ body ^. V.payload
    , _itemAddNext = lambdaWrap $ body ^. V.payload
    }
  , _fpGetExample = pure $ fakeExample param
  }

getStoredNameS :: MonadA m => Guid -> ConvertM m MStoredName
getStoredNameS = ConvertM.liftTransaction . ConvertExpr.getStoredName

addFuncParamName ::
  MonadA m => FuncParam MStoredName f ->
  ConvertM m (FuncParam MStoredName f)
addFuncParamName fp = do
  name <- getStoredNameS $ fp ^. fpGuid
  pure fp { _fpName = name }

convertPositionalFuncParam ::
  (Typeable1 m, MonadA m, Monoid a) => V.Lam (InputExpr m a) ->
  InputPayload m a ->
  ConvertM m (FuncParam MStoredName m)
convertPositionalFuncParam (V.Lam param _paramTypeConstraint body) lamExprPl = do
  addFuncParamName FuncParam
    { _fpName = Nothing
    , _fpGuid = paramGuid
    , _fpVarKind = FuncParameter
    , _fpId = Guid.combine lamGuid paramGuid
    , _fpAltIds = [paramGuid] -- For easy jumpTo
    , _fpInferredType = mParamType
    , _fpMActions =
      mkPositionalFuncParamActions param
      <$> lamExprPl ^. ipStored
      <*> traverse (^. ipStored) body
    }
  where
    paramGuid = Trash.guidOfVar param
    mParamType = lamExprPl ^? ipInferred . Lens._Just . Infer.plType . ExprLens._TFun . _1
    lamGuid = lamExprPl ^. ipGuid

convertLam ::
  (MonadA m, Typeable1 m, Monoid a) =>
  V.Lam (InputExpr m a) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam paramVar _paramTypeConstraint result) exprPl = do
  param <- convertPositionalFuncParam lam exprPl
  resultS <- ConvertM.convertSubexpression result
  BodyLam
    Lam
    { _lParam =
        param
        & fpMActions .~ Nothing
    , _lResult = resultS
    }
    & ConvertExpr.make exprPl
    <&> rPayload . plActions . Lens._Just . mSetToInnerExpr .~ do
      guard $ Lens.nullOf ExprLens.valHole result
      bodyStored <- traverse (^. ipStored) result
      stored <- exprPl ^. ipStored
      return $ do
        deleteParamRef paramVar bodyStored
        ExprIRef.valIGuid <$>
          DataOps.setToWrapper (Property.value (bodyStored ^. V.payload)) stored

convertVar ::
  (MonadA m, Typeable1 m) => V.Var ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVar param exprPl = do
  recordParamsMap <- (^. ConvertM.scRecordParamsInfos) <$> ConvertM.readContext
  case Map.lookup param recordParamsMap of
    Just (ConvertM.RecordParamsInfo defGuid jumpTo) -> do
      defName <- getStoredNameS defGuid
      ConvertExpr.make exprPl $ BodyGetParams GetParams
        { _gpDefGuid = defGuid
        , _gpDefName = defName
        , _gpJumpTo = jumpTo
        }
    Nothing -> do
      parName <- getStoredNameS parGuid
      ConvertExpr.make exprPl .
        BodyGetVar $ GetVar
        { _gvName = parName
        , _gvIdentifier = parGuid
        , _gvJumpTo = pure parGuid
        , _gvVarType = GetParameter
        }
  where
    parGuid = Trash.guidOfVar param

jumpToDefI ::
  MonadA m => Anchors.CodeProps m -> DefIM m -> T m Guid
jumpToDefI cp defI = IRef.guid defI <$ DataOps.newPane cp defI

convertVLiteralInteger ::
  (MonadA m, Typeable1 m) => Integer ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVLiteralInteger i exprPl = ConvertExpr.make exprPl $ BodyLiteralInteger i

convertTag :: (MonadA m, Typeable1 m) => T.Tag -> ConvertM m (TagG MStoredName)
convertTag tag = TagG tag <$> getStoredNameS (Trash.guidOfTag tag)

convertAtom ::
  (MonadA m, Typeable1 m) => String ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertAtom str exprPl =
  ConvertExpr.make exprPl $ BodyAtom str

-- sideChannel :: Monad m => Lens' s a -> LensLike m s (side, s) a (side, a)
-- sideChannel lens f s = (`runStateT` s) . Lens.zoom lens $ StateT f

-- writeRecordFields ::
--   MonadA m =>
--   ExprIRef.ValIM m -> result ->
--   ( [(T.Tag, ExprIRef.ValIM m)] ->
--     T m
--     ( result
--     , [(T.Tag, ExprIRef.ValIM m)]
--     )
--   ) ->
--   T m result
-- writeRecordFields iref def f = do
--   oldBody <- ExprIRef.readValBody iref
--   case oldBody ^? V._VRec of
--     Nothing -> return def
--     Just oldRecord -> do
--       (res, newRecord) <- sideChannel V.recordFields f oldRecord
--       ExprIRef.writeExprBody iref $ V.VRec newRecord
--       return res

-- recordFieldActions ::
--   MonadA m => Guid -> ExprIRef.ValIM m -> ExprIRef.ValIM m ->
--   ListItemActions m
-- recordFieldActions defaultGuid exprIRef iref =
--   ListItemActions
--   { _itemDelete = action delete
--   , _itemAddNext = action addNext
--   }
--   where
--     action f = writeRecordFields iref defaultGuid $ splitFields f
--     addNext (prevFields, field, nextFields) = do
--       tagHole <- DataOps.newHole
--       exprHole <- DataOps.newHole
--       return
--         ( ExprIRef.valIGuid tagHole
--         , prevFields ++ field : (tagHole, exprHole) : nextFields
--         )
--     delete (prevFields, _, nextFields) =
--       return
--       ( case nextFields ++ reverse prevFields of
--         [] -> defaultGuid
--         ((nextTagExpr, _) : _) -> ExprIRef.valIGuid nextTagExpr
--       , prevFields ++ nextFields
--       )
--     splitFields f oldFields =
--       case break ((== exprIRef) . fst) oldFields of
--       (prevFields, field : nextFields) -> f (prevFields, field, nextFields)
--       _ -> return (defaultGuid, oldFields)

convertField ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Maybe (ExprIRef.ValIM m) -> Guid ->
  T.Tag -> InputExpr m a ->
  ConvertM m (RecordField MStoredName m (ExpressionU m a))
convertField _mIRef _defaultGuid tag expr = do
  tagS <- convertTag tag
  exprS <- ConvertM.convertSubexpression expr
  return RecordField
    { _rfMItemActions = error "TODO: rfMItemActions"
 --      recordFieldActions defaultGuid <$> tagExpr ^? SugarInfer.exprIRef <*> mIRef
    , _rfTag = tagS
    , _rfExpr = exprS
    }

convertEmptyRecord ::
  (Typeable1 m, MonadA m) => InputPayload m a -> ConvertM m (ExpressionU m a)
convertEmptyRecord exprPl =
  ConvertExpr.make exprPl $ BodyRecord
    Record
    { _rFields =
        FieldList
        { _flItems = []
        , _flMAddFirstItem = error "TODO: _flMAddFirstItem" -- addField <$> exprPl ^? SugarInfer.plIRef
        }
    }

convertRecExtend ::
  (Typeable1 m, MonadA m, Monoid a) =>
  V.RecExtend (InputExpr m a) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertRecExtend (V.RecExtend tag val rest) exprPl = do
  restS <- ConvertM.convertSubexpression rest
  fieldS <- convertField (exprPl ^? SugarInfer.plIRef) defaultGuid tag val
  case restS ^. rBody of
    BodyRecord (Record (FieldList restFields _mAddFirstAddItem)) ->
      ConvertExpr.make exprPl $ BodyRecord $ Record $
        FieldList (fieldS : restFields) $
        error "TODO: Support add first item on records"
    _ -> error "TODO: Support record extend of non-record"
  where
    defaultGuid = exprPl ^. ipGuid
    -- addField iref =
    --   writeRecordFields iref defaultGuid $ \recordFields -> do
    --     tag <- T.Tag <$> Transaction.newKey
    --     holeExpr <- DataOps.newHole
    --     return
    --       ( ExprIRef.valIGuid holeTagExpr
    --       , (tag, holeExpr) : recordFields
    --       )

convertGetField ::
  (MonadA m, Typeable1 m, Monoid a) =>
  V.GetField (InputExpr m a) ->
  InputPayload m a ->
  ConvertM m (ExpressionU m a)
convertGetField (V.GetField recExpr tag) exprPl = do
  tagParamInfos <- (^. ConvertM.scTagParamInfos) <$> ConvertM.readContext
  let
    mkGetVar jumpTo = do
      name <- getStoredNameS tagGuid
      pure GetVar
        { _gvName = name
        , _gvIdentifier = tagGuid
        , _gvJumpTo = pure jumpTo
        , _gvVarType = GetFieldParameter
        }
  mVar <- traverse mkGetVar $ do
    paramInfo <- Map.lookup tag tagParamInfos
    param <- recExpr ^? ExprLens.valVar
    guard $ param == ConvertM.tpiFromParameters paramInfo
    return $ ConvertM.tpiJumpTo paramInfo
  ConvertExpr.make exprPl =<<
    case mVar of
    Just var ->
      return $ BodyGetVar var
    Nothing -> do
      tName <- getStoredNameS $ Trash.guidOfTag tag
      traverse ConvertM.convertSubexpression
        GetField
        { _gfRecord = recExpr
        , _gfTag =
            TagG
            { _tagGId = tag
            , _tagGName = tName
            }
        }
        <&> BodyGetField
  where
    tagGuid = Trash.guidOfTag tag

removeRedundantSubExprTypes :: Expression n m a -> Expression n m a
removeRedundantSubExprTypes =
  rBody %~
  ( Lens.traversed %~ removeRedundantTypes
    & Lens.outside _BodyHole .~
      BodyHole . (Lens.traversed %~ removeRedundantSubExprTypes)
  ) .
  (_BodyApply . aFunc %~ remSuc) .
  (_BodyGetField . gfRecord %~ remSuc) .
  (_BodyLam . lResult %~ remSuc)
  where
    remSuc =
      Lens.filtered (Lens.nullOf (rBody . _BodyHole)) %~
      SugarRemoveTypes.successfulType

removeRedundantTypes :: Expression name m a -> Expression name m a
removeRedundantTypes =
  removeRedundantSubExprTypes .
  (Lens.filtered cond %~ SugarRemoveTypes.successfulType)
  where
    cond e =
      Lens.anyOf (rBody . _BodyGetVar) ((/= GetDefinition) . (^. gvVarType)) e ||
      Lens.has (rBody . _BodyRecord) e

convertGlobal ::
  (MonadA m, Typeable1 m) =>
  V.GlobalId -> InputPayload m a -> ConvertM m (ExpressionU m a)
convertGlobal globalId exprPl =
  runMatcherT $ do
    justToLeft $ ConvertList.nil globalId exprPl
    lift $ do
      cp <- (^. ConvertM.scCodeAnchors) <$> ConvertM.readContext
      defName <- getStoredNameS defGuid
      ConvertExpr.make exprPl .
        BodyGetVar $ GetVar
        { _gvName = defName
        , _gvIdentifier = defGuid
        , _gvJumpTo = jumpToDefI cp defI
        , _gvVarType = GetDefinition
        }
    where
      defI = ExprIRef.defI globalId
      defGuid = IRef.guid defI

convertExpressionI ::
  (Typeable1 m, MonadA m, Monoid a) =>
  InputExpr m a -> ConvertM m (ExpressionU m a)
convertExpressionI ee =
  ($ ee ^. V.payload) $
  case ee ^. V.body of
  V.BAbs x -> convertLam x
  V.BApp x -> ConvertApply.convert x
  V.BRecExtend x -> convertRecExtend x
  V.BGetField x -> convertGetField x
  V.BLeaf (V.LVar x) -> convertVar x
  V.BLeaf (V.LGlobal x) -> convertGlobal x
  V.BLeaf (V.LLiteralInteger x) -> convertVLiteralInteger x
  V.BLeaf V.LHole -> ConvertHole.convert
  V.BLeaf V.LRecEmpty -> convertEmptyRecord

mkContext ::
  (MonadA m, Typeable1 m) =>
  Anchors.Code (Transaction.MkProperty m) (Tag m) ->
  Infer.Context -> T m (Context m)
mkContext cp inferContext = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scInferContext = inferContext
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scTagParamInfos = mempty
    , _scRecordParamsInfos = mempty
    , scConvertSubexpression = convertExpressionI
    }

convertExpressionPure ::
  (MonadA m, Typeable1 m, RandomGen g, Monoid a) =>
  Anchors.CodeProps m -> g ->
  Val a -> T m (ExpressionU m a)
convertExpressionPure cp gen res = do
  context <- mkContext cp (err "InferContext")
  fmap removeRedundantTypes .
    ConvertM.run context .
    ConvertM.convertSubexpression $
    InputExpr.makePure gen res
  where
    err x = error $ "convertExpressionPure: " ++ x

data ConventionalParams m a = ConventionalParams
  { cpTags :: [Guid]
  , cpParamInfos :: Map Guid ConvertM.TagParamInfo
  , cpRecordParamsInfos :: Map V.Var (ConvertM.RecordParamsInfo m)
  , cpParams :: [FuncParam MStoredName m]
  , cpAddFirstParam :: T m Guid
  , cpHiddenPayloads :: [InputPayload m a]
  }

data FieldParam = FieldParam
  { fpTagGuid :: Guid
  , fpFieldType :: Type
  }

mkRecordParams ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> V.Var -> [FieldParam] ->
  InputExpr m a ->
  Maybe (Val (Stored m)) ->
  ConvertM m (ConventionalParams m a)
mkRecordParams recordParamsInfo param fieldParams lambdaExprI mBodyStored = do
  params <- traverse mkParam fieldParams
  pure ConventionalParams
    { cpTags = fpTagGuid <$> fieldParams
    , cpParamInfos = mconcat $ mkParamInfo <$> fieldParams
    , cpRecordParamsInfos = Map.singleton param recordParamsInfo
    , cpParams = params
    , cpAddFirstParam =
      error "TODO cpAddFirstParam"--  addFirstFieldParam lamGuid $
      -- fromMaybe (error "Record param type must be stored!") mParamTypeI
    , cpHiddenPayloads = [lambdaExprI ^. V.payload]
    }
  where
    lamGuid = lambdaExprI ^. SugarInfer.exprGuid
    mLambdaP = lambdaExprI ^. V.payload . ipStored
    fpIdGuid = Guid.combine lamGuid . fpTagGuid
    mkParamInfo fp =
      Map.singleton (fpTagGuid fp) . ConvertM.TagParamInfo param $ fpIdGuid fp
    mkParam fp = do
      let guid = fpTagGuid fp
      addFuncParamName FuncParam
        { _fpName = Nothing
        , _fpGuid = guid
        , _fpId = -- TOOD: Is this supposed to be the same?
                  -- It used to be different: "Guid.combine lamGuid guid"
                  fpIdGuid fp
        , _fpAltIds = [] -- TODO: fpAltIds still needed?
        , _fpVarKind = FuncFieldParameter
        , _fpInferredType = Just $ fpFieldType fp
        , _fpMActions = error "TODO: _fpMActions"
          -- fpActions (fpIdGuid fp)
          -- <$> mLambdaP <*> mParamTypeI <*> mBodyStored
        }
--     fpActions tagExprGuid lambdaP paramTypeI bodyStored =
--       FuncParamActions
--       { _fpListItemActions = ListItemActions
--         { _itemAddNext = error "TODO:add" -- addFieldParamAfter lamGuid tagExprGuid paramTypeI
--         , _itemDelete =
--           error "TODO:del"
-- --          delFieldParam tagExprGuid paramTypeI param lambdaP bodyStored
--         }
--       , _fpGetExample = pure $ fakeExample tagExprGuid
--       }

type ExprField m = (T.Tag, ExprIRef.ValIM m)
-- rereadFieldParamTypes ::
--   MonadA m =>
--   Guid -> ExprIRef.ValIM m ->
--   ([ExprField m] -> ExprField m -> [ExprField m] -> T m Guid) ->
--   T m Guid
-- rereadFieldParamTypes tagExprGuid paramTypeI f = do
--   paramType <- ExprIRef.readValBody paramTypeI
--   let
--     mBrokenFields =
--       paramType ^? V._VRec . ExprLens.kindedRecordFields KType .
--       (Lens.to . break) ((T.Tag tagExprGuid ==) . fst)
--   case mBrokenFields of
--     Just (prevFields, theField : nextFields) -> f prevFields theField nextFields
--     _ -> return tagExprGuid

-- rewriteFieldParamTypes ::
--   MonadA m => ExprIRef.ValIM m -> [ExprField m] -> T m ()
-- rewriteFieldParamTypes paramTypeI fields =
--   ExprIRef.writeExprBody paramTypeI . V.VRec $
--   V.Record KType fields

-- addFieldParamAfter :: MonadA m => Guid -> Guid -> ExprIRef.ValIM m -> T m Guid
-- addFieldParamAfter lamGuid tagExprGuid paramTypeI =
--   rereadFieldParamTypes tagExprGuid paramTypeI $
--   \prevFields theField nextFields -> do
--     fieldGuid <- Transaction.newKey
--     holeTypeI <- DataOps.newHole
--     rewriteFieldParamTypes paramTypeI $
--       prevFields ++ theField : (T.Tag fieldGuid, holeTypeI) : nextFields
--     pure $ Guid.combine lamGuid fieldGuid

-- delFieldParam ::
--   MonadA m => Guid -> ExprIRef.ValIM m -> Guid ->
--   Stored m -> Val (Stored m) -> T m Guid
-- delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored =
--   rereadFieldParamTypes tagExprGuid paramTypeI $
--   \prevFields ((T.Tag tagG), _) nextFields -> do
--     deleteFieldParamRef paramGuid tagG bodyStored
--     case prevFields ++ nextFields of
--       [] -> error "We were given fewer than 2 field params, which should never happen"
--       [(T.Tag fieldTagGuid, fieldTypeI)] -> do
--         ExprIRef.writeExprBody (Property.value lambdaP) $
--           ExprUtil.makeLambda fieldTagGuid fieldTypeI bodyI
--         deleteParamRef paramGuid bodyStored
--         let
--           toGetParam iref =
--             ExprIRef.writeExprBody iref $
--             ExprLens.bodyParameterRef # fieldTagGuid
--         onMatchingSubexprs (toGetParam . Property.value)
--           (isGetFieldParam paramGuid fieldTagGuid) bodyStored
--         pure $ Guid.combine lamGuid fieldTagGuid
--       newFields -> do
--         rewriteFieldParamTypes paramTypeI newFields
--         pure $ dest prevFields nextFields
--   where
--     lamGuid = ExprIRef.valIGuid $ Property.value lambdaP
--     bodyI = bodyStored ^. V.payload . Property.pVal
--     dest prevFields nextFields =
--       fromMaybe (ExprIRef.valIGuid bodyI) . listToMaybe $
--       map (getParamGuidFromTagExprI . fst) $ nextFields ++ reverse prevFields
--     getParamGuidFromTagExprI (T.Tag tGuid) = Guid.combine lamGuid tGuid

convertDefinitionParams ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> [Guid] -> InputExpr m a ->
  ConvertM m
  ( [FuncParam MStoredName m]
  , ConventionalParams m a
  , InputExpr m a
  )
convertDefinitionParams recordParamsInfo usedTags expr =
  case expr ^. V.body of
  V.BAbs lambda@(V.Lam paramGuid paramType body) -> do
    param <- convertPositionalFuncParam lambda (expr ^. V.payload)
    case schemeType paramType of
      T.TRecord composite
        | Nothing <- extension
        , ListUtils.isLengthAtLeast 2 fields
        , all ((`notElem` usedTags) . fpTagGuid) fieldParams -> do
          convParams <-
            mkRecordParams recordParamsInfo paramGuid fieldParams
            expr (traverse (^. ipStored) body)
          return ([], convParams, body)
        where
          FlatComposite fields extension = FlatComposite.fromComposite composite
          fieldParams = map makeFieldParam fields
      _ ->
        pure
        ( []
        , singleConventionalParam stored param paramGuid paramType body
        , body
        )
  _ -> return ([], emptyConventionalParams stored, expr)
  where
    stored =
      fromMaybe (error "Definition body is always stored!") $
      expr ^. SugarInfer.exprStored
    makeFieldParam (T.Tag fieldTagGuid, typeExpr) =
      FieldParam
      { fpTagGuid = fieldTagGuid
      , fpFieldType = typeExpr
      }

singleConventionalParam ::
  MonadA m =>
  Stored m -> FuncParam MStoredName m ->
  Guid -> InputExpr m a -> InputExpr m a -> ConventionalParams m a
singleConventionalParam _lamProp existingParam existingParamGuid existingParamType body =
  ConventionalParams
  { cpTags = []
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams =
    [ existingParam & fpMActions . Lens._Just . fpListItemActions . itemAddNext .~
      addSecondParam (\old new -> [old, new])
    ]
  , cpAddFirstParam = addSecondParam (\old new -> [new, old])
  , cpHiddenPayloads = []
  }
  where
    _existingParamTag = T.Tag existingParamGuid
    _existingParamTypeIRef =
      fromMaybe (error "Only stored record param type is converted as record") $
      existingParamType ^? SugarInfer.exprIRef
    _bodyWithStored =
      fromMaybe (error "Definition body should be stored") $
      traverse (^. ipStored) body
    addSecondParam _mkFields = error "TODO: addSecondParam"
--     addSecondParam mkFields = do
--       let existingParamField = (existingParamTag, existingParamTypeIRef)
--       (newTag, newParamField) <- newField
--       newParamTypeI <-
--         ExprIRef.newExprBody . V.VRec . V.Record KType $
--         mkFields existingParamField newParamField
--       newParamsGuid <- Transaction.newKey
--       ExprIRef.writeExprBody (Property.value lamProp) $
--         ExprUtil.makeLambda newParamsGuid newParamTypeI .
--         Property.value $ bodyWithStored ^. V.payload
--       let
--         toGetField iref = do
--           recordRef <- ExprIRef.newExprBody $ ExprLens.bodyParameterRef # newParamsGuid
--           tagRef <- ExprIRef.newExprBody existingParamTag
--           ExprIRef.writeExprBody iref $
--             V.VGetField V.GetField
--             { V._getFieldRecord = recordRef
--             , V._getFieldTag = tagRef
--             }
--       onMatchingSubexprs (toGetField . Property.value)
--         (isGetParamOf existingParamGuid) bodyWithStored
--       let lamGuid = ExprIRef.valIGuid $ Property.value lamProp
--       pure $ Guid.combine lamGuid $ newTag ^. Lens.from V.tag

emptyConventionalParams :: MonadA m => ExprIRef.ValIProperty m -> ConventionalParams m a
emptyConventionalParams stored = ConventionalParams
  { cpTags = []
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams = []
  , cpAddFirstParam = lambdaWrap stored
  , cpHiddenPayloads = []
  }

data ExprWhereItem a = ExprWhereItem
  { ewiBody :: Val a
  , ewiParamGuid :: Guid
  , ewiArg :: Val a
  , ewiHiddenPayloads :: [a]
  , ewiInferredType :: Maybe Type
  }

isUnconstrained :: Scheme -> Bool
isUnconstrained (Scheme tvs constraints (T.TVar tv))
  | constraints == mempty
  , tv `TypeVars.member` tvs = True
  | otherwise = False

mExtractWhere :: InputExpr m a -> Maybe (ExprWhereItem (InputPayload m a))
mExtractWhere expr = do
  V.Apply func arg <- expr ^? ExprLens.valApply
  V.Lam paramGuid paramTypeConstraint body <- func ^? V.body . ExprLens._BAbs
  -- paramType has to be Hole for this to be sugarred to Where
  guard $ isUnconstrained paramTypeConstraint
  Just ExprWhereItem
    { ewiBody = body
    , ewiParamGuid = paramGuid
    , ewiArg = arg
    , ewiHiddenPayloads = (^. V.payload) <$> [expr, func, paramTypeConstraint]
    , ewiInferredType = arg ^? V.payload . ipInferred . Lens._Just . Infer.plType
    }

convertWhereItems ::
  (MonadA m, Typeable1 m, Monoid a) =>
  [Guid] ->
  InputExpr m a ->
  ConvertM m ([WhereItem MStoredName m (ExpressionU m a)], InputExpr m a)
convertWhereItems usedTags expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just ewi -> do
    let
      defGuid = ewiParamGuid ewi
      recordParamsInfo =
        ConvertM.RecordParamsInfo defGuid $ pure defGuid
    value <-
      convertDefinitionContent recordParamsInfo usedTags $
      ewiArg ewi
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef defGuid bodyStored
             SugarInfer.replaceWith topLevelProp $ bodyStored ^. V.payload
        , _itemAddNext = fst <$> DataOps.redexWrap topLevelProp
        }
    name <- getStoredNameS defGuid
    let
      hiddenData = ewiHiddenPayloads ewi ^. Lens.traversed . ipData
      item = WhereItem
        { _wiValue =
            value
            & dBody . rPayload . plData <>~ hiddenData
        , _wiGuid = defGuid
        , _wiActions =
            mkWIActions <$>
            expr ^. SugarInfer.exprStored <*>
            traverse (^. ipStored) (ewiBody ewi)
        , _wiName = name
        , _wiInferredType = ewiInferredType ewi
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ ewiBody ewi
    return (item : nextItems, whereBody)

newField :: MonadA m => T m (T.Tag, ExprIRef.ValIM m)
newField = (,) <$> (T.Tag <$> Transaction.newKey) <*> DataOps.newHole

-- addFirstFieldParam :: MonadA m => Guid -> ExprIRef.ValIM m -> T m Guid
-- addFirstFieldParam lamGuid recordI = do
--   recordBody <- ExprIRef.readValBody recordI
--   case recordBody ^? V._VRec . ExprLens.kindedRecordFields KType of
--     Just fields -> do
--       (newTag, field) <- newField
--       ExprIRef.writeExprBody recordI $
--         V.VRec . V.Record KType $ (newTag, field) : fields
--       pure $ Guid.combine lamGuid $ newTag ^. Lens.from V.tag
--     _ -> pure $ ExprIRef.valIGuid recordI

assertedGetProp :: String -> Val (InputPayloadP inferred (Maybe stored) a) -> stored
assertedGetProp _ Val (InputPayload { _ipStored = Just prop }) _ = prop
assertedGetProp msg _ = error msg

convertDefinitionContent ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> [Guid] -> InputExpr m a ->
  ConvertM m (DefinitionContent MStoredName m (ExpressionU m a))
convertDefinitionContent recordParamsInfo usedTags expr = do
  (depParams, convParams, funcBody) <-
    convertDefinitionParams recordParamsInfo usedTags expr
  ConvertM.local
    ((ConvertM.scTagParamInfos <>~ cpParamInfos convParams) .
     (ConvertM.scRecordParamsInfos <>~ cpRecordParamsInfos convParams)) $ do
      (whereItems, whereBody) <-
        convertWhereItems (usedTags ++ cpTags convParams) funcBody
      bodyS <- ConvertM.convertSubexpression whereBody
      return DefinitionContent
        { _dDepParams = depParams
        , _dParams = cpParams convParams
        , _dBody =
          bodyS
          & rPayload . plData <>~
            cpHiddenPayloads convParams ^. Lens.traversed . ipData
        , _dWhereItems = whereItems
        , _dAddFirstParam = cpAddFirstParam convParams
        , _dAddInnermostWhereItem =
          fmap fst . DataOps.redexWrap $
          assertedGetProp "Where must be stored" whereBody
        }

convertDefIBuiltin ::
  (Typeable1 m, MonadA m) => Anchors.CodeProps m ->
  Definition.Builtin -> DefIM m ->
  Val (Stored m) ->
  T m (DefinitionBody MStoredName m (ExpressionU m [Guid]))
convertDefIBuiltin cp (Definition.Builtin name) defI defType =
  DefinitionBodyBuiltin <$> do
    defTypeS <-
      defType
      & void
      & convertExpressionPure cp iDefTypeGen
      <&> Lens.mapped . Lens.mapped .~ mempty
    pure DefinitionBuiltin
      { biName = name
      , biMSetName = Just setName
      , biType = defTypeS
      }
  where
    defGuid = IRef.guid defI
    iDefTypeGen = ConvertExpr.mkGen 1 3 defGuid
    typeIRef = Property.value (defType ^. V.payload)
    setName =
      Transaction.writeIRef defI .
      (`Definition.Body` typeIRef) .
      Definition.ContentBuiltin . Definition.Builtin

makeTypeInfo :: MonadA m => Type -> Type -> DefinitionTypeInfo m
makeTypeInfo defType inferredType
  | defType `Scheme.alphaEq` inferredType = DefinitionExportedTypeInfo defType
  | otherwise =
      DefinitionNewType AcceptNewType
        { antOldType = defType
        , antNewType = inferredType
        , antAccept =
          inferredType
          & void
          & ExprIRef.newVal
          >>= Property.set (defType ^. V.payload . _1)
        }

convertDefIExpr ::
  (MonadA m, Typeable1 m) => Anchors.CodeProps m ->
  Val (Load.ExprPropertyClosure (Tag m)) ->
  DefIM m ->
  Val (Stored m) ->
  T m (DefinitionBody MStoredName m (ExpressionU m [Guid]))
convertDefIExpr cp exprLoaded defI defType = do
  (valInferred, newInferContext) <-
    SugarInfer.loadInfer $
    exprLoaded <&> Load.exprPropertyOfClosure
  let
    addStoredGuids lens x = (x, ExprIRef.valIGuid . Property.value <$> x ^.. lens)
    guidIntoPl (pl, x) = pl & ipData %~ \() -> x
  context <- lift $ mkContext cp newInferContext
  ConvertM.run context $ do
    content <-
      valInferred
      <&> guidIntoPl . addStoredGuids (ipStored . Lens._Just)
      & traverse . ipInferred %~ Just
      & convertDefinitionContent recordParamsInfo []
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = removeRedundantTypes <$> content
      , _deTypeInfo =
        makeTypeInfo defType $ valInferred ^. V.payload . _1 . Infer.plType
      }
  where
    defGuid = IRef.guid defI
    recordParamsInfo = ConvertM.RecordParamsInfo defGuid $ jumpToDefI cp defI

convertDefI ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m ->
  -- TODO: Use DefinitionClosure?
  Definition.Definition
    (Val (Load.ExprPropertyClosure (Tag m)))
    (ExprIRef.DefIM m) ->
  T m (Definition (Maybe String) m (ExpressionU m [Guid]))
convertDefI cp (Definition.Definition (Definition.Body bodyContent typeLoaded) defI) = do
  bodyS <- convertDefContent bodyContent $ Load.exprPropertyOfClosure <$> typeLoaded
  name <- lift $ ConvertExpr.getStoredName defGuid
  return Definition
    { _drGuid = defGuid
    , _drName = name
    , _drBody = bodyS
    }
  where
    defGuid = IRef.guid defI
    convertDefContent (Definition.ContentBuiltin builtin) =
      convertDefIBuiltin cp builtin defI
    convertDefContent (Definition.ContentExpr exprLoaded) =
      convertDefIExpr cp exprLoaded defI
