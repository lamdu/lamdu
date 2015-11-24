{-# LANGUAGE LambdaCase, NoImplicitPrelude, FlexibleContexts, TypeFamilies, RankNTypes, RecordWildCards #-}
module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , InTransaction(..)
    , NameConvertor, CPSNameConvertor
    , OldExpression, NewExpression
    , toDef, toExpression, toBody
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Sugar.Names.CPS (CPS(..))
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Types

import           Prelude.Compat

type T = Transaction

type CPSNameConvertor m = OldName m -> CPS m (NewName m)
type NameConvertor m = OldName m -> m (NewName m)

newtype InTransaction m tm = InTransaction (forall a. m a -> T tm a)

-- TODO: Rename MonadNameWalk
class (MonadA m, MonadA (TM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type TM m :: * -> *
    opRun :: m (InTransaction m (TM m))

    opWithParamName :: NameGen.VarInfo -> CPSNameConvertor m
    opWithLetName :: NameGen.VarInfo -> CPSNameConvertor m
    opWithDefName :: CPSNameConvertor m
    opWithTagName :: CPSNameConvertor m
    opGetDefName :: NameConvertor m
    opGetTagName :: NameConvertor m
    opGetTIdName :: NameConvertor m
    opGetParamName :: NameConvertor m
    opGetHiddenParamsName :: NameConvertor m

type OldExpression m a = Expression (OldName m) (TM m) a
type NewExpression m a = Expression (NewName m) (TM m) a

isFunctionType :: Type -> NameGen.VarInfo
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NormalVar

toHoleResult ::
    MonadNaming m =>
    HoleResult (OldName m) (TM m) ->
    m (HoleResult (NewName m) (TM m))
toHoleResult = holeResultConverted toExpression

toHoleOption ::
    MonadNaming m =>
    HoleOption (OldName m) (TM m) ->
    m (HoleOption (NewName m) (TM m))
toHoleOption option@HoleOption{..} =
    do
        InTransaction run <- opRun
        pure option
            { _hoSugaredBaseExpr = _hoSugaredBaseExpr >>= run . toExpression
            , _hoResults = _hoResults <&> second (>>= run . toHoleResult)
            }
    where
        {-# INLINE second #-}
        second f (x, y) = (x, f y)

toHoleActions ::
    MonadNaming m =>
    HoleActions (OldName m) (TM m) ->
    m (HoleActions (NewName m) (TM m))
toHoleActions ha@HoleActions {..} =
    do
        InTransaction run <- opRun
        pure ha
            { _holeOptions = _holeOptions >>= run . traverse toHoleOption
            , _holeOptionLiteralNum =
                _holeOptionLiteralNum <&> (>>= run . toHoleOption)
            }

toNamedVar ::
    MonadNaming m =>
    NamedVar (OldName m) (TM m) ->
    m (NamedVar (NewName m) (TM m))
toNamedVar namedVar =
    (nvNameRef . nrName) f namedVar
    where
        f = case namedVar ^. nvVarType of
            GetParameter      -> opGetParamName
            GetFieldParameter -> opGetTagName
            GetDefinition     -> opGetDefName

toGetVar ::
    MonadNaming m =>
    GetVar (OldName m) (TM m) ->
    m (GetVar (NewName m) (TM m))
toGetVar (GetVarNamed x) = GetVarNamed <$> toNamedVar x
toGetVar (GetVarParamsRecord x) = GetVarParamsRecord <$> traverse opGetTagName x

toLet ::
    MonadNaming m => (a -> m b) ->
    Let (OldName m) (TM m) a ->
    m (Let (NewName m) (TM m) b)
toLet expr item@Let{..} =
    do
        (name, body) <-
            runCPS (opWithLetName (isFunctionType (_lAnnotation ^. aInferredType)) _lName) $
            toBinderBody expr _lBody
        value <- toBinder expr _lValue
        item { _lValue = value, _lName = name, _lBody = body } & pure

toBinderContent ::
    MonadNaming m => (a -> m b) ->
    BinderContent (OldName m) (TM m) a ->
    m (BinderContent (NewName m) (TM m) b)
toBinderContent expr (BinderLet l) = toLet expr l <&> BinderLet
toBinderContent expr (BinderExpr e) = expr e <&> BinderExpr

toBinderBody ::
    MonadNaming m => (a -> m b) ->
    BinderBody (OldName m) (TM m) a ->
    m (BinderBody (NewName m) (TM m) b)
toBinderBody expr = bbContent %%~ toBinderContent expr

toBinder ::
    MonadNaming m => (a -> m b) ->
    Binder (OldName m) (TM m) a ->
    m (Binder (NewName m) (TM m) b)
toBinder expr binder@Binder{..} =
    do
        (params, body) <-
            runCPS (withBinderParams _bParams) $ toBinderBody expr _bBody
        binder
            { _bParams = params
            , _bBody = body
            } & pure

toLam ::
    MonadNaming m => (a -> m b) ->
    Lambda (OldName m) (TM m) a ->
    m (Lambda (NewName m) (TM m) b)
toLam = lamBinder . toBinder

toBody ::
    MonadNaming m => (a -> m b) ->
    Body (OldName m) (TM m) a ->
    m (Body (NewName m) (TM m) b)
toBody expr = \case
    BodyList       x -> traverse expr x <&> BodyList
    BodyGetField   x -> traverse expr x >>= gfTag toTagG <&> BodyGetField
    BodyInject     x -> traverse expr x >>= iTag toTagG <&> BodyInject
    BodyRecord     x -> traverse expr x >>= (rItems . traverse . rfTag) toTagG <&> BodyRecord
    BodyCase       x -> traverse expr x >>= (cAlts . traverse . caTag) toTagG <&> BodyCase
    BodyApply      x -> traverse expr x >>= (aAnnotatedArgs . traverse . aaTag) toTagG <&> BodyApply
    BodyHole       x -> traverse expr x >>= (holeMActions . Lens._Just) toHoleActions <&> BodyHole
    BodyToNom      x -> traverse expr x >>= nTId toTIdG <&> BodyToNom
    BodyFromNom    x -> traverse expr x >>= nTId toTIdG <&> BodyFromNom
    BodyGetVar     x -> toGetVar x <&> BodyGetVar
    BodyLiteralNum x -> pure $ BodyLiteralNum x
    BodyLam        x -> toLam expr x <&> BodyLam
    where
        toTagG = tagGName opGetTagName
        toTIdG = tidgName %%~ opGetTIdName

toExpression :: MonadNaming m => OldExpression m a -> m (NewExpression m a)
toExpression = rBody (toBody toExpression)

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) (TM m) -> CPS m (BinderParams (NewName m) (TM m))
withBinderParams DefintionWithoutParams = pure DefintionWithoutParams
withBinderParams (NullParam a) = pure (NullParam a)
withBinderParams (VarParam fp) =
    opWithParamName (isFunctionType (fp ^. fpAnnotation . aInferredType))
    (fp ^. fpInfo . npiName)
    <&> VarParam . \newName -> fp & fpInfo . npiName .~ newName
withBinderParams (FieldParams xs) = onTagParams xs <&> FieldParams

onTagParams ::
    MonadNaming m =>
    [(T.Tag, FuncParam (NamedParamInfo (OldName m) (TM m)))] ->
    CPS m [(T.Tag, FuncParam (NamedParamInfo (NewName m) (TM m)))]
onTagParams =
    (traverse . second . fpInfo . npiName) opWithTagName
    where
        second f (x, y) = (,) x <$> f y

toDefinitionBody ::
    MonadNaming m => (a -> m b) ->
    DefinitionBody (OldName m) (TM m) a ->
    m (DefinitionBody (NewName m) (TM m) b)
toDefinitionBody _ (DefinitionBodyBuiltin bi) = pure (DefinitionBodyBuiltin bi)
toDefinitionBody f (DefinitionBodyExpression (DefinitionExpression typeInfo content)) =
     toBinder f content
     <&> DefinitionExpression typeInfo
     <&> DefinitionBodyExpression

toDef ::
    MonadNaming m => (a -> m b) ->
    Definition (OldName m) (TM m) a ->
    m (Definition (NewName m) (TM m) b)
toDef f def@Definition {..} =
    do
        (name, body) <- runCPS (opWithDefName _drName) $ toDefinitionBody f _drBody
        pure def { _drName = name, _drBody = body }
