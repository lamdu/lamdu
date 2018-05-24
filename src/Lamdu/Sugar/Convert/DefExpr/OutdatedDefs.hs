module Lamdu.Sugar.Convert.DefExpr.OutdatedDefs
    ( scan
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import           Lamdu.Calc.Type.Scheme (Scheme, schemeType, alphaEq)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import qualified Lamdu.Sugar.Convert.Type as ConvertType
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data IsHoleArg = IsHoleArg | NotHoleArg deriving Eq

argToHoleFunc :: Lens.Traversal' (Val a) (Val a)
argToHoleFunc =
    ExprLens.valApply .
    Lens.filtered (Lens.has (V.applyFunc . ExprLens.valHole)) .
    V.applyArg

recursivelyFixExpr ::
    Monad m =>
    (IsHoleArg -> Val a -> Maybe ((IsHoleArg -> Val a -> m ()) -> m ())) ->
    Val a -> m ()
recursivelyFixExpr mFix =
    go NotHoleArg
    where
        go isHoleArg expr =
            case mFix isHoleArg expr of
            Just fix -> fix go
            Nothing -> recurse expr
        recurse val =
            case val ^? argToHoleFunc of
            Just arg -> go IsHoleArg arg
            Nothing -> traverse_ (go NotHoleArg) (val ^. Val.body)

changeFuncRes :: Monad m => V.Var -> Val (ValIProperty m) -> T m ()
changeFuncRes usedDefVar =
    recursivelyFixExpr mFix
    where
        mFix NotHoleArg (Val pl (V.BLeaf (V.LVar v)))
            | v == usedDefVar = DataOps.applyHoleTo pl & void & const & Just
        mFix isHoleArg
            (Val pl (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) arg)))
            | v == usedDefVar =
            Just $
            \go ->
            do
                when (isHoleArg == NotHoleArg) (void (DataOps.applyHoleTo pl))
                go NotHoleArg arg
        mFix _ _ = Nothing

-- | Only if hole not already applied to it
applyHoleTo :: Monad m => Val (ValIProperty m) -> T m ()
applyHoleTo val
    | Lens.has argToHoleFunc val
    || Lens.has ExprLens.valHole val = pure ()
    | otherwise = val ^. Val.payload & DataOps.applyHoleTo & void

data RecordChange = RecordChange
    { fieldsAdded :: Set T.Tag
    , fieldsRemoved :: Set T.Tag
    , fieldsChanged :: Map T.Tag ArgChange
    }

data ArgChange = ArgChange | ArgRecordChange RecordChange

changeFuncArg :: Monad m => ArgChange -> V.Var -> Val (ValIProperty m) -> T m ()
changeFuncArg change usedDefVar =
    recursivelyFixExpr mFix
    where
        mFix NotHoleArg (Val pl (V.BLeaf (V.LVar v)))
            | v == usedDefVar = DataOps.applyHoleTo pl & void & const & Just
        mFix _ (Val _ (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) arg)))
            | v == usedDefVar = fixArg change arg & Just
        mFix _ _ = Nothing
        fixArg ArgChange arg go =
            do
                applyHoleTo arg
                go IsHoleArg arg
        fixArg (ArgRecordChange recChange) arg go =
            ( if Set.null (fieldsRemoved recChange)
                then
                    arg ^. Val.payload . Property.pVal
                    <$ changeFields (fieldsChanged recChange) arg go
                else
                    do
                        go IsHoleArg arg
                        V.Apply
                            <$> DataOps.newHole
                            ?? arg ^. Val.payload . Property.pVal
                            <&> V.BApp
                            >>= ExprIRef.newValBody
            )
            >>= addFields (fieldsAdded recChange)
            >>= arg ^. Val.payload . Property.pSet
        addFields fields src = foldM addField src fields
        addField src tag =
            V.RecExtend tag <$> DataOps.newHole ?? src
            <&> V.BRecExtend
            >>= ExprIRef.newValBody
        changeFields changes arg go
            | Map.null changes = go NotHoleArg arg
            | otherwise =
                case arg ^. Val.body of
                V.BRecExtend (V.RecExtend tag fieldVal rest) ->
                    case Map.lookup tag changes of
                    Nothing ->
                        do
                            go NotHoleArg fieldVal
                            changeFields changes rest go
                    Just fieldChange ->
                        do
                            fixArg fieldChange fieldVal go
                            changeFields (Map.delete tag changes) rest go
                _ -> fixArg ArgChange arg go

isPartSame ::
    Lens.Getting (Monoid.First T.Type) T.Type T.Type -> Scheme -> Scheme -> Bool
isPartSame part preType newType =
    do
        prePart <- preType & schemeType %%~ (^? part)
        newPart <- newType & schemeType %%~ (^? part)
        alphaEq prePart newPart & guard
    & Lens.has Lens._Just

argChangeType :: Scheme -> Scheme -> ArgChange
argChangeType prevArg newArg =
    do
        prevProd <- prevArg ^? schemeType . T._TRecord <&> FlatComposite.fromComposite
        prevProd ^? FlatComposite.extension . Lens._Nothing
        newProd <- newArg ^? schemeType . T._TRecord <&> FlatComposite.fromComposite
        newProd ^? FlatComposite.extension . Lens._Nothing
        let prevTags = prevProd ^. FlatComposite.fields & Map.keysSet
        let newTags = newProd ^. FlatComposite.fields & Map.keysSet
        let changedTags =
                Map.intersectionWith fieldChange
                (prevProd ^. FlatComposite.fields)
                (newProd ^. FlatComposite.fields)
                & Map.mapMaybe id
        ArgRecordChange RecordChange
            { fieldsAdded = Set.difference newTags prevTags
            , fieldsRemoved = Set.difference prevTags newTags
            , fieldsChanged = changedTags
            }
            & pure
    & fromMaybe ArgChange
    where
        fieldChange prevField newField
            | alphaEq prevFieldScheme newFieldScheme = Nothing
            | otherwise = argChangeType prevFieldScheme newFieldScheme & Just
            where
                prevFieldScheme = prevArg & schemeType .~ prevField
                newFieldScheme = newArg & schemeType .~ newField

fixDefExpr ::
    Monad m => Scheme -> Scheme -> V.Var -> Val (ValIProperty m) -> T m ()
fixDefExpr prevType newType usedDefVar defExpr =
    ( -- Function result changed (arg is the same).
        changeFuncRes usedDefVar defExpr <$
        guard (isPartSame (T._TFun . _1) prevType newType)
    ) <|>
    do
        isPartSame (T._TFun . _2) prevType newType & guard
        -- Function arg changed (result is the same).
        prevArg <- prevType & schemeType %%~ (^? T._TFun . _1)
        newArg <- newType & schemeType %%~ (^? T._TFun . _1)
        changeFuncArg (argChangeType prevArg newArg) usedDefVar defExpr & pure
    & fromMaybe (SubExprs.onGetVars (DataOps.applyHoleTo <&> void) usedDefVar defExpr)

updateDefType ::
    Monad m =>
    Scheme -> Scheme -> V.Var ->
    Def.Expr (Val (ValIProperty m)) -> (Def.Expr (ValI m) -> T m ()) ->
    T m PostProcess.Result ->
    T m ()
updateDefType prevType newType usedDefVar defExpr setDefExpr typeCheck =
    do
        defExpr
            <&> (^. Val.payload . Property.pVal)
            & Def.exprFrozenDeps . Infer.depsGlobalTypes . Lens.at usedDefVar ?~ newType
            & setDefExpr
        x <- typeCheck
        case x of
            PostProcess.GoodExpr -> pure ()
            PostProcess.BadExpr{} -> fixDefExpr prevType newType usedDefVar (defExpr ^. Def.expr)

scan ::
    Monad m =>
    EntityId -> Def.Expr (Val (ValIProperty m)) ->
    (Def.Expr (ValI m) -> T m ()) ->
    T m PostProcess.Result ->
    T m (Map V.Var (Sugar.DefinitionOutdatedType InternalName (T m ())))
scan entityId defExpr setDefExpr typeCheck =
    defExpr ^. Def.exprFrozenDeps . Infer.depsGlobalTypes
    & Map.toList & traverse (uncurry scanDef) <&> mconcat
    where
        scanDef globalVar usedType =
            ExprIRef.defI globalVar & Transaction.readIRef
            <&> (^. Def.defType)
            >>= processDef globalVar usedType
        processDef globalVar usedType newUsedDefType
            | alphaEq usedType newUsedDefType = pure Map.empty
            | otherwise =
                do
                    usedTypeS <- ConvertType.convertScheme (EntityId.usedTypeOf entityId) usedType
                    currentTypeS <- ConvertType.convertScheme (EntityId.currentTypeOf entityId) newUsedDefType
                    Sugar.DefinitionOutdatedType
                        { Sugar._defTypeWhenUsed = usedTypeS
                        , Sugar._defTypeCurrent = currentTypeS
                        , Sugar._defTypeUseCurrent =
                            updateDefType usedType newUsedDefType globalVar defExpr setDefExpr typeCheck
                        } & Map.singleton globalVar & pure
