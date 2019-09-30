{-# LANGUAGE TypeFamilies #-}
module Lamdu.Sugar.Convert.DefExpr.OutdatedDefs
    ( scan
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens.Extended as Lens
import           Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper (Tree, Pure(..), _Pure, htraverse1)
import           Hyper.Type.AST.FuncType (funcIn, funcOut)
import           Hyper.Type.AST.Row (RowExtend(..), freExtends, freRest)
import           Hyper.Type.AST.Scheme (sTyp)
import           Hyper.Type.Ann (Ann(..), ann, val)
import           Lamdu.Calc.Definition (depsGlobalTypes)
import           Lamdu.Calc.Infer (alphaEq)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
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
    Lens.filteredBy (V.appFunc . ExprLens.valHole) .
    V.appArg

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
        recurse x =
            case x ^? argToHoleFunc of
            Just arg -> go IsHoleArg arg
            Nothing -> traverse_ (go NotHoleArg) (x ^.. val . htraverse1)

changeFuncRes :: Monad m => V.Var -> Val (ValP m) -> T m ()
changeFuncRes usedDefVar =
    recursivelyFixExpr mFix
    where
        mFix NotHoleArg (Ann pl (V.BLeaf (V.LVar v)))
            | v == usedDefVar = DataOps.applyHoleTo pl & void & const & Just
        mFix isHoleArg
            (Ann pl (V.BApp (V.App (Ann _ (V.BLeaf (V.LVar v))) arg)))
            | v == usedDefVar =
            Just $
            \go ->
            do
                when (isHoleArg == NotHoleArg) (void (DataOps.applyHoleTo pl))
                go NotHoleArg arg
        mFix _ _ = Nothing

-- | Only if hole not already applied to it
applyHoleTo :: Monad m => Val (ValP m) -> T m ()
applyHoleTo x
    | Lens.has argToHoleFunc x
    || Lens.has ExprLens.valHole x = pure ()
    | otherwise = x ^. ann & DataOps.applyHoleTo & void

data RecordChange = RecordChange
    { fieldsAdded :: Set T.Tag
    , fieldsRemoved :: Set T.Tag
    , fieldsChanged :: Map T.Tag ArgChange
    }

data ArgChange = ArgChange | ArgRecordChange RecordChange

fixArg ::
    Monad m =>
    ArgChange -> Val (ValP m) ->
    (IsHoleArg -> Val (ValP m) -> T m ()) ->
    T m ()
fixArg ArgChange arg go =
    do
        applyHoleTo arg
        go IsHoleArg arg
fixArg (ArgRecordChange recChange) arg go =
    ( if Set.null (fieldsRemoved recChange)
        then
            arg ^. ann . Property.pVal
            <$ changeFields (fieldsChanged recChange) arg go
        else
            do
                go IsHoleArg arg
                V.App
                    <$> DataOps.newHole
                    ?? arg ^. ann . Property.pVal
                    <&> V.BApp
                    >>= ExprIRef.newValI
    )
    >>= addFields (fieldsAdded recChange)
    >>= arg ^. ann . Property.pSet

addFields :: Monad m => Set T.Tag -> ValI m -> Transaction m (ValI m)
addFields fields src =
    foldM addField src fields
    where
        addField x tag =
            RowExtend tag <$> DataOps.newHole ?? x
            <&> V.BRecExtend
            >>= ExprIRef.newValI

changeFields ::
    Monad m =>
    Map T.Tag ArgChange -> Val (ValP m) ->
    (IsHoleArg -> Val (ValP m) -> T m ()) ->
    T m ()
changeFields changes arg go
    | Map.null changes = go NotHoleArg arg
    | otherwise =
        case arg ^. val of
        V.BRecExtend (RowExtend tag fieldVal rest) ->
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

changeFuncArg :: Monad m => ArgChange -> V.Var -> Val (ValP m) -> T m ()
changeFuncArg change usedDefVar =
    recursivelyFixExpr mFix
    where
        mFix NotHoleArg (Ann pl (V.BLeaf (V.LVar v)))
            | v == usedDefVar = DataOps.applyHoleTo pl & void & const & Just
        mFix _ (Ann _ (V.BApp (V.App (Ann _ (V.BLeaf (V.LVar v))) arg)))
            | v == usedDefVar = fixArg change arg & Just
        mFix _ _ = Nothing

isPartSame ::
    Lens.Getting (Monoid.First (Tree Pure T.Type)) (Tree Pure T.Type) (Tree Pure T.Type) ->
    Tree Pure T.Scheme -> Tree Pure T.Scheme -> Bool
isPartSame part preType newType =
    do
        prePart <- (_Pure . sTyp) (^? part) preType
        newPart <- (_Pure . sTyp) (^? part) newType
        alphaEq prePart newPart & guard
    & Lens.has Lens._Just

argChangeType :: Tree Pure T.Scheme -> Tree Pure T.Scheme -> ArgChange
argChangeType prevArg newArg =
    do
        prevProd <- prevArg ^? _Pure . sTyp . _Pure . T._TRecord . T.flatRow
        prevProd ^? freRest . _Pure . T._REmpty
        newProd <- newArg ^? _Pure . sTyp . _Pure . T._TRecord . T.flatRow
        newProd ^? freRest . _Pure . T._REmpty
        let prevTags = prevProd ^. freExtends & Map.keysSet
        let newTags = newProd ^. freExtends & Map.keysSet
        let changedTags =
                Map.intersectionWith fieldChange
                (prevProd ^. freExtends)
                (newProd ^. freExtends)
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
                prevFieldScheme = prevArg & _Pure . sTyp .~ prevField
                newFieldScheme = newArg & _Pure . sTyp .~ newField

fixDefExpr ::
    Monad m =>
    Tree Pure T.Scheme -> Tree Pure T.Scheme ->
    V.Var -> Val (ValP m) -> T m ()
fixDefExpr prevType newType usedDefVar defExpr =
    ( -- Function result changed (arg is the same).
        changeFuncRes usedDefVar defExpr <$
        guard (isPartSame (_Pure . T._TFun . funcIn) prevType newType)
    ) <|>
    do
        isPartSame (_Pure . T._TFun . funcOut) prevType newType & guard
        -- Function arg changed (result is the same).
        prevArg <- (_Pure . sTyp) (^? _Pure . T._TFun . funcIn) prevType
        newArg <- (_Pure . sTyp) (^? _Pure . T._TFun . funcIn) newType
        changeFuncArg (argChangeType prevArg newArg) usedDefVar defExpr & pure
    & fromMaybe (SubExprs.onGetVars (DataOps.applyHoleTo <&> void) usedDefVar defExpr)

updateDefType ::
    Monad m =>
    Tree Pure T.Scheme -> Tree Pure T.Scheme -> V.Var ->
    Def.Expr (Val (ValP m)) -> (Def.Expr (ValI m) -> T m ()) ->
    T m PostProcess.Result ->
    T m ()
updateDefType prevType newType usedDefVar defExpr setDefExpr typeCheck =
    do
        defExpr
            <&> (^. ann . Property.pVal)
            & Def.exprFrozenDeps . depsGlobalTypes . Lens.at usedDefVar ?~ newType
            & setDefExpr
        x <- typeCheck
        case x of
            PostProcess.GoodExpr -> pure ()
            PostProcess.BadExpr{} -> fixDefExpr prevType newType usedDefVar (defExpr ^. Def.expr)

scan ::
    Monad m =>
    EntityId -> Def.Expr (Val (ValP m)) ->
    (Def.Expr (ValI m) -> T m ()) ->
    T m PostProcess.Result ->
    T m (Map V.Var (Sugar.DefinitionOutdatedType InternalName (T m) ()))
scan entityId defExpr setDefExpr typeCheck =
    defExpr ^. Def.exprFrozenDeps . depsGlobalTypes
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
                            updateDefType usedType newUsedDefType globalVar
                            defExpr setDefExpr typeCheck
                        } & Map.singleton globalVar & pure
