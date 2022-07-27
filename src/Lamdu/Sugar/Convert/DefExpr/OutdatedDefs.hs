{-# LANGUAGE TypeFamilies #-}
module Lamdu.Sugar.Convert.DefExpr.OutdatedDefs
    ( scan
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens.Extended as Lens
import           Control.Monad (foldM)
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import           Hyper
import           Hyper.Syntax (funcIn, funcOut)
import           Hyper.Syntax.Row (RowExtend(..), freExtends, freRest)
import           Hyper.Syntax.Scheme (sTyp)
import           Lamdu.Calc.Definition (depsGlobalTypes)
import           Lamdu.Calc.Infer (alphaEq)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, HRef)
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

argToHoleFunc :: Lens.Traversal' (Ann a # V.Term) (Ann a # V.Term)
argToHoleFunc =
    ExprLens.valApply .
    Lens.filteredBy (V.appFunc . ExprLens.valHole) .
    V.appArg

recursivelyFixExpr ::
    Monad m =>
    (IsHoleArg -> Ann a # V.Term -> Maybe ((IsHoleArg -> Ann a # V.Term -> m ()) -> m ())) ->
    Ann a # V.Term -> m ()
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
            Nothing ->
                htraverse_
                ( \case
                    HWitness V.W_Term_Term -> go NotHoleArg
                    _ -> const (pure ())
                ) (x ^. hVal)

changeFuncRes :: Monad m => V.Var -> Ann (HRef m) # V.Term -> T m ()
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
applyHoleTo :: Monad m => Ann (HRef m) # V.Term -> T m ()
applyHoleTo x
    | Lens.has argToHoleFunc x
    || Lens.has ExprLens.valHole x = pure ()
    | otherwise = x ^. hAnn & DataOps.applyHoleTo & void

data RecordChange = RecordChange
    { fieldsAdded :: Set T.Tag
    , fieldsRemoved :: Set T.Tag
    , fieldsChanged :: Map T.Tag ArgChange
    }

data ArgChange = ArgChange | ArgRecordChange RecordChange

fixArg ::
    Monad m =>
    ArgChange -> Ann (HRef m) # V.Term ->
    (IsHoleArg -> Ann (HRef m) # V.Term -> T m ()) ->
    T m ()
fixArg ArgChange arg go =
    do
        applyHoleTo arg
        go IsHoleArg arg
fixArg (ArgRecordChange recChange) arg go =
    ( if Set.null (fieldsRemoved recChange)
        then
            arg ^. hAnn . ExprIRef.iref
            <$ changeFields (fieldsChanged recChange) arg go
        else
            do
                go IsHoleArg arg
                V.App
                    <$> DataOps.newHole
                    ?? arg ^. hAnn . ExprIRef.iref
                    <&> V.BApp
                    >>= ExprIRef.newValI
    )
    >>= addFields (fieldsAdded recChange)
    >>= arg ^. hAnn . ExprIRef.setIref

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
    Map T.Tag ArgChange -> Ann (HRef m) # V.Term ->
    (IsHoleArg -> Ann (HRef m) # V.Term -> T m ()) ->
    T m ()
changeFields changes arg go
    | Lens.hasn't traverse changes = go NotHoleArg arg
    | otherwise =
        case arg ^. hVal of
        V.BRecExtend (RowExtend tag fieldVal rest) ->
            case changes ^. Lens.at tag of
            Nothing ->
                do
                    go NotHoleArg fieldVal
                    changeFields changes rest go
            Just fieldChange ->
                do
                    fixArg fieldChange fieldVal go
                    changeFields (changes & Lens.at tag .~ Nothing) rest go
        _ -> fixArg ArgChange arg go

changeFuncArg :: Monad m => ArgChange -> V.Var -> Ann (HRef m) # V.Term -> T m ()
changeFuncArg change usedDefVar =
    recursivelyFixExpr mFix
    where
        mFix NotHoleArg (Ann pl (V.BLeaf (V.LVar v)))
            | v == usedDefVar = DataOps.applyHoleTo pl & void & const & Just
        mFix _ (Ann _ (V.BApp (V.App (Ann _ (V.BLeaf (V.LVar v))) arg)))
            | v == usedDefVar = fixArg change arg & Just
        mFix _ _ = Nothing

isPartSame ::
    Lens.Getting (Monoid.First (Pure # T.Type)) (Pure # T.Type) (Pure # T.Type) ->
    Pure # T.Scheme -> Pure # T.Scheme -> Bool
isPartSame part preType newType =
    do
        prePart <- (_Pure . sTyp) (^? part) preType
        newPart <- (_Pure . sTyp) (^? part) newType
        alphaEq prePart newPart & guard
    & Lens.has Lens._Just

argChangeType :: Pure # T.Scheme -> Pure # T.Scheme -> ArgChange
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
    Pure # T.Scheme -> Pure # T.Scheme ->
    V.Var -> Ann (HRef m) # V.Term -> T m ()
fixDefExpr prevType newType usedDefVar defExpr =
     -- Function result changed (arg is the same).
    changeFuncRes usedDefVar defExpr <$
    guard (isPartSame (_Pure . T._TFun . funcIn) prevType newType)
    <|>
    do
        isPartSame (_Pure . T._TFun . funcOut) prevType newType & guard
        -- Function arg changed (result is the same).
        prevArg <- (_Pure . sTyp) (^? _Pure . T._TFun . funcIn) prevType
        newArg <- (_Pure . sTyp) (^? _Pure . T._TFun . funcIn) newType
        changeFuncArg (argChangeType prevArg newArg) usedDefVar defExpr & pure
    & fromMaybe (SubExprs.onGetVars (DataOps.applyHoleTo <&> void) usedDefVar defExpr)

updateDefType ::
    Monad m =>
    Pure # T.Scheme -> Pure # T.Scheme -> V.Var ->
    Def.Expr (Ann (HRef m) # V.Term) -> (Def.Expr (ValI m) -> T m ()) ->
    T m PostProcess.Result ->
    T m ()
updateDefType prevType newType usedDefVar defExpr setDefExpr typeCheck =
    do
        defExpr
            <&> (^. hAnn . ExprIRef.iref)
            & Def.exprFrozenDeps . depsGlobalTypes . Lens.at usedDefVar ?~ newType
            & setDefExpr
        x <- typeCheck
        case x of
            PostProcess.GoodExpr -> pure ()
            PostProcess.BadExpr{} -> fixDefExpr prevType newType usedDefVar (defExpr ^. Def.expr)

scan ::
    MonadTransaction n m =>
    EntityId -> Def.Expr (Ann (HRef n) # V.Term) ->
    (Def.Expr (ValI n) -> T n ()) ->
    T n PostProcess.Result ->
    m (Map V.Var (Sugar.DefinitionOutdatedType InternalName (OnceT (T n)) (T n) ()))
scan entityId defExpr setDefExpr typeCheck =
    defExpr ^@.. Def.exprFrozenDeps . depsGlobalTypes . Lens.itraversed
    & traverse (uncurry scanDef) <&> mconcat
    where
        scanDef globalVar usedType =
            ExprIRef.defI globalVar & Transaction.readIRef & transaction
            <&> (^. Def.defType)
            >>= processDef globalVar usedType
        processDef globalVar usedType newUsedDefType
            | alphaEq usedType newUsedDefType = pure mempty
            | otherwise =
                do
                    usedTypeS <- ConvertType.convertScheme (EntityId.usedTypeOf entityId) usedType
                    currentTypeS <- ConvertType.convertScheme (EntityId.currentTypeOf entityId) newUsedDefType
                    globalVar Lens.~~>
                        Sugar.DefinitionOutdatedType
                        { Sugar._defTypeWhenUsed = usedTypeS & undefined -- TODO
                        , Sugar._defTypeCurrent = currentTypeS & undefined -- TODO
                        , Sugar._defTypeUseCurrent =
                            updateDefType usedType newUsedDefType globalVar
                            defExpr setDefExpr typeCheck
                        } & pure
