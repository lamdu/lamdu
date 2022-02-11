module Lamdu.Sugar.Convert.Suggest
    ( suggestTopLevelVal, suggestVal, genLamVar, suggestRec, suggestCase
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Extended as BS
import qualified Data.UUID as UUID
import           Hyper
import           Hyper.Syntax (FuncType(..), funcIn, funcOut)
import           Hyper.Syntax.Nominal (NominalInst, nId)
import           Hyper.Syntax.Row (RowExtend(..))
import           Hyper.Type.Prune (Prune(..))
import           Lamdu.Calc.Definition (Deps, depsNominals)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Expr.Load as Load
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

-- Suggest expression to fit a type.
-- Not used for subexpressions of suggested expression,
-- so may suggest multiple expressions.
suggestTopLevelVal :: Monad m => Pure # T.Type -> T m [(Deps, Pure # V.Term)]
suggestTopLevelVal t =
    (t ^.. _Pure . T._TFun . funcIn . _Pure . T._TInst & foldMap suggestFromNom) <>
    (t ^.. _Pure . T._TVariant & foldMap suggestVariantValues <&> Lens.mapped %~ (,) mempty) <>
    ( suggestVal t
        <&> (^? Lens.filtered (Lens.nullOf (_Pure . V._BLeaf . V._LHole)))
        <&> Lens._Just %~ (,) mempty
        <&> (^.. Lens._Just)
    )
    <&>
    (<> ((t ^.. _Pure . T._TFun . funcOut . _Pure . T._TVariant >>= suggestInjectOrGetFields V.LInject)
            <> (t ^.. _Pure . T._TFun . funcIn . _Pure . T._TRecord >>= suggestInjectOrGetFields V.LGetField)
            <&> (,) mempty
        )
    )

suggestFromNom :: Monad m => NominalInst T.NominalId T.Types # Pure -> Transaction m [(Deps, Pure # V.Term)]
suggestFromNom n =
    Load.nominal tid <&> (^.. Lens._Right) <&> Lens.mapped %~
    \s -> (mempty & depsNominals . Lens.at tid ?~ s, _Pure . V._BLeaf . V._LFromNom # tid)
    where
        tid = n ^. nId

suggestInjectOrGetFields :: (T.Tag -> V.Leaf) -> Pure # T.Row -> [Pure # V.Term]
suggestInjectOrGetFields o t =
    case t ^. _Pure of
    T.RExtend (RowExtend tag _ rest) -> Pure (V.BLeaf (o tag)) : suggestInjectOrGetFields o rest
    _ -> []

suggestVariantValues :: Monad m => Pure # T.Row -> T m [Pure # V.Term]
suggestVariantValues t =
    case t ^. _Pure of
    T.RExtend (RowExtend tag val rest) ->
        (:)
        <$> (suggestVal val <&> Pure . V.BApp . V.App (Pure (V.BLeaf (V.LInject tag))))
        <*> suggestVariantValues rest
    _ -> pure []

-- Suggest an expression to fit a type.
-- Used in suggested sub-expressions, so does not suggest to-noms.
suggestVal :: Monad m => Pure # T.Type -> T m (Pure # V.Term)
suggestVal t =
    case t ^. _Pure of
    T.TRecord r -> suggestRec r
    T.TFun f ->
        case f ^? funcIn . _Pure . T._TVariant of
        Just r -> suggestCase r (f ^. funcOut)
        Nothing ->
            genLamVar <&>
            \v -> _Pure . V._BLam # V.TypedLam v (_Pure . _HCompose # Pruned) (_Pure # V.BLeaf V.LHole)
    _ -> _Pure # V.BLeaf V.LHole & pure

suggestCase :: Monad m => Pure # T.Row -> Pure # T.Type -> T m (Pure # V.Term)
suggestCase r t =
    case r ^. _Pure of
    T.RVar{} -> _Pure # V.BLeaf V.LHole & pure
    T.REmpty -> _Pure # V.BLeaf V.LAbsurd & pure
    T.RExtend (RowExtend tag fieldType rest) ->
        RowExtend tag
        <$> suggestVal (_Pure . T._TFun # FuncType fieldType t)
        <*> suggestCase rest t
        <&> (_Pure . V._BCase #)

suggestRec :: Monad m => Pure # T.Row -> T m (Pure # V.Term)
suggestRec t =
    case t ^. _Pure of
    T.RVar{} -> _Pure # V.BLeaf V.LHole & pure
    T.REmpty -> _Pure # V.BLeaf V.LRecEmpty & pure
    T.RExtend (RowExtend tag fieldType rest) ->
        RowExtend tag
        <$> suggestVal fieldType
        <*> suggestRec rest
        <&> (_Pure . V._BRecExtend #)

genLamVar :: Monad m => T m V.Var
genLamVar = Transaction.newKey <&> V.Var . Identifier . BS.strictify . UUID.toByteString
