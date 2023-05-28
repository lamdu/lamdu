{-# LANGUAGE TypeApplications #-}

module Lamdu.Sugar.Convert.NodeActions.ReplaceParent
    ( setChildReplaceParentActions
    ) where

import qualified Control.Lens.Extended as Lens
import           Control.Monad.Once (OnceT)
import           Hyper
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

fragmentAnnIndex ::
    (Applicative f, Lens.Indexable j p) =>
    p a (f a) -> Lens.Indexed (Term v name i o # Annotated j) a (f a)
fragmentAnnIndex = Lens.filteredByIndex (_BodyFragment . fExpr . annotation)

bodyIndex ::
    Lens.IndexedTraversal' (k # Ann a) (Ann a # k) (Ann a # k)
bodyIndex = Lens.filteredBy hVal

class FixReplaceParent expr where
    fixReplaceParent :: (a -> a -> a) -> Annotated a # expr -> Annotated a # expr

instance FixReplaceParent (Const a) where
    fixReplaceParent _ = id

instance FixReplaceParent (Composite v name i o) where
    fixReplaceParent _ = id

instance FixReplaceParent (PostfixFunc v name i o) where
    fixReplaceParent _ = id

-- * Replace-parent with fragment sets directly to fragment expression
-- * Replace-parent of fragment expr without "heal" available -
--   replaces parent of fragment rather than fragment itself (i.e: replaces grandparent).

-- TODO: These instances have a repeating pattern
instance FixReplaceParent (Binder v name i o) where
    fixReplaceParent setToExpr =
        (hVal . bBody . _BinderTerm . typeMismatchPayloads %~ join setToExpr) .
        ((bodyIndex . Lens.filteredByIndex (bBody . _BinderTerm) . fragmentAnnIndex) <. annotation %@~ setToExpr)

instance FixReplaceParent (Term v name i o) where
    fixReplaceParent setToExpr =
        (hVal . typeMismatchPayloads %~ join setToExpr) .
        ((bodyIndex . fragmentAnnIndex) <. annotation %@~ setToExpr)

instance FixReplaceParent (Else v name i o) where
    fixReplaceParent setToExpr =
        (hVal . _SimpleElse . typeMismatchPayloads %~ join setToExpr) .
        ((bodyIndex . Lens.filteredByIndex _SimpleElse . fragmentAnnIndex) <. annotation%@~ setToExpr)

-- TODO: This is an indexed lens of some sort?
typeMismatchPayloads ::
    (a -> Identity a) ->
    Term v name i o # Annotated a -> Identity (Term v name i o # Annotated a)
typeMismatchPayloads =
    _BodyFragment . Lens.filteredBy (fTypeMismatch . Lens._Just) . fExpr .
    annotation

setChildReplaceParentActions ::
    Monad m =>
    ConvertM m (
        ExprIRef.HRef m # V.Term ->
        Term v name (OnceT (T m)) (T m) # Annotated (ConvertPayload m) ->
        Term v name (OnceT (T m)) (T m) # Annotated (ConvertPayload m)
    )
setChildReplaceParentActions =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal stored bod ->
    let setToExpr srcPl =
            pActions . mReplaceParent ?~
            (protectedSetToVal
                stored
                (srcPl ^. pStored . ExprIRef.iref)
                <&> EntityId.ofValI)
        setForChildren = hmap (\_ -> annotation %~ join setToExpr)
    in
    case bod of
    BodyFragment f | Lens.has (fTypeMismatch . Lens._Just) f ->
        -- Replace-parent for child of expr in fragment attempts heal
        f & fExpr . hVal %~ setForChildren & BodyFragment
    _ -> setForChildren bod
    & hmap (Proxy @FixReplaceParent #> fixReplaceParent setToExpr)
