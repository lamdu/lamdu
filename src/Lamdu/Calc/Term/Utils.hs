{-# LANGUAGE TemplateHaskell, TypeApplications, ScopedTypeVariables, RankNTypes #-}

module Lamdu.Calc.Term.Utils
    ( Composite(..), tags, rest
    , case_
    , recExtend
    , culledSubexprPayloads
    ) where

import qualified Control.Lens as Lens
import           Hyper (Recursively(..), HFoldable(..), (#>), withDict)
import           Hyper.Syntax.Row (RowExtend(..))
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

-- | Return all subexprs until the given cut-point
culledSubexprPayloads ::
    forall t a r.
    Recursively HFoldable t =>
    (forall n. a # n -> Maybe r) -> Ann a # t -> [r]
culledSubexprPayloads f (Ann pl body) =
    case f pl of
    Nothing -> []
    Just x ->
        withDict (recursively (Proxy @(HFoldable t))) $
        x : hfoldMap (Proxy @(Recursively HFoldable) #> culledSubexprPayloads f) body

data Composite a = Composite
    { _tags :: Map T.Tag a
    , _rest :: Maybe a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Composite

case_ :: RowExtend T.Tag V.Term V.Term # Annotated pl -> Composite (Val pl)
case_ (RowExtend tag handler r) =
    caseVal r
    & tags . Lens.at tag ?~ handler
    where
        caseVal v@(Ann _ body) =
            case body of
            V.BLeaf V.LAbsurd -> Composite mempty Nothing
            V.BCase x -> case_ x
            _ -> Composite mempty (Just v)

recExtend :: RowExtend T.Tag V.Term V.Term # Annotated pl -> Composite (Val pl)
recExtend (RowExtend tag field r) =
    recExtendVal r
    & tags . Lens.at tag ?~ field
    where
        recExtendVal v@(Ann _ body) =
            case body of
            V.BLeaf V.LRecEmpty -> Composite mempty Nothing
            V.BRecExtend x -> recExtend x
            _ -> Composite mempty (Just v)
