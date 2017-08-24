{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable #-}
-- TODO: Move to more general place?
module Lamdu.Compiler.Flatten
    ( Composite(..), tags, rest
    , case_, Case
    , recExtend, Record
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val as V

import           Lamdu.Prelude

data Composite p a = Composite
    { _tags :: Map T.Tag a
    , _rest :: Maybe a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Composite

type Case = Composite T.SumTag
type Record = Composite T.ProductTag

case_ :: V.Case (Val pl) -> Case (Val pl)
case_ (V.Case tag handler r) =
    caseVal r
    & tags . Lens.at tag ?~ handler
    where
        caseVal val@(Val _ body) =
            case body of
            V.BLeaf V.LAbsurd -> Composite mempty Nothing
            V.BCase x -> case_ x
            _ -> Composite mempty (Just val)

recExtend :: V.RecExtend (Val pl) -> Record (Val pl)
recExtend (V.RecExtend tag field r) =
    recExtendVal r
    & tags . Lens.at tag ?~ field
    where
        recExtendVal val@(Val _ body) =
            case body of
            V.BLeaf V.LRecEmpty -> Composite mempty Nothing
            V.BRecExtend x -> recExtend x
            _ -> Composite mempty (Just val)
