{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Calc.Term.Utils
    ( Composite(..), tags, rest
    , case_, Case
    , recExtend, Record
    , culledSubexprPayloads
    ) where

import qualified Control.Lens as Lens
import           Data.Tree.Diverse (Ann(..))
import           Lamdu.Calc.Term (Val, termChildren)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

-- | Return all subexprs until the given cut-point
culledSubexprPayloads :: (a -> Bool) -> Val a -> [a]
culledSubexprPayloads cut =
    go
    where
        go (Ann pl body)
            | cut pl = []
            | otherwise = pl : body ^. termChildren . Lens.to go

data Composite p a = Composite
    { _tags :: Map T.Tag a
    , _rest :: Maybe a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Composite

type Case = Composite T.VariantTag
type Record = Composite T.RecordTag

case_ :: V.Case (Val pl) -> Case (Val pl)
case_ (V.Case tag handler r) =
    caseVal r
    & tags . Lens.at tag ?~ handler
    where
        caseVal v@(Ann _ body) =
            case body of
            V.BLeaf V.LAbsurd -> Composite mempty Nothing
            V.BCase x -> case_ x
            _ -> Composite mempty (Just v)

recExtend :: V.RecExtend (Val pl) -> Record (Val pl)
recExtend (V.RecExtend tag field r) =
    recExtendVal r
    & tags . Lens.at tag ?~ field
    where
        recExtendVal v@(Ann _ body) =
            case body of
            V.BLeaf V.LRecEmpty -> Composite mempty Nothing
            V.BRecExtend x -> recExtend x
            _ -> Composite mempty (Just v)
