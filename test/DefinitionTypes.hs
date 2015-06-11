{-# LANGUAGE OverloadedStrings #-}
module DefinitionTypes (definitionTypes) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Monoid (mappend)
import Data.String (IsString(..))
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.Type (Type, (~>))
import qualified Data.Map as Map
import qualified Data.Store.Map as MapStore
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.ExampleDB as ExampleDB
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

boolType :: Type
boolType = T.TInst "Bool" Map.empty

definitionTypes :: Map V.GlobalId Scheme
definitionTypes =
    exampleDBDefs `mappend` extras
    where
        extras = Map.singleton "IntToBoolFunc" $ Scheme.mono $ T.int ~> boolType
        exampleDBDefs =
            fst . MapStore.runEmpty . Transaction.run MapStore.mapStore $
                do
                    (_, defIs) <- ExampleDB.createBuiltins
                    Map.fromList <$> mapM readDef defIs

        nameOf = fmap fromString . Transaction.getP . Anchors.assocNameRef
        readDef defI =
            do
                Definition.BodyBuiltin (Definition.Builtin _ scheme) <- Transaction.readIRef defI
                name <- nameOf defI
                return (name, scheme)
