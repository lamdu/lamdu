{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module DefinitionTypes (definitionTypes) where

import           Prelude.Compat

import qualified Data.Map as Map
import qualified Data.Store.Map as MapStore
import qualified Data.Store.Transaction as Transaction
import           Data.String (IsString(..))
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.ExampleDB as ExampleDB
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Type (Type, (~>))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Infer as Infer

boolType :: Type
boolType = T.TInst "Bool" Map.empty

definitionTypes :: Infer.Loaded
definitionTypes =
    Infer.Loaded (exampleDBDefs `mappend` extras) Map.empty
    where
        extras = Map.singleton "IntToBoolFunc" $ Scheme.mono $ T.TInt ~> boolType
        exampleDBDefs =
            fst . MapStore.runEmpty . Transaction.run MapStore.mapStore $
                do
                    publics <- ExampleDB.createPublics
                    Map.fromList <$> mapM readDef (ExampleDB.publicDefs publics)

        nameOf = fmap fromString . Transaction.getP . Anchors.assocNameRef
        readDef defI =
            do
                Definition.BodyBuiltin (Definition.Builtin _ scheme) <- Transaction.readIRef defI
                name <- nameOf defI
                return (name, scheme)
