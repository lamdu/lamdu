{-# LANGUAGE OverloadedStrings #-}
module DefinitionTypes (definitionTypes) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Data.Map (Map)
import Data.Monoid (mappend)
import Data.String (IsString(..))
import Lamdu.Expr ((~>))
import Lamdu.Expr.Scheme (Scheme)
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Map as MapStore
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.ExampleDB as ExampleDB
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.Scheme as Scheme

boolType :: E.Type
boolType = E.TInst "Bool" Map.empty

definitionTypes :: Map E.GlobalId Scheme
definitionTypes =
  exampleDBDefs `mappend` extras
  where
    extras = Map.singleton "IntToBoolFunc" $ Scheme.mono $ E.intType ~> boolType
    exampleDBDefs =
      fst . MapStore.runEmpty . Transaction.run MapStore.mapStore $
        do  (_, defIs) <- ExampleDB.createBuiltins
            Map.fromList <$> mapM readDef defIs

    nameOf = fmap fromString . Transaction.getP . Anchors.assocNameRef . IRef.guid
    readDef defI =
      do  defBody <- Transaction.readIRef defI
          case defBody ^. Definition.bodyType of
            Definition.NoExportedType -> fail "Builtins must have exported types"
            Definition.ExportedType scheme ->
              do  name <-nameOf defI
                  return (name, scheme)
