-- | Deduplicate exported entities of the same name

{-# LANGUAGE TemplateHaskell, LambdaCase, OverloadedStrings, FlexibleContexts #-}
module Main (main) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (_NominalType)
import qualified Lamdu.Calc.Type.Nominal as Nominal
import qualified Lamdu.Calc.Val as V
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Data.Export.JSON.Codec (TagOrder)
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import           Lamdu.Data.Export.JSON.Process (process)
import qualified Lamdu.Expr.Lens as ExprLens
import           System.IO (hPutStrLn, stderr)

data DedupState = DedupState
    { _tagRenames :: Map T.Tag T.Tag
    , _tagCanonical :: Map String (TagOrder, T.Tag)
    }
Lens.makeLenses ''DedupState

emptyState :: DedupState
emptyState = DedupState Map.empty Map.empty

type EntityOrdering = (Int, Identifier)

entityOrdering :: Codec.Entity -> EntityOrdering
entityOrdering (Codec.EntityTag _ _ (T.Tag ident))                  = (0, ident)
entityOrdering (Codec.EntityNominal _ (T.NominalId nomId) _)        = (1, nomId)
entityOrdering (Codec.EntityLamVar _ _ _ (V.Var ident))             = (2, ident)
entityOrdering (Codec.EntityDef (Definition _ (_, _, V.Var ident))) = (3, ident)
entityOrdering (Codec.EntityRepl _)                                 = (4, "")

dedup :: [Codec.Entity] -> IO [Codec.Entity]
dedup entities =
    traverse f entities
    & (`evalStateT` emptyState)
    <&> concat
    <&> List.sortOn entityOrdering
    where
        f :: Codec.Entity -> StateT DedupState IO [Codec.Entity]
        f entity =
            do
                renames <- Lens.use tagRenames
                let rename x = Map.lookup x renames & fromMaybe x
                g rename entity
        g :: (T.Tag -> T.Tag) -> Codec.Entity -> StateT DedupState IO [Codec.Entity]
        g _ entity@(Codec.EntityTag tagOrder (Just name) tag) =
            Lens.use (tagCanonical . Lens.at name)
            >>= \case
                Nothing ->
                    [entity] <$ (tagCanonical . Lens.at name ?= (tagOrder, tag))
                Just (oldTagOrder, oldTag) ->
                    do
                        when (oldTagOrder /= tagOrder) $ lift $ hPutStrLn stderr warn
                        [] <$ (tagRenames . Lens.at tag ?= oldTag)
                    where
                        warn =
                            unwords
                            [ "Conflicting tagOrder for", show name
                            , show oldTagOrder, "vs", show tagOrder
                            , "using", show oldTagOrder
                            ]
        g _ entity@(Codec.EntityTag _ Nothing _) = pure [entity]
        g rename (Codec.EntityRepl x) =
            pure [x & ExprLens.valTags %~ rename & Codec.EntityRepl]
        g rename (Codec.EntityDef x) =
            pure
            [x
             & Definition.defTags %~ rename
             & Definition.defBody . Lens.mapped . ExprLens.valTags %~ rename
             & Codec.EntityDef]
        g rename (Codec.EntityNominal mName nomId nom) =
            pure
            [nom
             & Nominal.nomType . _NominalType . ExprLens.schemeTags %~ rename
             & Codec.EntityNominal mName nomId]
        g rename (Codec.EntityLamVar paramList mName uuid var) =
            pure
            [ Codec.EntityLamVar
              (paramList
               & Lens._Just . Lens.mapped %~ rename)
              mName uuid var
            ]

main :: IO ()
main = process dedup
