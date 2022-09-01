-- | Exported JSON import support
{-# LANGUAGE FlexibleInstances #-}
module Lamdu.Data.Export.JSON.Import
    ( fileImportAll
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as AesonDiff
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import           Hyper
import           Hyper.Syntax.Nominal (NominalDecl)
import           Hyper.Syntax.Scheme (QVars)
import           Hyper.Type.Functor (_F)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Definition (Definition(..))
import           Lamdu.Data.Export.JSON.Codec (Version(..))
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import qualified Lamdu.Data.Export.JSON.Migration as Migration
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Data.Tag (Tag(..))
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Revision.Deltum.Hyper (writeRecursively, _ExistingRef)
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

writeValAtUUID :: Monad m => Val UUID -> T m (ValI m)
writeValAtUUID x =
    x
    & hflipped %~ hmap (const ((:*: Const ()) . (_ExistingRef . _F #) . IRef.unsafeFromUUID . getConst))
    & writeRecursively
    <&> (^. hAnn . _1)

insertTo ::
    (Monad m, Ord a, Binary a) =>
    a -> (DbLayout.Code (IRef ViewM) ViewM -> IRef m (Set a)) -> T m ()
insertTo item setIRef =
    Transaction.readIRef iref
    <&> Set.insert item
    >>= Transaction.writeIRef iref
    where
        iref = setIRef DbLayout.codeIRefs

importDef :: Definition (Val UUID) (Meta.PresentationMode, T.Tag, V.Var) -> T ViewM ()
importDef (Definition defBody defScheme (presentationMode, tag, globalId)) =
    do
        Property.setP (Anchors.assocPresentationMode globalId) presentationMode
        Property.setP (Anchors.assocTag globalId) tag
        bodyValI <- Lens.traverse writeValAtUUID defBody
        Definition bodyValI defScheme () & Transaction.writeIRef defI
        defI `insertTo` DbLayout.globals
    where
        defI = ExprIRef.defI globalId

importTag :: T.Tag -> Tag -> T ViewM ()
importTag tagId tagData =
    do
        Transaction.writeIRef (ExprIRef.tagI tagId) tagData
        tagId `insertTo` DbLayout.tags

importLamVar :: Monad m => T.Tag -> V.Var -> T m ()
importLamVar tag var =
    Property.setP (Anchors.assocTag var) tag

importNominal :: T.Tag -> T.NominalId -> Either (T.Types # QVars) (Pure # NominalDecl T.Type) -> T ViewM ()
importNominal tag nomId nominal =
    do
        Property.setP (Anchors.assocTag nomId) tag
        Transaction.writeIRef (ExprIRef.nominalI nomId) nominal
        nomId `insertTo` DbLayout.tids

importOne :: Codec.Entity -> T ViewM ()
importOne (Codec.EntityDef def) = importDef def
importOne (Codec.EntityTag tagId tagData) = importTag tagId tagData
importOne (Codec.EntityNominal mName nomId nom) = importNominal mName nomId nom
importOne (Codec.EntityLamVar tag var) = importLamVar tag var
importOne (Codec.EntityOpenDefPane var) = DataOps.newPane DbLayout.codeAnchors (Anchors.PaneDefinition (ExprIRef.defI var))
importOne (Codec.EntitySchemaVersion _) =
    error "Only one schemaVersion allowed in beginning of document"

importEntities :: [Codec.Entity] -> T ViewM ()
importEntities (Codec.EntitySchemaVersion ver : entities) =
    if ver == Migration.currentVersion
    then traverse_ importOne entities
    else "Unsupported schema version: " ++ show ver & error
importEntities _ = error "Missing schema version"

fileImportAll :: FilePath -> IO (Version, T ViewM ())
fileImportAll importPath =
    do
        (origVersion, migrated) <-
            LBS.readFile importPath <&> Aeson.eitherDecode
            >>= either fail pure
            >>= Migration.migrateAsNeeded
        case Aeson.fromJSON migrated of
            Aeson.Error str -> fail str
            Aeson.Success entities
                | reencoded == migrated -> pure (origVersion, importEntities entities)
                | otherwise ->
                    "JSON codec ignored fields:\n" <>
                    LBSChar.unpack
                    (AesonPretty.encodePretty (Aeson.toJSON (AesonDiff.diff reencoded migrated)))
                    & fail
                where
                    reencoded = Aeson.toJSON entities
