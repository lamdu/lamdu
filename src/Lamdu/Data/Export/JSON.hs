-- | Import/Export JSON support
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Lamdu.Data.Export.JSON
    ( fileExportRepl, jsonExportRepl
    , fileExportAll, verifyAll
    , fileExportDef
    , fileImportAll
    ) where

import           AST (Ann(..), annotations, monoChildren, ToKnot(..))
import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (WriterT, runWriterT)
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Identifier (Identifier)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import qualified Lamdu.Data.Export.JSON.Migration as Migration
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Data.Tag (Tag(..), tagName, tagOrder)
import           Lamdu.Expr.IRef (ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.UniqueId (ToUUID(..))
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Visited = Visited
    { _visitedDefs :: Set V.Var
    , _visitedTags :: Set T.Tag
    , _visitedNominals :: Set T.NominalId
    }
Lens.makeLenses ''Visited

type Export m = WriterT [Codec.Entity] (StateT Visited (T m))

type EntityOrdering = (Int, Identifier)

entityOrdering :: Codec.Entity -> EntityOrdering
entityOrdering (Codec.EntitySchemaVersion _)                          = (0, "")
entityOrdering (Codec.EntityTag _ _ (T.Tag ident))                    = (1, ident)
entityOrdering (Codec.EntityNominal _ (T.NominalId nomId) _)          = (2, nomId)
entityOrdering (Codec.EntityLamVar _ _ _ (V.Var ident))               = (3, ident)
entityOrdering (Codec.EntityDef (Definition _ _ (_, _, V.Var ident))) = (4, ident)
entityOrdering (Codec.EntityRepl _)                                   = (5, "")

entityVersion :: Codec.Entity
entityVersion = Codec.EntitySchemaVersion Migration.currentVersion

runExport :: Monad m => Export m a -> T m (a, Aeson.Value)
runExport act =
    act
    & runWriterT
    <&> _2 %~ Aeson.toJSON . List.sortOn entityOrdering . (entityVersion :)
    & (`State.evalStateT` Visited mempty mempty mempty)

trans :: Monad m => T m a -> Export m a
trans = lift . lift

withVisited ::
    (Monad m, Ord a) =>
    Lens.ALens' Visited (Set a) -> a -> Export m () -> Export m ()
withVisited l x act =
    do
        alreadyVisited <- Lens.use (Lens.cloneLens l . Lens.contains x)
        unless alreadyVisited $
            do
                Lens.assign (Lens.cloneLens l . Lens.contains x) True
                act

readAssocTag :: Monad m => ToUUID a => a -> T m T.Tag
readAssocTag = Property.getP . Anchors.assocTag

tell :: Monad m => Codec.Entity -> Export m ()
tell = Writer.tell . (: [])

exportTag :: Monad m => T.Tag -> Export m ()
exportTag tag =
    do
        info <- ExprIRef.readTagInfo tag & trans
        let name = info ^. tagName
        let mName = name <$ (guard . not . Text.null) name
        Codec.EntityTag (info ^. tagOrder) mName tag & tell
    & withVisited visitedTags tag

exportNominal :: Monad m => T.NominalId -> Export m ()
exportNominal nomId =
    do
        nominal <- trans (Load.nominal nomId)
        tag <- readAssocTag nomId & trans
        Codec.EntityNominal tag nomId nominal & tell
        & withVisited visitedNominals nomId

exportSubexpr :: Monad m => Val (ValP m) -> Export m ()
exportSubexpr (Ann lamP (V.BLam (V.Lam lamVar _))) =
    do
        tag <- readAssocTag lamVar & trans
        mParamList <- Property.getP (Anchors.assocFieldParamList lamI) & trans
        Codec.EntityLamVar mParamList tag (toUUID lamI) lamVar & tell
    where
        lamI = lamP ^. Property.pVal
exportSubexpr _ = pure ()

exportVal :: Monad m => Val (ValP m) -> Export m ()
exportVal x =
    do
        x ^.. ExprLens.valGlobals mempty & traverse_ exportDef
        x ^.. ExprLens.valTags & traverse_ exportTag
        x ^.. ExprLens.valNominals & traverse_ exportNominal
        x ^.. ExprLens.subExprs & traverse_ exportSubexpr

exportDef :: Monad m => V.Var -> Export m ()
exportDef globalId =
    do
        presentationMode <- Property.getP (Anchors.assocPresentationMode globalId) & trans
        tag <- readAssocTag globalId & trans
        exportTag tag
        def <- Load.def defI & trans
        def ^. Definition.defBody & traverse_ exportVal
        let def' =
                def
                & Definition.defBody . Lens.mapped . annotations %~
                    toUUID . Property.value
        (presentationMode, tag, globalId) <$ def' & Codec.EntityDef & tell
    & withVisited visitedDefs globalId
    where
        defI = ExprIRef.defI globalId

exportRepl :: Export ViewM ()
exportRepl =
    do
        repl <- Load.defExpr (DbLayout.repl DbLayout.codeAnchors) & trans
        traverse_ exportVal repl
        repl <&> annotations %~ toUUID . Property.value & Codec.EntityRepl & tell

jsonExportRepl :: T ViewM Aeson.Value
jsonExportRepl = runExport exportRepl <&> snd

fileExportRepl :: FilePath -> T ViewM (IO ())
fileExportRepl = export "repl" exportRepl

fileExportDef :: Monad m => V.Var -> FilePath -> T m (IO ())
fileExportDef globalId =
    export ("def: " ++ show globalId) (exportDef globalId)

exportAll :: Export ViewM ()
exportAll =
    do
        exportSet DbLayout.globals (exportDef . ExprIRef.globalId)
        exportSet DbLayout.tags exportTag
        exportSet DbLayout.tids exportNominal
        exportRepl
    where
        exportSet indexIRef exportFunc =
            indexIRef DbLayout.codeIRefs & Transaction.readIRef & trans
            >>= traverse_ exportFunc

-- | Verify that the data in the database is valid
verifyAll :: T ViewM ()
verifyAll = runExport exportAll & void

fileExportAll :: FilePath -> T ViewM (IO ())
fileExportAll = export "all" exportAll

export :: Monad m => String -> Export m a -> FilePath -> T m (IO ())
export msg act exportPath =
    runExport act
    <&> snd
    <&> \json ->
        do
            putStrLn $ "Exporting " ++ msg ++ " to " ++ show exportPath
            LBS.writeFile exportPath (AesonPretty.encodePretty json)

writeValAt :: Monad m => Val (ValI m) -> T m (ValI m)
writeValAt (Ann valI body) =
    valI <$ (monoChildren writeValAt body >>= ExprIRef.writeValI valI)

writeValAtUUID :: Monad m => Val UUID -> T m (ValI m)
writeValAtUUID x = x & annotations %~ ToKnot . IRef.unsafeFromUUID & writeValAt

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

importRepl :: Definition.Expr (Val UUID) -> T ViewM ()
importRepl defExpr =
    traverse writeValAtUUID defExpr >>=
    Transaction.writeIRef (DbLayout.repl DbLayout.codeIRefs)

importTag :: Codec.TagOrder -> Maybe Text -> T.Tag -> T ViewM ()
importTag order mName tag =
    do
        Transaction.writeIRef (ExprIRef.tagI tag) (Tag (mName ^. Lens._Just) order)
        tag `insertTo` DbLayout.tags

importLamVar :: Monad m => Maybe Meta.ParamList -> T.Tag -> UUID -> V.Var -> T m ()
importLamVar paramList tag lamUUID var =
    do
        Property.setP (Anchors.assocFieldParamList lamI) paramList
        Property.setP (Anchors.assocTag var) tag
    where
        lamI = IRef.unsafeFromUUID lamUUID & ToKnot

importNominal :: T.Tag -> T.NominalId -> Maybe Nominal -> T ViewM ()
importNominal tag nomId nominal =
    do
        Property.setP (Anchors.assocTag nomId) tag
        traverse_ (Transaction.writeIRef (ExprIRef.nominalI nomId)) nominal
        nomId `insertTo` DbLayout.tids

importOne :: Codec.Entity -> T ViewM ()
importOne (Codec.EntityDef def) = importDef def
importOne (Codec.EntityRepl x) = importRepl x
importOne (Codec.EntityTag order mName tag) = importTag order mName tag
importOne (Codec.EntityNominal mName nomId nom) = importNominal mName nomId nom
importOne (Codec.EntityLamVar paramList tag lamUUID var) = importLamVar paramList tag lamUUID var
importOne (Codec.EntitySchemaVersion _) =
    fail "Only one schemaVersion allowed in beginning of document"

importEntities :: [Codec.Entity] -> T ViewM ()
importEntities (Codec.EntitySchemaVersion ver : entities) =
    if ver == 8
    then traverse_ importOne entities
    else "Unsupported schema version: " ++ show ver & fail
importEntities _ = "Missing schema version"  & fail

fileImportAll :: FilePath -> IO (T ViewM ())
fileImportAll importPath =
    do
        putStrLn $ "importing from: " ++ show importPath
        LBS.readFile importPath <&> Aeson.eitherDecode
            >>= either fail pure
            >>= Migration.migrateAsNeeded
            <&> Aeson.fromJSON
            <&> \case
                Aeson.Error str -> fail str
                Aeson.Success entities -> importEntities entities
