-- | Import/Export JSON support
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, FlexibleContexts, LambdaCase #-}
module Lamdu.Data.Export.JSON
    ( fileExportRepl, jsonExportRepl
    , fileExportAll, verifyAll
    , fileExportDef
    , fileImportAll
    ) where

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
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Identifier (Identifier)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Nominal (Nominal)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import qualified Lamdu.Data.Export.JSON.Migration as Migration
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.UniqueId (ToUUID)
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import qualified Revision.Deltum.Property as Property
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

type Export = WriterT [Codec.Entity] (StateT Visited (T ViewM))

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

runExport :: Export a -> T ViewM (a, Aeson.Value)
runExport act =
    act
    & runWriterT
    <&> _2 %~ Aeson.toJSON . List.sortOn entityOrdering . (entityVersion :)
    & (`State.evalStateT` Visited mempty mempty mempty)

trans :: T ViewM a -> Export a
trans = lift . lift

withVisited :: Ord a => Lens.ALens' Visited (Set a) -> a -> Export () -> Export ()
withVisited l x act =
    do
        alreadyVisited <- Lens.use (Lens.cloneLens l . Lens.contains x)
        unless alreadyVisited $
            do
                Lens.assign (Lens.cloneLens l . Lens.contains x) True
                act

readAssocTag :: Monad m => ToUUID a => a -> T m T.Tag
readAssocTag x = Anchors.assocTag x & Transaction.getP

tell :: Codec.Entity -> Export ()
tell = Writer.tell . (: [])

exportTag :: T.Tag -> Export ()
exportTag tag =
    do
        tagOrder <- Transaction.getP (Anchors.assocTagOrder tag) & trans
        name <- Transaction.getP (Anchors.assocTagNameRef tag) & trans
        let mName = name <$ (guard . not . Text.null) name
        Codec.EntityTag tagOrder mName tag & tell
    & withVisited visitedTags tag

exportNominal :: T.NominalId -> Export ()
exportNominal nomId =
    do
        nominal <- trans (Load.nominal nomId)
        mName <- readAssocTag nomId & trans
        Codec.EntityNominal mName nomId nominal & tell
        & withVisited visitedNominals nomId

exportSubexpr :: Val (ValI ViewM) -> Export ()
exportSubexpr (Val lamI (V.BLam (V.Lam lamVar _))) =
    do
        tag <- readAssocTag lamVar & trans
        mParamList <- Transaction.getP (Anchors.assocFieldParamList lamI) & trans
        Codec.EntityLamVar mParamList tag (valIToUUID lamI) lamVar & tell
exportSubexpr _ = pure ()

exportVal :: Val (ValI ViewM) -> Export ()
exportVal val =
    do
        val ^.. ExprLens.valGlobals mempty & traverse_ exportDef
        val ^.. ExprLens.valTags & traverse_ exportTag
        val ^.. ExprLens.valNominals & traverse_ exportNominal
        val ^.. ExprLens.subExprs & traverse_ exportSubexpr

valIToUUID :: ValI m -> UUID
valIToUUID = IRef.uuid . ExprIRef.unValI

exportDef :: V.Var -> Export ()
exportDef globalId =
    do
        presentationMode <- Transaction.getP (Anchors.assocPresentationMode globalId) & trans
        tag <- readAssocTag globalId & trans
        def <-
            Load.def defI & trans
            <&> Definition.defBody . Lens.mapped . Lens.mapped %~ Property.value
        traverse_ exportVal (def ^. Definition.defBody)
        let def' = def & Definition.defBody . Lens.mapped . Lens.mapped %~ valIToUUID
        (presentationMode, tag, globalId) <$ def' & Codec.EntityDef & tell
    & withVisited visitedDefs globalId
    where
        defI = ExprIRef.defI globalId

exportRepl :: Export ()
exportRepl =
    do
        repl <-
            DbLayout.repl DbLayout.codeIRefs & Transaction.readIRef
            >>= traverse ExprIRef.readVal
            & trans
        traverse_ exportVal repl
        repl <&> Lens.mapped %~ valIToUUID & Codec.EntityRepl & tell

jsonExportRepl :: T ViewM Aeson.Value
jsonExportRepl = runExport exportRepl <&> snd

fileExportRepl :: FilePath -> T ViewM (IO ())
fileExportRepl = export "repl" exportRepl

fileExportDef :: V.Var -> FilePath -> T ViewM (IO ())
fileExportDef globalId =
    export ("def: " ++ show globalId) (exportDef globalId)

exportAll :: Export ()
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

export :: String -> Export a -> FilePath -> T ViewM (IO ())
export msg act exportPath =
    runExport act
    <&> snd
    <&> \json ->
        do
            putStrLn $ "Exporting " ++ msg ++ " to " ++ show exportPath
            LBS.writeFile exportPath (AesonPretty.encodePretty json)

writeValAt :: Monad m => Val (ValI m) -> T m (ValI m)
writeValAt (Val valI body) =
    valI <$ (traverse writeValAt body >>= ExprIRef.writeValBody valI)

writeValAtUUID :: Monad m => Val UUID -> T m (ValI m)
writeValAtUUID val = val <&> IRef.unsafeFromUUID <&> ExprIRef.ValI & writeValAt

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
        Transaction.setP (Anchors.assocPresentationMode globalId) presentationMode
        Transaction.setP (Anchors.assocTag globalId) tag
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
importTag tagOrder mName tag =
    do
        Transaction.setP (Anchors.assocTagOrder tag) tagOrder
        traverse_ (Transaction.setP (Anchors.assocTagNameRef tag)) mName
        tag `insertTo` DbLayout.tags

importLamVar :: Maybe Meta.ParamList -> T.Tag -> UUID -> V.Var -> T ViewM ()
importLamVar paramList tag lamUUID var =
    do
        Transaction.setP (Anchors.assocFieldParamList lamI) paramList
        Transaction.setP (Anchors.assocTag var) tag
    where
        lamI = IRef.unsafeFromUUID lamUUID & ExprIRef.ValI

importNominal :: T.Tag -> T.NominalId -> Nominal -> T ViewM ()
importNominal tag nomId nominal =
    do
        Transaction.setP (Anchors.assocTag nomId) tag
        Transaction.writeIRef (ExprIRef.nominalI nomId) nominal
        nomId `insertTo` DbLayout.tids

importOne :: Codec.Entity -> T ViewM ()
importOne (Codec.EntityDef def) = importDef def
importOne (Codec.EntityRepl val) = importRepl val
importOne (Codec.EntityTag tagOrder mName tag) = importTag tagOrder mName tag
importOne (Codec.EntityNominal mName nomId nom) = importNominal mName nomId nom
importOne (Codec.EntityLamVar paramList tag lamUUID var) = importLamVar paramList tag lamUUID var
importOne (Codec.EntitySchemaVersion _) =
    fail "Only one schemaVersion allowed in beginning of document"

importEntities :: [Codec.Entity] -> T ViewM ()
importEntities (Codec.EntitySchemaVersion ver : entities) =
    if ver == 4
    then mapM_ importOne entities
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
