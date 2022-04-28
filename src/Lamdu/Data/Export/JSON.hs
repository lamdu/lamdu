-- | JSON Export support
{-# LANGUAGE TemplateHaskell, TypeApplications, FlexibleInstances #-}
module Lamdu.Data.Export.JSON
    ( fileExportRepl, jsonExportRepl
    , fileExportAll, fileExportDef, fileExportTag, fileExportNominal
    , Version(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (WriterT, runWriterT)
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Property as Property
import           Hyper
import           Hyper.Recurse (unwrapM, (##>>))
import           Hyper.Syntax.Nominal (nParams)
import           Hyper.Type.Prune (Prune)
import           Lamdu.Calc.Identifier (Identifier)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Data.Export.JSON.Codec (Version(..))
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import qualified Lamdu.Data.Export.JSON.Migration as Migration
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.UniqueId (ToUUID(..))
import           Revision.Deltum.Hyper (HRef)
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
entityOrdering (Codec.EntityTag (T.Tag ident)_ )                      = (1, ident)
entityOrdering (Codec.EntityNominal _ (T.NominalId nomId) _)          = (2, nomId)
entityOrdering (Codec.EntityLamVar _ (V.Var ident))                   = (3, ident)
entityOrdering (Codec.EntityDef (Definition _ _ (_, _, V.Var ident))) = (4, ident)
entityOrdering (Codec.EntityRepl _)                                   = (5, "")
entityOrdering (Codec.EntityOpenDefPane (V.Var ident))                = (6, ident)

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
exportTag tag
    | tag == Anchors.anonTag = pure ()
    | otherwise =
        ExprIRef.readTagData tag & trans
        >>= tell . Codec.EntityTag tag
        & withVisited visitedTags tag

exportNominal :: Monad m => T.NominalId -> Export m ()
exportNominal nomId =
    do
        nominal <- trans (Load.nominal nomId)
        either id (^. _Pure . nParams) nominal
            & htraverse_
                ( Proxy @ExprLens.HasQVar #>
                    traverse_ (exportTag . T.Tag) . (^.. ExprLens.qvarsQVarIds)
                )
        tag <- readAssocTag nomId & trans
        Codec.EntityNominal tag nomId nominal & tell
        & withVisited visitedNominals nomId

class ExportSubexpr k where
    exportSubexpr :: Monad m => Ann (HRef m) # k -> Export m ()
    exportSubexpr _ = pure ()

instance ExportSubexpr V.Term where
    exportSubexpr (Ann _ (V.BLam (V.TypedLam lamVar _ _))) =
        do
            tag <- readAssocTag lamVar & trans
            exportTag tag
            Codec.EntityLamVar tag lamVar & tell
    exportSubexpr _ = pure ()

instance ExportSubexpr (HCompose Prune T.Type)
    -- TODO: Export noms!

instance ExportSubexpr (HCompose Prune T.Row)
    -- TODO: Export used tags!

exportVal :: Monad m => Ann (HRef m) # V.Term -> Export m ()
exportVal x =
    do
        x ^.. ExprLens.valGlobals mempty & traverse_ exportDef
        x ^.. ExprLens.valTags & traverse_ exportTag
        x ^.. ExprLens.valNominals & traverse_ exportNominal
        unwrapM (Proxy @ExportSubexpr ##>> \n -> n ^. hVal <$ exportSubexpr n) x & void

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
                & Definition.defBody . Lens.mapped . hflipped %~
                    hmap (const (Const . toUUID . (^. ExprIRef.iref)))
        (presentationMode, tag, globalId) <$ def' & Codec.EntityDef & tell
    & withVisited visitedDefs globalId
    where
        defI = ExprIRef.defI globalId

exportRepl :: Export ViewM ()
exportRepl =
    do
        repl <- Load.defExpr (DbLayout.repl DbLayout.codeAnchors) & trans
        traverse_ exportVal repl
        repl
            <&> hflipped %~ hmap (const (Const . toUUID . (^. ExprIRef.iref)))
            & Codec.EntityRepl & tell

jsonExportRepl :: T ViewM Aeson.Value
jsonExportRepl = runExport exportRepl <&> snd

fileExportRepl :: FilePath -> T ViewM (IO ())
fileExportRepl = export "repl" exportRepl

fileExportDef :: Monad m => V.Var -> FilePath -> T m (IO ())
fileExportDef globalId =
    export ("def: " ++ show globalId)
    (exportDef globalId >> tell (Codec.EntityOpenDefPane globalId))

fileExportTag :: Monad m => T.Tag -> FilePath -> T m (IO ())
fileExportTag tag =
    export ("tag: " ++ show tag) (exportTag tag)

fileExportNominal :: Monad m => T.NominalId -> FilePath -> T m (IO ())
fileExportNominal nomId =
    export ("nominal: " ++ show nomId) (exportNominal nomId)

exportAll :: Export ViewM ()
exportAll =
    do
        exportSet DbLayout.globals (exportDef . ExprIRef.globalId)
        exportSet DbLayout.tags exportTag
        exportSet DbLayout.tids exportNominal
        exportSet DbLayout.panes exportOpenPane
        exportRepl
    where
        exportSet indexIRef exportFunc =
            indexIRef DbLayout.codeIRefs & Transaction.readIRef & trans
            >>= traverse_ exportFunc

exportOpenPane :: Monad m => Anchors.Pane ViewM -> Export m ()
exportOpenPane (Anchors.PaneDefinition defI) = ExprIRef.globalId defI & Codec.EntityOpenDefPane & tell
exportOpenPane _ = pure ()

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
