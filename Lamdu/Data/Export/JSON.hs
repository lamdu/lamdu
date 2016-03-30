-- | Import/Export JSON support
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, FlexibleContexts, LambdaCase #-}
module Lamdu.Data.Export.JSON
    ( exportRepl
    , importAll
    ) where

-- TODO: Assoc data?
-- TODO: Schema version? What granularity?

import qualified Control.Lens as Lens
import           Control.Lens.Operators hiding ((.=))
import           Control.Lens.Tuple
import           Control.Monad (unless)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.Writer (WriterT(..))
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Either.Combinators (swapEither)
import           Data.Foldable (traverse_)
import           Data.Set (Set)
import           Data.Store.IRef
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.DbLayout (ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.UniqueId (ToUUID)
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

type T = Transaction

replIRef :: IRef ViewM (ValI ViewM)
replIRef = DbLayout.repl DbLayout.codeIRefs


data Visited = Visited
    { _visitedDefs :: Set V.Var
    , _visitedTags :: Set T.Tag
    , _visitedNominals :: Set T.NominalId
    }
Lens.makeLenses ''Visited

type Export = WriterT [Codec.Encoded] (StateT Visited (T ViewM))

runExport :: Export a -> T ViewM (a, Codec.Encoded)
runExport act =
    act
    & runWriterT
    <&> _2 %~ Codec.encodeArray
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

readAssocName :: ToUUID a => a -> Export (Maybe String)
readAssocName x =
    do
        name <- Anchors.assocNameRef x & Transaction.getP & trans
        return $
            if null name
            then Nothing
            else Just name

tell :: Codec.Encoded -> Export ()
tell = Writer.tell . (: [])

exportTag :: T.Tag -> Export ()
exportTag tag =
    do
        mName <- readAssocName tag
        Codec.encodeNamedTag (mName, tag) & tell
    & withVisited visitedTags tag

exportNominal :: T.NominalId -> Export ()
exportNominal nomId =
    trans (Load.nominal nomId) >>=
    \case
    Nothing -> return ()
    Just nominal ->
        do
            mName <- readAssocName nomId
            Codec.encodeNamedNominal ((mName, nomId), nominal) & tell
        & withVisited visitedNominals nomId

recurse :: Val a -> Export ()
recurse val =
    do
        -- TODO: Add a recursion on all ExprLens.subExprs -- that have
        -- a Lam inside them, on the "Var" to export all the var names
        -- too?  OR: alternatively, remove the "name" crap and just
        -- add the set of all UUIDs we ever saw to the WriterT, and
        -- export all associated names/data of all!
        val ^.. ExprLens.valGlobals mempty & traverse_ exportDef
        val ^.. ExprLens.valTags & traverse_ exportTag
        val ^.. ExprLens.valNominals & traverse_ exportNominal

exportDef :: V.Var -> Export ()
exportDef globalId =
    do
        mName <- readAssocName globalId
        def <-
            ExprIRef.defI globalId & Load.def & trans
            <&> Definition.defBody . Lens.mapped . Lens.mapped %~ Property.value
        traverse_ recurse (def ^. Definition.defBody)
        (globalId, mName) <$ def & Codec.encodeDef & tell
    & withVisited visitedDefs globalId

exportRepl :: T ViewM Codec.Encoded
exportRepl =
    do
        repl <- Transaction.readIRef replIRef >>= ExprIRef.readVal & trans
        recurse repl
        Codec.encodeRepl repl & tell
    & runExport
    <&> snd

setName :: ToUUID a => a -> String -> T ViewM ()
setName x = Transaction.setP (Anchors.assocNameRef x)

importDef :: Definition (Val ()) (V.Var, Maybe String) -> T ViewM ()
importDef (Definition body (globalId, mName)) =
    do
        traverse_ (setName globalId) mName
        body' <- traverse ExprIRef.newVal body
        Transaction.writeIRef defI body'
    where
        defI = ExprIRef.defI globalId

importRepl :: Val () -> T ViewM ()
importRepl val = ExprIRef.newVal val >>= Transaction.writeIRef replIRef

importTag :: (Maybe String, T.Tag) -> T ViewM ()
importTag (mName, tag) = traverse_ (setName tag) mName

importNominal :: ((Maybe String, T.NominalId), Nominal) -> T ViewM ()
importNominal ((mName, nomId), nominal) =
    do
        traverse_ (setName nomId) mName
        Transaction.writeIRef (ExprIRef.nominalI nomId) nominal

-- Like asum/msum but collects the errors
firstSuccess :: [Either err a] -> Either [err] a
firstSuccess = swapEither . sequence . fmap swapEither

importOne :: Codec.Encoded -> T ViewM ()
importOne json =
    firstSuccess
    [ try Codec.decodeDef importDef
    , try Codec.decodeRepl importRepl
    , try Codec.decodeNamedTag importTag
    , try Codec.decodeNamedNominal importNominal
    ]
    & either parseFail return
    >>= id
    where
        parseFail errors =
            "Failed to parse: " ++ BSL8.unpack (AesonPretty.encodePretty json) ++
            "\n" ++ unlines errors
            & fail
        try decode imp = Codec.runDecoder decode json <&> imp

importAll :: Codec.Encoded -> T ViewM ()
importAll json =
    Codec.runDecoder Codec.decodeArray json
    & either fail return
    >>= traverse_ importOne
