{-# LANGUAGE EmptyDataDecls #-}

module Editor.Anchors(
    clipboard, root, rootIRef,
    focalPointIRefs, branches, view,
    viewBoxsAnchor, dbBoxsAnchor,
    initDB,
    dbStore, DBTag,
    viewStore, ViewTag)
where

import           Control.Monad                   (unless)
import           Data.Binary                     (Binary)
import           Data.Binary.Utils               (encodeS)
import           Data.Store.IRef                 (IRef, guid)
import qualified Data.Store.IRef                 as IRef
import qualified Data.Store.Transaction          as Transaction
import           Data.Store.Transaction          (Transaction, Store)
import           Data.Store.Rev.Branch           (Branch)
import           Data.Store.Rev.View             (View)
import qualified Data.Store.Rev.Branch           as Branch
import qualified Data.Store.Rev.Version          as Version
import qualified Data.Store.Rev.View             as View
import qualified Data.Store.Property             as Property
import qualified Data.Store.Db                   as Db
import           Data.Store.Db                   (Db)
import           Editor.Data                     (ITreeD, TreeD)
import qualified Editor.Data                     as Data
import qualified Graphics.UI.Bottle.Widgets.Box      as Box
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

clipboard :: Monad m => Transaction.Property ViewTag m [ITreeD]
clipboard = Transaction.anchorRefDef "clipboard" []

rootIRef :: ITreeD
rootIRef = IRef.anchor "root"

root :: Monad m => Transaction.Property ViewTag m TreeD
root = Transaction.fromIRef rootIRef

focalPointIRefs :: Monad m => Transaction.Property ViewTag m [ITreeD]
focalPointIRefs = Transaction.anchorRefDef "focalPoint" []

boxsAnchor :: Monad m => String -> String -> Transaction.Property anyTag m Box.Cursor
boxsAnchor name = Transaction.containerStr . Transaction.anchorContainerDef name $ 0

viewBoxsAnchor :: Monad m => String -> Transaction.Property ViewTag m Box.Cursor
viewBoxsAnchor = boxsAnchor "GUI.box(v)"

dbBoxsAnchor :: Monad m => String -> Transaction.Property DBTag m Box.Cursor
dbBoxsAnchor = boxsAnchor "GUI.box(d)"

branchesIRef :: IRef [(IRef TextEdit.Model, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => Transaction.Property DBTag m [(IRef TextEdit.Model, Branch)]
branches = Transaction.fromIRef branchesIRef

-- Initialize an IRef if it does not already exist.
initRef :: (Binary a, Monad m) => IRef a -> Transaction t m a -> Transaction t m a
initRef iref act = do
  exists <- Transaction.irefExists iref
  unless exists (Property.set p =<< act)
  Property.get p
  where
    p = Transaction.fromIRef iref

viewIRef :: IRef View
viewIRef = IRef.anchor "HEAD"

view :: Monad m => Transaction.Property DBTag m View
view = Transaction.fromIRef viewIRef

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef branchesIRef $ do
      masterNameIRef <- Transaction.newIRef $ TextEdit.makeModel "master"
      initialVersionIRef <- Version.makeInitialVersion [(guid rootIRef, encodeS (Data.makeNode "" []))]
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    _ <- initRef viewIRef $ View.new (snd . head $ bs)
    return ()
