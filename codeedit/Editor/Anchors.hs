{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}

module Editor.Anchors
  ( panes, panesIRef
  , clipboards, clipboardsIRef
  , cursor, cursorIRef, preCursor, postCursor, preJumps
  , branches, branchesIRef
  , view, viewIRef
  , redos, redosIRef
  , currentBranchIRef, currentBranch
  , globals, builtinsMap
  , newBuiltin
  , Pane
  , dbStore, DBTag
  , viewStore, ViewTag
  , aDataRef, aNameRef, variableNameRef
  , makePane, makeDefinition, newPane
  , savePreJumpPosition, canJumpBack, jumpBack
  )
where

import Control.Monad (when, liftM)
import Data.Binary (Binary(..))
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Store.Db (Db)
import Data.Store.Guid(Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property(Property(Property))
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version(Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction, Store(..))
import qualified Data.ByteString as SBS
import qualified Data.Store.Db as Db
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data as Data
import qualified Graphics.UI.Bottle.Widget as Widget

type Pane = Data.DefinitionIRef

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

panesIRef :: IRef [Pane]
panesIRef = IRef.anchor "panes"

panes :: Monad m => Transaction.Property ViewTag m [Pane]
panes = Transaction.fromIRef panesIRef

clipboardsIRef :: IRef [Data.ExpressionIRef]
clipboardsIRef = IRef.anchor "clipboard"

clipboards :: Monad m => Transaction.Property ViewTag m [Data.ExpressionIRef]
clipboards = Transaction.fromIRef clipboardsIRef

branchesIRef :: IRef [(IRef String, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => Transaction.Property DBTag m [(IRef String, Branch)]
branches = Transaction.fromIRef branchesIRef

currentBranchIRef :: IRef Branch
currentBranchIRef = IRef.anchor "currentBranch"

currentBranch :: Monad m => Transaction.Property DBTag m Branch
currentBranch = Transaction.fromIRef currentBranchIRef

cursorIRef :: IRef Widget.Id
cursorIRef = IRef.anchor "cursor"

-- TODO: This should be an index
globals :: Monad m => Transaction.Property ViewTag m [Data.VariableRef]
globals = Transaction.fromIRef $ IRef.anchor "globals"

builtinsMap :: Monad m => Transaction.Property ViewTag m (Map Data.FFIName Data.VariableRef)
builtinsMap = Transaction.fromIRef $ IRef.anchor "builtinsMap"

-- Cursor is untagged because it is both saved globally and per-revision.
-- Cursor movement without any revisioned changes are not saved per-revision.
cursor :: Monad m => Transaction.Property DBTag m Widget.Id
cursor = Transaction.fromIRef cursorIRef

preJumps :: Monad m => Transaction.Property ViewTag m [Widget.Id]
preJumps = Transaction.fromIRef $ IRef.anchor "prejumps"

preCursor :: Monad m => Transaction.Property ViewTag m Widget.Id
preCursor = Transaction.fromIRef $ IRef.anchor "precursor"

postCursor :: Monad m => Transaction.Property ViewTag m Widget.Id
postCursor = Transaction.fromIRef $ IRef.anchor "postcursor"

viewIRef :: IRef View
viewIRef = IRef.anchor "HEAD"

redosIRef :: IRef [Version]
redosIRef = IRef.anchor "redos"

redos :: Monad m => Transaction.Property DBTag m [Version]
redos = Transaction.fromIRef redosIRef

view :: Monad m => Transaction.Property DBTag m View
view = Transaction.fromIRef viewIRef

makePane :: Data.DefinitionIRef -> Pane
makePane = id

makeDefinition
  :: Monad m => String
  -> Transaction ViewTag m Data.DefinitionIRef
makeDefinition newName = do
  holeI <- Data.newExprIRef Data.ExpressionHole
  typeI <- Data.newExprIRef Data.ExpressionHole
  defI <-
    Transaction.newIRef $
    Data.Definition typeI holeI
  Property.pureModify globals (Data.DefinitionRef defI :)
  (Property.set . aNameRef . IRef.guid) defI newName
  return defI

aDataRef
  :: (Binary b, Monad m) => SBS.ByteString -> b -> Guid
  -> Property (Transaction t m) b
aDataRef str def = Property.pureCompose (fromMaybe def) Just . combineGuid
  where
    combineGuid guid =
      Property getter setter
      where
        getter = Transaction.lookup aGuid
        setter (Just val) = Transaction.writeGuid aGuid val
        setter Nothing = Transaction.delete aGuid
        aGuid = Guid.combine guid $ Guid.make str

-- Get an associated name from the given IRef
aNameRef :: Monad m => Guid -> Property (Transaction t m) String
aNameRef = aDataRef "Name" ""

variableNameRef
  :: Monad m => Data.VariableRef -> Property (Transaction t m) String
variableNameRef = aNameRef . Data.variableRefGuid

newPane
  :: Monad m => Data.DefinitionIRef -> Transaction ViewTag m ()
newPane defI = do
  ps <- Property.get panes
  when (defI `notElem` ps) $
    Property.set panes $ makePane defI : ps

savePreJumpPosition :: Monad m => Widget.Id -> Transaction ViewTag m ()
savePreJumpPosition pos = Property.pureModify preJumps $ (pos :) . take 19

canJumpBack :: Monad m => Transaction ViewTag m Bool
canJumpBack = liftM (not . null) $ Property.get preJumps

jumpBack :: Monad m => Transaction ViewTag m (Maybe Widget.Id)
jumpBack = do
  jumps <- Property.get preJumps
  case jumps of
    [] -> return Nothing
    (j:js) -> do
      Property.set preJumps js
      return $ Just j

newBuiltin
  :: Monad m
  => String -> Data.ExpressionIRef
  -> Transaction t m Data.VariableRef
newBuiltin fullyQualifiedName typeI = do
  builtinExprI <-
    Data.newExprIRef . Data.ExpressionBuiltin $
    Data.FFIName (init path) name
  builtinIRef <- Transaction.newIRef $ Data.Definition typeI builtinExprI

  Property.set (aNameRef (IRef.guid builtinIRef)) name
  return $ Data.DefinitionRef builtinIRef
  where
    name = last path
    path = splitOn "." fullyQualifiedName
