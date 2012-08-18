{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}

module Editor.Anchors
  ( panes, panesIRef
  , clipboards, clipboardsIRef
  , cursor, cursorIRef, preCursor, postCursor, preJumps
  , branches, branchesIRef
  , view, viewIRef
  , redos, redosIRef
  , currentBranchIRef, currentBranch
  , globals
  , BuiltinsMap, builtinsMap
  , newBuiltin, newBuiltinExpression, newDefinition
  , Pane
  , dbStore, DBTag
  , viewStore, ViewTag
  , assocDataRef
  , assocNameRef, assocSearchTermRef
  , makePane, makeDefinition, newPane
  , savePreJumpPosition, jumpBack
  , MkProperty, getP, setP, modP
  )
where

import Control.Monad (liftM, liftM2, when)
import Data.Binary (Binary(..))
import Data.ByteString.Char8 ()
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

type MkProperty t m a = Transaction t m (Transaction.Property t m a)

panes :: Monad m => MkProperty ViewTag m [Pane]
panes = Transaction.fromIRef panesIRef

clipboardsIRef :: IRef [Data.ExpressionIRef]
clipboardsIRef = IRef.anchor "clipboard"

clipboards :: Monad m => MkProperty ViewTag m [Data.ExpressionIRef]
clipboards = Transaction.fromIRef clipboardsIRef

branchesIRef :: IRef [(IRef String, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => MkProperty DBTag m [(IRef String, Branch)]
branches = Transaction.fromIRef branchesIRef

currentBranchIRef :: IRef Branch
currentBranchIRef = IRef.anchor "currentBranch"

currentBranch :: Monad m => MkProperty DBTag m Branch
currentBranch = Transaction.fromIRef currentBranchIRef

cursorIRef :: IRef Widget.Id
cursorIRef = IRef.anchor "cursor"

-- TODO: This should be an index
globals :: Monad m => MkProperty ViewTag m [Data.VariableRef]
globals = Transaction.fromIRef $ IRef.anchor "globals"

type BuiltinsMap = Map Data.FFIName Data.VariableRef
builtinsMap :: Monad m => MkProperty ViewTag m BuiltinsMap
builtinsMap = Transaction.fromIRef $ IRef.anchor "builtinsMap"

-- Cursor is untagged because it is both saved globally and per-revision.
-- Cursor movement without any revisioned changes are not saved per-revision.
cursor :: Monad m => MkProperty DBTag m Widget.Id
cursor = Transaction.fromIRef cursorIRef

preJumps :: Monad m => MkProperty ViewTag m [Widget.Id]
preJumps = Transaction.fromIRef $ IRef.anchor "prejumps"

preCursor :: Monad m => MkProperty ViewTag m Widget.Id
preCursor = Transaction.fromIRef $ IRef.anchor "precursor"

postCursor :: Monad m => MkProperty ViewTag m Widget.Id
postCursor = Transaction.fromIRef $ IRef.anchor "postcursor"

viewIRef :: IRef View
viewIRef = IRef.anchor "HEAD"

redosIRef :: IRef [Version]
redosIRef = IRef.anchor "redos"

redos :: Monad m => MkProperty DBTag m [Version]
redos = Transaction.fromIRef redosIRef

view :: Monad m => MkProperty DBTag m View
view = Transaction.fromIRef viewIRef

makePane :: Data.DefinitionIRef -> Pane
makePane = id

makeDefinition
  :: Monad m
  => Transaction ViewTag m Data.DefinitionIRef
makeDefinition = do
  let newHole = Data.newExprIRef Data.ExpressionHole
  defI <- Transaction.newIRef =<< liftM2 Data.Definition newHole newHole
  modP globals (Data.DefinitionRef defI :)
  return defI

dataGuid :: SBS.ByteString -> Guid -> Guid
dataGuid str guid = Guid.combine guid $ Guid.make str

assocDataRef
  :: (Binary b, Monad m)
  => SBS.ByteString -> Guid
  -> MkProperty t m (Maybe b)
assocDataRef str guid = do
  val <- Transaction.lookup assocGuid
  return $ Property val set
  where
    assocGuid = dataGuid str guid
    set Nothing = Transaction.delete assocGuid
    set (Just x) = Transaction.writeGuid assocGuid x

assocDataRefDef
  :: (Eq def, Binary def, Monad m)
  => def -> SBS.ByteString
  -> Guid -> MkProperty t m def
assocDataRefDef def name =
  liftM (Property.pureCompose (fromMaybe def) f) . assocDataRef name
  where
    f x
      | x == def = Nothing
      | otherwise = Just x

assocNameRef :: Monad m => Guid -> MkProperty t m String
assocNameRef = assocDataRefDef "" "Name"

assocSearchTermRef :: Monad m => Guid -> MkProperty t m String
assocSearchTermRef = assocDataRefDef "" "searchTerm"

newPane
  :: Monad m => Data.DefinitionIRef -> Transaction ViewTag m ()
newPane defI = do
  panesP <- panes
  when (defI `notElem` Property.value panesP) $
    Property.set panesP $ makePane defI : Property.value panesP

savePreJumpPosition :: Monad m => Widget.Id -> Transaction ViewTag m ()
savePreJumpPosition pos = modP preJumps $ (pos :) . take 19

jumpBack :: Monad m => Transaction ViewTag m (Maybe (Transaction ViewTag m Widget.Id))
jumpBack = do
  preJumpsP <- preJumps
  return $
    case Property.value preJumpsP of
    [] -> Nothing
    (j:js) -> Just $ do
      Property.set preJumpsP js
      return j

newBuiltinExpression
  :: Monad m
  => String -> Data.ExpressionIRef
  -> Transaction t m Data.ExpressionIRef
newBuiltinExpression fullyQualifiedName typeI =
  Data.newExprIRef . Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName (init path) (last path)) typeI
  where
    path = splitOn "." fullyQualifiedName

newBuiltin
  :: Monad m
  => String -> Data.ExpressionIRef
  -> Transaction t m Data.VariableRef
newBuiltin fullyQualifiedName typeI =
  newDefinition name . (`Data.Definition` typeI) =<<
    newBuiltinExpression fullyQualifiedName typeI
  where
    name = last $ splitOn "." fullyQualifiedName

newDefinition :: Monad m => String -> Data.DefinitionI -> Transaction t m Data.VariableRef
newDefinition name defI = do
  res <- Transaction.newIRef defI
  setP (assocNameRef (IRef.guid res)) name
  return $ Data.DefinitionRef res


getP :: Monad m => MkProperty t m a -> Transaction t m a
getP = liftM Property.value

setP :: Monad m => MkProperty t m a -> a -> Transaction t m ()
setP mkProp val = do
  prop <- mkProp
  Property.set prop val

modP :: Monad m => MkProperty t m a -> (a -> a) -> Transaction t m ()
modP mkProp f = do
  prop <- mkProp
  Property.pureModify prop f
