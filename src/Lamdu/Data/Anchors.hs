{-# LANGUAGE Rank2Types, OverloadedStrings, TemplateHaskell #-}
module Lamdu.Data.Anchors
  ( Code(..), onCode
  , Revision(..), onRevision
  , Pane, makePane
  , CodeProps, RevisionProps
  , assocNameRef
  , SpecialFunctions(..)
  ) where

import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.ByteString.Char8 ()
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version(Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (MkProperty(..))
import Lamdu.Data.Expression (Field)
import Lamdu.Data.Expression.IRef (DefI)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.FFI as FFI
import qualified Lamdu.Data.Expression.IRef as DataIRef

data SpecialFunctions t = SpecialFunctions
  { sfCons :: DataIRef.DefI t
  , sfNil :: DataIRef.DefI t
  }
derive makeBinary ''SpecialFunctions

type Pane t = DefI t

data Code f t = Code
  { panes :: f [Pane t]
  , clipboards :: f [DefI t]
  , globals :: f [DefI t]
  , specialFunctions :: f (SpecialFunctions t)
  , ffiEnv :: f (FFI.Env t)
  , preJumps :: f [Widget.Id]
  , preCursor :: f Widget.Id
  , postCursor :: f Widget.Id
  , fields :: f [Field]
  }
onCode :: Binary t => (forall a. Binary a => f a -> g a) -> Code f t -> Code g t
onCode f (Code x0 x1 x2 x3 x4 x5 x6 x7 x8) =
  Code (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8)

data Revision f t = Revision
  { branches :: f [Branch t]
  , currentBranch :: f (Branch t)
  , cursor :: f Widget.Id
  , redos :: f [Version t]
  , view :: f (View t)
  }
onRevision :: (forall a. Binary a => f a -> g a) -> Revision f t -> Revision g t
onRevision f (Revision x0 x1 x2 x3 x4) =
  Revision (f x0) (f x1) (f x2) (f x3) (f x4)

type CodeProps m = Code (MkProperty m) (Tag m)
type RevisionProps m = Revision (MkProperty m) (Tag m)

makePane :: DefI t -> Pane t
makePane = id

assocNameRef :: MonadA m => Guid -> MkProperty m String
assocNameRef = Transaction.assocDataRefDef "" "Name"
