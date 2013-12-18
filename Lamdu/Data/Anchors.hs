{-# LANGUAGE Rank2Types, OverloadedStrings, TemplateHaskell #-}
module Lamdu.Data.Anchors
  ( Code(..), onCode
  , Revision(..), onRevision
  , Pane, makePane
  , CodeProps, RevisionProps
  , assocNameRef
  , PresentationMode(..)
  , assocPresentationMode
  , SpecialFunctions(..)
  ) where

import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.ByteString.Char8 ()
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version(Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (MkProperty(..))
import Lamdu.Data.Expr.IRef (DefI)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Data.FFI as FFI

data SpecialFunctions t = SpecialFunctions
  { sfNil :: DefI t
  , sfCons :: DefI t
  , sfHeadTag :: Guid
  , sfTailTag :: Guid
  }

type Pane t = DefI t

data Code f t = Code
  { panes :: f [Pane t]
  , clipboards :: f [DefI t]
  , globals :: f [DefI t]
  , specialFunctions :: f (SpecialFunctions t)
  , ffiEnv :: f (FFI.Env t)
  , preJumps :: f [WidgetId.Id]
  , preCursor :: f WidgetId.Id
  , postCursor :: f WidgetId.Id
  , tags :: f [Guid]
  }
onCode :: Binary t => (forall a. Binary a => f a -> g a) -> Code f t -> Code g t
onCode f (Code x0 x1 x2 x3 x4 x5 x6 x7 x8) =
  Code (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8)

data Revision f t = Revision
  { branches :: f [Branch t]
  , currentBranch :: f (Branch t)
  , cursor :: f WidgetId.Id
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

data PresentationMode = OO | Verbose | Infix
  deriving (Eq, Ord, Enum, Bounded, Show)

assocPresentationMode ::
  MonadA m => Guid -> Transaction.MkProperty m PresentationMode
assocPresentationMode = Transaction.assocDataRefDef OO "PresentationMode"

derive makeBinary ''SpecialFunctions
derive makeBinary ''PresentationMode
