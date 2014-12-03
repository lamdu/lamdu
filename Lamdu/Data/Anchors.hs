{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveGeneric #-}
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
import Data.Binary (Binary)
import Data.ByteString.Char8 ()
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version(Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (MkProperty(..))
import GHC.Generics (Generic)
import Lamdu.Expr.IRef (DefI)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId

data SpecialFunctions t = SpecialFunctions
  { sfNil :: DefI t
  , sfCons :: DefI t
  , sfHeadTag :: T.Tag
  , sfTailTag :: T.Tag
  , sfTrue :: DefI t
  , sfFalse :: DefI t
  } deriving (Generic)
instance Binary (SpecialFunctions t)

type Pane t = DefI t

data Code f t = Code
  { panes :: f [Pane t]
  , clipboards :: f [DefI t]
  , globals :: f [DefI t]
  , specialFunctions :: f (SpecialFunctions t)
  , preJumps :: f [WidgetId.Id]
  , preCursor :: f WidgetId.Id
  , postCursor :: f WidgetId.Id
  , tags :: f [T.Tag]
  }
onCode :: (forall a. Binary a => f a -> g a) -> Code f t -> Code g t
onCode f (Code x0 x1 x2 x3 x4 x5 x6 x7) =
  Code (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)

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

assocNameRef :: (UniqueId.ToGuid a, MonadA m) => a -> MkProperty m String
assocNameRef = Transaction.assocDataRefDef "" "Name" . UniqueId.toGuid

data PresentationMode = OO | Verbose | Infix
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)
instance Binary PresentationMode

assocPresentationMode ::
  (UniqueId.ToGuid a, MonadA m) =>
  a -> Transaction.MkProperty m PresentationMode
assocPresentationMode =
    Transaction.assocDataRefDef OO "PresentationMode" . UniqueId.toGuid
