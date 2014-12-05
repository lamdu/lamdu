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
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version (Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (MkProperty(..))
import GHC.Generics (Generic)
import Lamdu.Expr.IRef (DefI)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId

data SpecialFunctions m = SpecialFunctions
  { sfNil :: DefI m
  , sfCons :: DefI m
  , sfHeadTag :: T.Tag
  , sfTailTag :: T.Tag
  , sfTrue :: DefI m
  , sfFalse :: DefI m
  } deriving (Generic)
instance Binary (SpecialFunctions m)

type Pane m = DefI m

data Code f m = Code
  { panes :: f [Pane m]
  , clipboards :: f [DefI m]
  , globals :: f [DefI m]
  , specialFunctions :: f (SpecialFunctions m)
  , preJumps :: f [WidgetId.Id]
  , preCursor :: f WidgetId.Id
  , postCursor :: f WidgetId.Id
  , tags :: f [T.Tag]
  }
onCode :: (forall a. Binary a => f a -> g a) -> Code f m -> Code g m
onCode f (Code x0 x1 x2 x3 x4 x5 x6 x7) =
  Code (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)

data Revision f m = Revision
  { branches :: f [Branch m]
  , currentBranch :: f (Branch m)
  , cursor :: f WidgetId.Id
  , redos :: f [Version m]
  , view :: f (View m)
  }
onRevision :: (forall a. Binary a => f a -> g a) -> Revision f m -> Revision g m
onRevision f (Revision x0 x1 x2 x3 x4) =
  Revision (f x0) (f x1) (f x2) (f x3) (f x4)

type CodeProps m = Code (MkProperty m) (m)
type RevisionProps m = Revision (MkProperty m) (m)

makePane :: DefI m -> Pane m
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
