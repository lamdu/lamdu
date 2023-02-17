-- | "Definition"-related texts
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.I18N.Definitions where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Element.Id (ElemIds)

import           Lamdu.Prelude

data Definitions a = Definitions
    { _newDefinitionButton :: a
    , _undelete :: a
    , _undeleteButton :: a
    , _defUpdateHeader :: a
    , _defUpdateTo :: a
    , _defUpdateWas :: a
    , _updateDefType :: a
    , _typeUpdateDialog :: a
    , _def :: a
    , _execRepl :: a
    , _extract :: a
    , _extractToOuter :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving anyclass ElemIds
    deriving Applicative via (Generically1 Definitions)
Lens.makeLenses ''Definitions
JsonTH.derivePrefixed "_" ''Definitions
