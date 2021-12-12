-- | AnnotatedName
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Names.Annotated
    ( Name(..), internal, disambiguator, nameType
    , tag
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal (InternalName(..), inTag)
import           Lamdu.Sugar.Names.Walk (Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

-- | Info about a single instance of use of a name:
data Name = Name
    { _internal :: !InternalName
    , -- | Is the name used in a function application context? We consider
      -- the application as a disambiguator
      _disambiguator :: !(Maybe Disambiguator)
    , _nameType :: !Walk.NameType
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''Name

tag :: Lens' Name T.Tag
tag = internal . inTag
