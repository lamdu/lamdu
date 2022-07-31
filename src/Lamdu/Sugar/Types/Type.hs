-- | Sugaring of Lamdu.Calc.Type modules/ASTs
{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Lamdu.Sugar.Types.Type
    ( Scheme(..), schemeForAll, schemeType
    , T.NominalId
    , ParamKind(..), _TypeParam, _RowParam
    , TId(..), tidName, tidTId
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Types.TaggedList (TaggedList)

import           Lamdu.Prelude

data TId name = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    } deriving (Generic, Eq)

data ParamKind = TypeParam | RowParam deriving (Eq, Ord, Generic)

data Scheme name i o = Scheme
    { _schemeForAll :: TaggedList name i o (Property o ParamKind)
    , _schemeType :: ()
    } deriving Generic

traverse Lens.makeLenses [''Scheme, ''TId] <&> concat
Lens.makePrisms ''ParamKind
