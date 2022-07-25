-- | Sugaring of Lamdu.Calc.Type modules/ASTs
{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Lamdu.Sugar.Types.Type
    ( Scheme(..), schemeForAll, schemeType
    , T.NominalId
    , TId(..), tidName, tidTId
    ) where

import qualified Control.Lens as Lens
import           Hyper.Syntax.Scheme (QVars)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Prelude

data TId name = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    } deriving (Generic, Eq)


data Scheme name = Scheme
    { _schemeForAll :: T.Types # QVars
    , _schemeType :: ()
    } deriving Generic

traverse Lens.makeLenses [''Scheme, ''TId] <&> concat
