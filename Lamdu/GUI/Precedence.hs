{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.Precedence
    ( Precedence(..), MyPrecedence(..), ParentPrecedence(..), needParens
    , precLeft, precRight
    ) where

import qualified Control.Lens as Lens

data Precedence = Precedence
    { _precLeft :: {-# UNPACK #-} !Int
    , _precRight :: {-# UNPACK #-} !Int
    }

Lens.makeLenses ''Precedence

instance Num Precedence where
    fromInteger x = Precedence (fromInteger x) (fromInteger x)
    x + y = Precedence (_precLeft x + _precLeft y) (_precRight x + _precRight y)
    (*) = error "instance Num Precedence.*"
    (-) = error "instance Num Precedence.-"
    abs = error "instance Num Precedence.abs"
    signum = error "instance Num Precedence.signum"

newtype MyPrecedence = MyPrecedence { unMyPrecedence :: Precedence }
newtype ParentPrecedence = ParentPrecedence { unParentPrecedence :: Precedence }

needParens :: ParentPrecedence -> MyPrecedence -> Bool
needParens (ParentPrecedence parent) (MyPrecedence my) =
    _precLeft parent > _precLeft my || _precRight parent > _precRight my
