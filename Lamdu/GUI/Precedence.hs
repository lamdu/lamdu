{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.Precedence
    ( Precedence(..), MyPrecedence(..), ParentPrecedence(..), needParens
    , precBefore, precAfter
    ) where

import qualified Control.Lens as Lens

data Precedence = Precedence
    { _precBefore :: {-# UNPACK #-} !Int
    , _precAfter :: {-# UNPACK #-} !Int
    }

Lens.makeLenses ''Precedence

instance Num Precedence where
    fromInteger x = Precedence (fromInteger x) (fromInteger x)
    x + y = Precedence (_precBefore x + _precBefore y) (_precAfter x + _precAfter y)
    (*) = error "instance Num Precedence.*"
    (-) = error "instance Num Precedence.-"
    abs = error "instance Num Precedence.abs"
    signum = error "instance Num Precedence.signum"

newtype MyPrecedence = MyPrecedence Precedence
newtype ParentPrecedence = ParentPrecedence Precedence

needParens :: ParentPrecedence -> MyPrecedence -> Bool
needParens (ParentPrecedence parent) (MyPrecedence my) =
    _precBefore parent > _precBefore my || _precAfter parent > _precAfter my
