{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.Precedence
    ( Precedence(..), make
    , MyPrecedence(..), my
    , ParentPrecedence(..), parent
    , needParens
    , precBefore, precAfter
    ) where

import qualified Control.Lens as Lens

data Precedence = Precedence
    { _precBefore :: {-# UNPACK #-} !Int
    , _precAfter :: {-# UNPACK #-} !Int
    }

Lens.makeLenses ''Precedence

make :: Int -> Precedence
make p = Precedence p p

-- instance Num Precedence where
--     fromInteger x = Precedence (fromInteger x) (fromInteger x)
--     x + y = Precedence (_precBefore x + _precBefore y) (_precAfter x + _precAfter y)
--     (*) = error "instance Num Precedence.*"
--     (-) = error "instance Num Precedence.-"
--     abs = error "instance Num Precedence.abs"
--     signum = error "instance Num Precedence.signum"

newtype MyPrecedence = MyPrecedence Precedence
newtype ParentPrecedence = ParentPrecedence Precedence

parent :: Int -> ParentPrecedence
parent = ParentPrecedence . make

my :: Int -> MyPrecedence
my = MyPrecedence . make

needParens :: ParentPrecedence -> MyPrecedence -> Bool
needParens (ParentPrecedence x) (MyPrecedence y) =
    _precBefore x > _precBefore y || _precAfter x > _precAfter y
