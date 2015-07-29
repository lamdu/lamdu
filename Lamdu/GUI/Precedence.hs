module Lamdu.GUI.Precedence
    ( Precedence(..), MyPrecedence(..), ParentPrecedence(..), needParens
    ) where

data Precedence = Precedence
    { precLeft :: {-# UNPACK #-} !Int
    , precRight :: {-# UNPACK #-} !Int
    }

instance Num Precedence where
    fromInteger x = Precedence (fromInteger x) (fromInteger x)
    x + y = Precedence (precLeft x + precLeft y) (precRight x + precRight y)
    (*) = error "instance Num Precedence.*"
    (-) = error "instance Num Precedence.-"
    abs = error "instance Num Precedence.abs"
    signum = error "instance Num Precedence.signum"

newtype MyPrecedence = MyPrecedence { unMyPrecedence :: Precedence }
newtype ParentPrecedence = ParentPrecedence { unParentPrecedence :: Precedence }

needParens :: ParentPrecedence -> MyPrecedence -> Bool
needParens (ParentPrecedence parent) (MyPrecedence my) =
    precLeft parent > precLeft my || precRight parent > precRight my
