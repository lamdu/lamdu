module Lamdu.GUI.Precedence
    ( Precedence, MyPrecedence(..), ParentPrecedence(..)
    ) where

type Precedence = Int
newtype MyPrecedence = MyPrecedence Precedence
newtype ParentPrecedence = ParentPrecedence Precedence
