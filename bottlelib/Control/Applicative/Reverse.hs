{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Applicative.Reverse (ReverseApplicative(..)) where

import Control.Applicative(Applicative(..), (<**>))

newtype ReverseApplicative f a =
  ReverseApplicative { runReverseApplicative :: f a }
  deriving (Functor)

instance Applicative f => Applicative (ReverseApplicative f) where
  pure = ReverseApplicative . pure
  ReverseApplicative f <*> ReverseApplicative x = ReverseApplicative $ x <**> f
