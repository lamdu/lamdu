{-# LANGUAGE TemplateHaskell, DerivingVia #-}

module Lamdu.Sugar.Config where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Sugars a = Sugars
    { -- Allows assignments (`let f x = ...`) have parameters,
      -- rather than using lambdas (i.e `let f = \x -> ...`)
      _assignmentParameters
    , -- Record/case with multiple fields/alternatives, rather than a chain of rec-extends.
      -- Disabling this also imples disabling if-expressions and labeled-applies.
      _composite :: a
    , _fieldPuns :: a
    , _fragment :: a
    , _ifExpression :: a
    , _labeledApply :: a
    , _letExpression :: a
    , -- Somewhat similar to Scala's "Placeholder Syntax for Anonymous Functions",
      -- https://scala-lang.org/files/archive/spec/2.13/06-expressions.html#placeholder-syntax-for-anonymous-functions
      -- Except the underlined parameters are actually named, just don't repeat in the lambda left-hand side.
      _lightLambda :: a
    , _literalText :: a
    , _nullaryInject :: a
    , _nullaryParameter :: a
    , _destructure :: a
    , _destructureNested :: a
    } deriving stock (Eq, Show, Functor, Generic, Generic1)
    deriving Applicative via Generically1 Sugars
JsonTH.derivePrefixed "_" ''Sugars
Lens.makeLenses ''Sugars
