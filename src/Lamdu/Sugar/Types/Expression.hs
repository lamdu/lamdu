{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Expression
    ( Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyIfElse
    , Expression(..), rBody, rPayload
    , AnnotatedArg(..), aaTag, aaExpr
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , Fragment(..), fExpr, fHeal, fOptions
    , Lambda(..), lamFunc, lamMode
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Sugar.Types.Binder
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderMode)
import           Lamdu.Sugar.Types.Hole (Hole, HoleOption, Literal)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Simple
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

data Expression name i o a = Expression
    { _rPayload :: Payload name i o a
    , _rBody :: Body name i o (Expression name i o a)
    } deriving (Functor, Foldable, Traversable, Generic)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

data LabeledApply name i o expr = LabeledApply
    { _aFunc :: LabeledApplyFunc name i o ()
    , _aSpecialArgs :: SpecialArgs expr
    , _aAnnotatedArgs :: [AnnotatedArg name expr]
    , _aRelayedArgs :: [RelayedArg name i o]
    } deriving (Functor, Foldable, Traversable, Generic)

data Lambda name i o expr = Lambda
    { _lamMode :: BinderMode
    , _lamFunc :: Function name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name i o expr = Fragment
    { _fExpr :: expr
    , _fHeal :: Heal o
    , _fOptions :: i [HoleOption i o (Expression name i o ())]
    } deriving (Functor, Foldable, Traversable, Generic)

instance Show expr => Show (Fragment name i o expr) where
    show (Fragment expr _ _) = "(Fragment " ++ show expr ++ ")"

data Body name i o expr
    = BodyLam (Lambda name i o expr)
    | BodySimpleApply (V.Apply expr)
    | BodyLabeledApply (LabeledApply name i o expr)
    | BodyHole (Hole i o (Expression name i o ()))
    | BodyLiteral (Literal (Property o))
    | BodyRecord (Composite name i o expr)
    | BodyGetField (GetField name i o expr)
    | BodyCase (Case name i o expr)
    | BodyIfElse (IfElse name i o expr)
    | BodyInject (Inject name i o expr)
    | BodyGetVar (GetVar name o)
    | BodyToNom (Nominal name (BinderBody name i o expr))
    | BodyFromNom (Nominal name expr)
    | BodyFragment (Fragment name i o expr)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving (Functor, Foldable, Traversable, Generic)

instance (Show name, Show expr) => Show (LabeledApply name i o expr) where
    show (LabeledApply func specialArgs _annArgs _relayedArgs) =
        unwords ["LabeledApply of", show func, "with", show specialArgs, "..."]

Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Fragment
Lens.makeLenses ''Expression
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makePrisms ''Body
