{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Lamdu.Eval.Val
    ( ValHead, ValBody(..)
    , ThunkId(..), thunkIdInt
    , ScopeId(..), scopeIdInt, topLevelScopeId
    , Closure(..), Scope(..)
    , emptyScope
    , _HFunc, _HRecExtend, _HCase, _HRecEmpty, _HAbsurd, _HInteger, _HBuiltin
    , bitraverse, payloads, children
    ) where

import           Control.Applicative (Applicative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Binary (Binary)
import           Data.Foldable (Foldable)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Traversable (Traversable)
import           Lamdu.Data.Definition (FFIName)
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V

newtype ThunkId = ThunkId { getThunkId :: Int }
    deriving (Show, Eq, Ord)

newtype ScopeId = ScopeId { getScopeId :: Int }
    deriving (Show, Eq, Ord, Binary)

thunkIdInt :: Lens.Iso' ThunkId Int
thunkIdInt = Lens.iso getThunkId ThunkId

data Scope = Scope
    { _scopeMap :: Map V.Var ThunkId
    , _scopeId :: ScopeId
    } deriving (Show)

scopeIdInt :: Lens.Iso' ScopeId Int
scopeIdInt = Lens.iso getScopeId ScopeId

data Closure pl = Closure
    { _cOuterScope :: Scope
    , _cLam :: V.Lam (Val pl)
    , _cLamPayload :: pl
    } deriving (Show, Functor, Foldable, Traversable)

data ValBody val pl
    = HFunc (Closure pl)
    | HRecExtend (V.RecExtend val)
    | HRecEmpty
    | HAbsurd
    | HCase (V.Case val)
    | HInteger Integer
    | HBuiltin FFIName
    | HInject (V.Inject val)
    deriving (Functor, Foldable, Traversable)

instance (Show pl, Show val) => Show (ValBody val pl) where
    show (HFunc closure) = show closure
    show (HRecExtend recExtend) = show recExtend
    show (HCase case_) = show case_
    show (HInject inject) = show inject
    show HRecEmpty = "{}"
    show HAbsurd = "Absurd"
    show (HInteger x) = show x
    show (HBuiltin ffiName) = show ffiName

Lens.makePrisms ''ValBody

bitraverse ::
    Applicative f =>
    (va -> f vb) -> (a -> f b) ->
    ValBody va a -> f (ValBody vb b)
bitraverse val _  (HRecExtend x) = Lens.traverse val x <&> HRecExtend
bitraverse val _  (HCase x)      = Lens.traverse val x <&> HCase
bitraverse val _  (HInject x)    = Lens.traverse val x <&> HInject
bitraverse _   pl (HFunc x)      = Lens.traverse pl  x <&> HFunc
bitraverse _   _  HRecEmpty      = pure HRecEmpty
bitraverse _   _  HAbsurd        = pure HAbsurd
bitraverse _   _  (HInteger i)   = pure (HInteger i)
bitraverse _   _  (HBuiltin bi)  = pure (HBuiltin bi)

payloads :: Lens.Traversal (ValBody val a) (ValBody val b) a b
payloads = bitraverse pure

children :: Lens.Traversal (ValBody va a) (ValBody vb a) va vb
children f = bitraverse f pure

type ValHead = ValBody ThunkId

topLevelScopeId :: ScopeId
topLevelScopeId = ScopeId 0

emptyScope :: Scope
emptyScope = Scope Map.empty topLevelScopeId
