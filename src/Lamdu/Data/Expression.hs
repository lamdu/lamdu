{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expression
  ( VariableRef(..), _ParameterRef, _DefinitionRef
  , LamKind(..), _KindLambda, _KindPi
  , Lambda(..), lambdaKind, lambdaParamId, lambdaParamType, lambdaResult
  , Apply(..), applyFunc, applyArg
  , Leaf(..), _GetVariable, _LiteralInteger, _Hole, _Set, _IntegerType
  , Body(..), _BodyLam, _BodyApply, _BodyLeaf
  , BodyExpr
  , Expression(..), eBody, ePayload
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import qualified Control.Lens.TH as LensTH

data LamKind = KindLambda | KindPi
  deriving (Eq, Ord, Show, Typeable)

data Lambda expr = Lambda
  { _lambdaKind :: LamKind
  , _lambdaParamId :: {-# UNPACK #-}!Guid
  , _lambdaParamType :: expr
  -- TODO: Rename to _lambdaResult (for Pi it is not a body)
  , _lambdaResult :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Apply expr = Apply
  { _applyFunc :: expr
  , _applyArg :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
instance Applicative Apply where
  pure x = Apply x x
  Apply f0 a0 <*> Apply f1 a1 = Apply (f0 f1) (a0 a1)

data VariableRef def
  = ParameterRef {-# UNPACK #-} !Guid -- of the lambda/pi
  | DefinitionRef def
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Leaf def
  = GetVariable !(VariableRef def)
  | LiteralInteger !Integer
  | Set
  | IntegerType
  | Hole
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Body def expr
  = BodyLam {-# UNPACK #-}!(Lambda expr)
  | BodyApply {-# UNPACK #-} !(Apply expr)
  | BodyLeaf !(Leaf def)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type BodyExpr def a = Body def (Expression def a)

instance (Show expr, Show def) => Show (Body def expr) where
  show (BodyLam (Lambda KindLambda paramId paramType body)) =
    concat ["\\", show paramId, ":", showP paramType, "==>", showP body]
  show (BodyLam (Lambda KindPi paramId paramType body)) =
    concat ["(", show paramId, ":", showP paramType, ")->", showP body]
  show (BodyApply (Apply func arg)) = unwords [showP func, showP arg]
  show (BodyLeaf (GetVariable (ParameterRef guid))) = "par:" ++ show guid
  show (BodyLeaf (GetVariable (DefinitionRef defI))) = "def:" ++ show defI
  show (BodyLeaf (LiteralInteger int)) = show int
  show (BodyLeaf x) = show x

showP :: Show a => a -> String
showP = parenify . show

parenify :: String -> String
parenify x = concat ["(", x, ")"]

-- TODO: Expression = Cofree, do we want to use that?
data Expression def a = Expression
  { _eBody :: Body def (Expression def a)
  , _ePayload :: a
  } deriving (Functor, Eq, Ord, Foldable, Traversable, Typeable)

instance (Show a, Show def) => Show (Expression def a) where
  show (Expression body payload) =
    show body ++ showPayload
    where
      showPayload =
        case show payload of
        "()" -> ""
        x -> "{" ++ x ++ "}"

derive makeBinary ''LamKind
derive makeNFData ''LamKind
LensTH.makePrisms ''VariableRef
LensTH.makePrisms ''Leaf
LensTH.makePrisms ''Body
LensTH.makePrisms ''LamKind
LensTH.makeLenses ''Expression
LensTH.makeLenses ''Lambda
LensTH.makeLenses ''Apply

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [''VariableRef, ''Lambda, ''Apply, ''Leaf, ''Body, ''Expression]
