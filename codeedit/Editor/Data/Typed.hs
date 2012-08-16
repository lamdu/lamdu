{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( StoredExpression(..)
  , Ref
  , RefMap
  ) where

newtype Ref = Ref { unRef :: Int }
instance Show Ref where
  show = ('P' :) . show . unRef

data TypedValue = TypedValue
  { tvVal :: Ref
  , tvType :: Ref
  }
instance Show TypedValue where
  show (TypedValue v t) = concat [show v, ":", show t]

-- Initial Pass:
-- Get Definitions expand to their types and values.
-- Use expression's structures except for Apply.
--   (because an Apply can result in something else
--    but for example an Int or Lambda stays the same)
-- Add SimpleType, Apply, LambdaOrPi, Union rules
-- Param types of Lambdas and Pis are of type Set
-- Pi result type is of type Set

-- When recursing on an expression, we remember the parent expression guids,
-- And we make sure not to add a sub-expression with a parent guid (that's a recursive structure).

-- SimpleType Rule:
-- Type of a Lambda is a Pi, with same param type
-- Type of a Pi is Set
-- Type of Set is Set
-- Type of Builtin is what's stored in it

-- Apply Rule:
--
-- Apply =
-- Func                        Arg
-- ----                        ---
-- FuncT = ParamT -> ResultT   ArgT
-- --------------------------------
-- ApplyT
--
-- ParamT <=> ArgT
-- Recurse-Subst ResultT ApplyT Arg
-- If Arg is Get Param:
--   ArgT <=> ResultT
-- Case Func of
--   Hole -> Do Nothing
--   \LParamT -> Body : BodyT:
--     LParamT <=> ParamT
--     BodyT <=> ResultT
--     Recurse-Subst Body Apply Arg
--   Other -> Copy (Func Arg) to Apply
--
-- Where Recurse-Subst PreSubst PostSubst Arg
--   Recurse over PreSubst and PostSubst together:
--     When PostSubst part is hole:
--       Replace it with structure from PreSubst and resume recursion
--     When PreSubst part refers to its param:
--       PostSubst part <=> Arg

data ApplyComponents = ApplyComponents
  { acApply :: TypedValue
  , acFunc :: TypedValue
  , acArg :: TypedValue
  }

-- LambdaOrPi Rule:
-- Each of expr parts:
--   Part <=> Stored

data LambdaComponents = LambdaComponents
  { lcParent :: Ref
  , lcParamType :: Ref
  , lcResult :: Ref
  }

-- Union rule (get param, but also for recursive type)

data Rule
  = RuleSimpleType TypedValue
  | RuleUnion Ref Ref
  | RuleLambdaStructure LambdaComponents
  | RulePiStructure LambdaComponents
  | RuleApply ApplyComponents

data Conflict = Conflict
  { cExpression :: Data.PureGuidExpression
  , cRule :: Rule
  }

data RefData = RefData
  { rExpression :: Data.PureGuidExpression
  , rRules :: [Rule]
  , rErrors :: [Conflict]
  }

type RefMap = IntMap RefData
data InferState queue = InferState
  { _sRefMap :: RefMap
  , _sTouchedRefs :: IntSet
  }
LensTH.makeLenses ''InferState

data StoredExpression m = StoredExpression
  { _eProp :: Data.ExpressionIRefProperty m
  , _eValue :: Data.Expression (StoredExpression m)
  , _eInferred :: TypedValue
  }
LensTH.makeLenses ''StoredExpression

instance Show (StoredExpression s) where
  show (Expression prop value inferred) =
    unwords
    [ "("
    , show (Data.eipGuid prop), ":"
    , show value, "="
    , show inferred
    , ")"
    ]

toPureExpression ::
  Monad m => Expression s -> Data.PureGuidExpression
toPureExpression expr =
  Data.pureGuidExpression
  (eipGuid (expr ^. eProp)) .
  fmap toPureExpression $ expr ^. eValue

type T = Transaction ViewTag

-- Initial Pass:
-- Get Definitions expand to their types and values.
-- Use expression's structures except for Apply.
--   (because an Apply can result in something else
--    but for example an Int or Lambda stays the same)
-- Add SimpleType, Apply, LambdaOrPi, Union rules
-- Param types of Lambdas and Pis are of type Set
-- Pi result type is of type Set

fromLoaded ::
  Monad m =>
  Maybe Data.DefinitionIRef ->
  DataLoad.ExpressionEntity (T m) -> T m (StoredExpression (T m), InferState)
fromLoaded mRecursiveIRef rootEntity =
  (`runStateT` InferState mempty mempty) $ do
    rootTv <- createTopLevel rootEntity
  where
    createTopLevel entity = do
      typeRef <- createRef 
