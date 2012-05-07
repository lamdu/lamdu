{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.Ancestry(
  ExpressionAncestry, AncestryItem(..), getAncestryParams,
  ApplyParent(..), atApRole, atApFuncType, atApApply, atApParentPtr, ApplyRole(..), FuncType(..),
  LambdaParent(..), atLpFunc, atLpParentI,
  WhereParent(..), atWpWhere, atWpRole, WhereRole(..),
  ParamTypeParent(..), atPtParamI, isAncestryRHS)
where

import Data.Store.Guid(Guid)
import Data.Store.IRef(IRef)
import Editor.DataOps(ExpressionPtr)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Data as Data

data FuncType = Prefix | InfixLeft | InfixRight
  deriving (Eq, Ord, Show, Read)

data ApplyRole = ApplyFunc | ApplyArg
  deriving (Show, Read, Eq, Ord)

data ApplyParent m = ApplyParent
  { apRole :: ApplyRole
  , apFuncType :: FuncType
  , apApply :: Data.Apply
  , apParentPtr :: ExpressionPtr m
  }
AtFieldTH.make ''ApplyParent

data LambdaParent m = LambdaParent
  { lpFunc :: Sugar.Func m
  , lpParentI :: IRef Data.Expression
  }
AtFieldTH.make ''LambdaParent

data WhereRole
  = WhereBody
  | WhereDef (IRef Data.Expression)
  deriving (Show, Read, Eq, Ord)

data WhereParent m = WhereParent
  { wpWhere :: Sugar.Where m
  , wpRole :: WhereRole
  }
AtFieldTH.make ''WhereParent

newtype ParamTypeParent = ParamTypeParent
  { ptParamI :: IRef Data.Expression
  }
AtFieldTH.make ''ParamTypeParent

data AncestryItem m =
    AncestryItemApply (ApplyParent m)
  | AncestryItemLambda (LambdaParent m)
  | AncestryItemParamType ParamTypeParent
  | AncestryItemWhere (WhereParent m)

type ExpressionAncestry m = [AncestryItem m]

getAncestryParams :: ExpressionAncestry m -> [Guid]
getAncestryParams =
  map Sugar.guid . concatMap params
  where
    params (AncestryItemLambda (LambdaParent (Sugar.Func items _) _)) = map Sugar.fpActions items
    params (AncestryItemWhere (WhereParent (Sugar.Where items _) _)) = map Sugar.wiActions items
    params _ = []

isAncestryRHS :: ExpressionAncestry m -> Bool
isAncestryRHS [AncestryItemLambda _] = True
isAncestryRHS (AncestryItemLambda _ : AncestryItemWhere _ : _) = True
isAncestryRHS _ = False
