{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker) where

import Editor.Anchors(ViewTag)
import Editor.CTransaction(TWidget)
import Editor.CodeEdit.Ancestry(ExpressionAncestry)
import Editor.DataOps(ExpressionPtr)

type ExpressionEditMaker m =
  ExpressionAncestry m -> ExpressionPtr m -> TWidget ViewTag m
