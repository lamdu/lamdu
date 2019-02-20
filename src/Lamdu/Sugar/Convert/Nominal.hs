module Lamdu.Sugar.Convert.Nominal
    ( convertAppliedFromNom, convertToNom
    ) where

import           AST (Tree)
import           AST.Knot.Ann (Ann, val)
import           AST.Term.Nominal (ToNom(..))
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Extended (maybeToMPlus)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Text as ConvertText
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertNom ::
    Monad m =>
    NominalId -> expr -> ConvertM m (Nominal InternalName expr)
convertNom tid x =
    ConvertTId.convert tid
    <&> \tidS ->
    Nominal
    { _nTId = tidS
    , _nVal = x
    }

convertAppliedFromNom ::
    (Monad m, Monoid a) =>
    Tree (V.Apply V.Term) (Ann (Input.Payload m a)) ->
    ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedFromNom (V.Apply funcI argI) argS pl =
    do
        tid <- funcI ^? val . V._BLeaf . V._LFromNom & maybeToMPlus
        convertNom tid argS <&> BodyFromNom
            >>= addActions [funcI, argI] pl
            & lift

convertToNom ::
    (Monad m, Monoid a) =>
    Tree (ToNom NominalId V.Term) (Ann (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertToNom nom@(ToNom tid x) pl =
    do
        ConvertText.text nom pl & justToLeft
        ConvertBinder.convertBinder x
            >>= convertNom tid <&> BodyToNom
            >>= addActions [x] pl
            & lift
    & runMatcherT
