module Lamdu.Sugar.Convert.Nominal
    ( convertToNom, convertFromNom
    ) where

import           Hyper (Tree, Ann(..))
import           Hyper.Type.AST.Nominal (ToNom(..))
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
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

convertToNom ::
    (Monad m, Monoid a) =>
    Tree (Ann (Input.Payload m a)) (ToNom NominalId V.Term) ->
    ConvertM m (ExpressionU m a)
convertToNom (Ann pl nom@(ToNom tid x)) =
    do
        ConvertText.text nom pl & justToLeft
        Nominal
            <$> ConvertTId.convert tid
            <*> ConvertBinder.convertBinder x
            <&> BodyToNom
            >>= addActions [x] pl
            & lift
    & runMatcherT

convertFromNom ::
    (Monad m, Monoid a) =>
    NominalId -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertFromNom tid pl =
    ConvertTId.convert tid <&> BodyFromNom >>= addActions [] pl
