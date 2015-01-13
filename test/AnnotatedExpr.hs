module AnnotatedExpr where

import           Control.Applicative (pure, (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.State (State, runState)
import qualified Data.List as List
import           Formatting
import           Lamdu.Expr.Val (Val)
import           Text.PrettyPrint.HughesPJClass (pPrint)

type AnnotationIndex = Int
type AnnotationM = State (AnnotationIndex, [String])

addAnnotation :: String -> AnnotationM AnnotationIndex
addAnnotation msg = do
  count <- Lens.use _1
  _1 += 1
  _2 %= (msg :)
  pure count

errorMessage ::
  AnnotationM (Val [AnnotationIndex]) ->
  ([String], String)
errorMessage mkExpr =
  (resultErrs, fullMsg)
  where
    showErrItem ix err = ansiAround ansiRed ("{" ++ show ix ++ "}:\n") ++ err
    fullMsg =
      List.intercalate "\n" $
      show (pPrint (ixsStr <$> expr)) :
      "Errors:" :
      (Lens.itraversed %@~ showErrItem) resultErrs
    (expr, resultErrs) = _2 %~ reverse . snd $ runState mkExpr (0, [])
    ixsStr [] = UnescapedStr ""
    ixsStr ixs = UnescapedStr . ansiAround ansiRed . List.intercalate ", " $ map show ixs
