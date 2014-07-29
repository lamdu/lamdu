module AnnotatedExpr where

import Control.Applicative (pure, (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (State, runState)
import Formatting
import Text.PrettyPrint.HughesPJClass (pPrint)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Lamdu.Expr as E

type AnnotationIndex = Int
type AnnotationM = State (AnnotationIndex, [String])

addAnnotation :: String -> AnnotationM AnnotationIndex
addAnnotation msg = do
  count <- Lens.use Lens._1
  Lens._1 += 1
  Lens._2 %= (msg :)
  pure count

errorMessage ::
  AnnotationM (E.Val [AnnotationIndex]) ->
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
    (expr, resultErrs) = Lens._2 %~ reverse . snd $ runState mkExpr (0, [])
    ixsStr [] = UnescapedStr ""
    ixsStr ixs = UnescapedStr . ansiAround ansiRed . List.intercalate ", " $ map show ixs
