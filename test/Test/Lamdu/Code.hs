module Test.Lamdu.Code where

import           Lamdu.Calc.Term (Term)
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Expr.IRef (HRef)
import qualified Lamdu.Expr.Load as ExprLoad
import           Revision.Deltum.Transaction (Transaction)

import           Test.Lamdu.Prelude

type T = Transaction

readRepl :: T ViewM (Def.Expr (Ann (HRef ViewM) # Term))
readRepl = ExprLoad.defExpr (DbLayout.repl DbLayout.codeAnchors)
