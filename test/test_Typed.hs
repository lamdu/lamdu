{-# OPTIONS -Wall #-}
import qualified Data.Store.Guid as Guid
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
import Control.Monad.Identity (runIdentity)

type Entity = Typed.ExpressionEntity String

mkEntity :: String -> Data.Expression Entity -> Entity
mkEntity s = Typed.ExpressionEntity s . Data.GuidExpression (Guid.fromString s)

example :: Entity
example = mkEntity "int5" $ Data.ExpressionLiteralInteger 5

loader :: Typed.Loader m
loader = Typed.Loader $ error . show

main :: IO ()
main = do
  print loaded
  print $ Typed.infer uninferredState
  return ()
  where
    (loaded, uninferredState) = runIdentity $ Typed.fromLoaded loader Nothing example
