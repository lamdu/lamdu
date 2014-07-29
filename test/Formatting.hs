module Formatting where

import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as PP

data UnescapedStr = UnescapedStr String
instance Show UnescapedStr where show (UnescapedStr x) = x
instance Pretty UnescapedStr where pPrint (UnescapedStr x) = PP.text x

ansiRed :: String
ansiRed = "\ESC[31m"
ansiYellow :: String
ansiYellow = "\ESC[1;33m"
ansiReset :: String
ansiReset = "\ESC[0m"
ansiAround :: String -> String -> String
ansiAround prefix x = prefix ++ x ++ ansiReset
