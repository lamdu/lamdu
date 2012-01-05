{-# OPTIONS -O2 -Wall #-}

module Data.Store.BottleWidgets
    (MWidget,
     appendBoxChild, popCurChild,
     makeBox, makeTextEdit, makeChoiceWidget,
     widgetDownTransaction)
where

--import qualified Graphics.UI.Bottle.Widgets.Completion as Completion
import Control.Monad (when, liftM)
import Data.List.Utils (index)
import Data.Maybe (listToMaybe)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Store.Transaction (Transaction, Store)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

safeIndex :: Integral ix => ix -> [a] -> Maybe a
safeIndex n = listToMaybe . drop (fromIntegral n)

type MWidget m = m (Bool -> Widget (m ()))

appendBoxChild ::
  Monad m =>
  Transaction.Property t m Box.Cursor ->
  Transaction.Property t m [a] ->
  a -> Transaction t m ()
appendBoxChild boxModelRef valuesRef value = do
  values <- Property.get valuesRef
  Property.set valuesRef (values ++ [value])
  Property.set boxModelRef . length $ values

popCurChild ::
  Monad m =>
  Transaction.Property t m Box.Cursor ->
  Transaction.Property t m [a] ->
  Transaction t m (Maybe a)
popCurChild boxModelRef valuesRef = do
  values <- Property.get valuesRef
  curIndex <- Property.get boxModelRef
  let value = curIndex `safeIndex` values
  maybe (return ()) (delChild curIndex values) value
  return value
  where
    delChild curIndex values _child = do
      Property.set valuesRef (curIndex `removeAt` values)
      when (curIndex >= length values - 1) .
        Property.pureModify boxModelRef $ subtract 1

makeBox ::
  Monad m =>
  Box.Orientation ->
  [Bool -> Widget (Transaction t m ())] ->
  Transaction.Property t m Box.Cursor ->
  MWidget (Transaction t m)
makeBox orientation rows boxCursorRef =
  fromCursor `liftM` Property.get boxCursorRef
  where
    fromCursor cursor hasFocus =
      Box.make orientation (Property.set boxCursorRef) cursor rows hasFocus

makeWidget ::
  Monad m =>
  (model -> Bool -> Widget model) ->
  Transaction.Property t m model ->
  MWidget (Transaction t m)
makeWidget mkWidget ref = do
  model <- Property.get ref
  return $ fmap (Property.set ref) . mkWidget model

makeTextEdit ::
  Monad m => TextEdit.Style -> String -> Anim.AnimId ->
  Transaction.Property t m TextEdit.Model ->
  MWidget (Transaction t m)
makeTextEdit = (result . result) (result makeWidget . flip) $ TextEdit.make
-- makeCompletion :: Monad m =>
--                   Completion.Theme ->
--                   [(String, Int)] -> Int -> String -> Int ->
--                   Transaction.Property t m Completion.Model ->
--                   MWidget (Transaction t m)
-- makeCompletion =
--   (result . result . result .
--    result . result) makeWidget Completion.make

-- makeSimpleCompletion :: Monad m =>
--                         Completion.Theme ->
--                         [String] -> Int -> String -> Int ->
--                         Transaction.Property t m Completion.Model ->
--                         MWidget (Transaction t m)
-- makeSimpleCompletion =
--   (result . result . result .
--    result . result) makeWidget Completion.makeSimple

makeChoiceWidget :: Monad m =>
                    Box.Orientation ->
                    [(Bool -> Widget (Transaction t m ()), k)] ->
                    Transaction.Property t m Box.Cursor ->
                    Transaction t m (Bool -> Widget (Transaction t m ()), k)
makeChoiceWidget orientation keys boxModelRef = do
  widget <- makeBox orientation widgets boxModelRef
  itemIndex <- Property.get boxModelRef
  return (widget, unsafeUnjust "Given empty items" . index items $ min maxIndex itemIndex)
  where
    maxIndex = length items - 1
    widgets = map fst keys
    items = map snd keys

-- Take a widget parameterized on transaction on views (that lives in
-- a nested transaction monad) and convert it to one parameterized on
-- the nested transaction
widgetDownTransaction ::
  Monad m => Store t m ->
  MWidget (Transaction t m) ->
  MWidget m
widgetDownTransaction store = runTrans . (liftM . fmap . fmap) runTrans
  where
    runTrans = Transaction.run store
