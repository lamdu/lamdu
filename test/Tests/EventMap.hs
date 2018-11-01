module Tests.EventMap where

import Control.Lens (runIdentity)
import GUI.Momentu.EventMap hiding (KeyEvent)
import GUI.Momentu.Main.Events (Event(..))

import Test.Lamdu.Prelude hiding (lookup)

eventMap :: EventMap Integer
eventMap =
    mconcat
    [ charEventMap "A" (Doc ["B"]) (const Nothing)
    , charEventMap "C" (Doc ["D"]) (const (Just 1))
    ]

test :: Test
test =
    assertEqual "Lookup" (Just (Doc ["D"])) (res <&> (^. dhDoc))
    & testCase "event-map-lookup for char handlers"
    where
        res =
            lookup (pure Nothing) (EventChar 'X') eventMap
            & runIdentity
