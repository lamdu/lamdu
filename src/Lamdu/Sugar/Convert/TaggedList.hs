module Lamdu.Sugar.Convert.TaggedList where

import Lamdu.Sugar.Types.Tag (TagChoice)
import Lamdu.Sugar.Types.TaggedList

import Lamdu.Prelude

convert :: Applicative o => i (TagChoice name o) -> [TaggedItem name i o a] -> TaggedList name i o a
convert addFirst items =
    TaggedList
    { _tlAddFirst = addFirst
    , _tlItems =
        case items of
        [] -> Nothing
        (x:xs) ->
            Just TaggedListBody
            { _tlHead = x
            , _tlTail =
                -- Reordering action will be added in OrderTags phase
                xs <&> (`TaggedSwappableItem` pure ())
            }
    }
