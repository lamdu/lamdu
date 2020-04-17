{-# LANGUAGE TypeApplications, ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}

module Revision.Deltum.Hyper
    ( HStore(..), readRecursively
    , HRef(..), iref, setIref
    , toHRefs
    , Write(..), _WriteNew, _ExistingRef
    , writeRecursively
    ) where

import qualified Control.Lens as Lens
import           Data.UUID.Types (UUID)
import           Hyper
import           Hyper.Class.Context (HContext(..))
import           Hyper.Recurse
import           Hyper.Type.Functor (F(..), _F)
import           Revision.Deltum.IRef (IRef, uuid)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

class (Monad m, HTraversable h, HContext h, Binary (h # F (IRef m))) => HStore m h where
    hstoreRecursive :: Proxy m -> Proxy h -> Dict (HNodesConstraint h (HStore m))
    {-# INLINE hstoreRecursive #-}
    default hstoreRecursive ::
        HNodesConstraint h (HStore m) =>
        Proxy m -> Proxy h -> Dict (HNodesConstraint h (HStore m))
    hstoreRecursive _ _ = Dict

instance Recursive (HStore m) where
    recurse = hstoreRecursive (Proxy @m) . proxyArgument

-- TODO: Better name?
data HRef m h = HRef
    { _iref :: F (IRef m) h
    , _setIref :: F (IRef m) h -> T m ()
    }
Lens.makeLenses ''HRef

data Write m h
    = WriteNew
    | ExistingRef (F (IRef m) h)
    deriving (Generic, Binary)
Lens.makePrisms ''Write

readRecursively ::
    HStore m t =>
    F (IRef m) # t ->
    T m (Ann (F (IRef m)) # t)
readRecursively = readRecursivelyH mempty

readRecursivelyH ::
    forall m t.
    HStore m t =>
    Set UUID ->
    F (IRef m) # t ->
    T m (Ann (F (IRef m)) # t)
readRecursivelyH visited x
    | visited ^. Lens.contains k = error $ "Recursive reference: " ++ show x
    | otherwise =
        withDict (recurse (Proxy @(HStore m t))) $
        Transaction.readIRef (x ^. _F)
        >>= htraverse (Proxy @(HStore m) #> readRecursivelyH (visited & Lens.contains k .~ True))
        <&> Ann x
    where
        k = uuid (x ^. _F)

toHRefs ::
    forall h m p.
    HStore m h =>
    (F (IRef m) # h -> T m ()) ->
    Ann (F (IRef m) :*: p) # h ->
    Ann (HRef m :*: p) # h
toHRefs setValI (Ann (i :*: a) body) =
    withDict (recurse (Proxy @(HStore m h))) $
    hcontext body
    & hmap
        ( Proxy @(HStore m) #>
            \(HFunc s :*: x) ->
            toHRefs
            ( \n ->
                x & hAnn . _1 .~ n
                & s
                & getConst
                & hmap (const (^. hAnn . _1))
                & Transaction.writeIRef (i ^. _F)
            ) x
        )
    & Ann (HRef i setValI :*: a)

writeRecursively ::
    forall m t a.
    HStore m t =>
    Ann (Write m :*: a) # t ->
    T m (Ann (F (IRef m) :*: a) # t)
writeRecursively expr =
    withDict (recurse (Proxy @(HStore m t))) $
    do
        body <- expressionBodyFrom expr
        let bodyWithRefs = hmap (const (^. hAnn . _1)) body
        case mIRef of
            ExistingRef valI ->
                Ann (valI :*: pl) body <$
                Transaction.writeIRef (valI ^. _F) bodyWithRefs
            WriteNew ->
                Transaction.newIRef bodyWithRefs
                <&> \x -> Ann (_F # x :*: pl) body
    where
        mIRef :*: pl = expr ^. hAnn

expressionBodyFrom ::
    forall m t a.
    HStore m t =>
    Ann (Write m :*: a) # t ->
    T m (t # Ann (F (IRef m) :*: a))
expressionBodyFrom =
    withDict (recurse (Proxy @(HStore m t))) $
    htraverse (Proxy @(HStore m) #> writeRecursively) . (^. hVal)
