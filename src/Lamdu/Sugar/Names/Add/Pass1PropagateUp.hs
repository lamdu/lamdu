{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell, DerivingVia #-}

module Lamdu.Sugar.Names.Add.Pass1PropagateUp
    ( Pass1PropagateUp, runPass1PropagateUp
    , P1Name(..), P1KindedName(..), P1Out(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Control.Monad.Writer as Writer
import           Control.Monad.Trans.FastWriter (Writer, runWriter, MonadWriter)
import           Data.Kind (Type)
import           Data.MMap (MMap(..))
import           Data.Set.Ordered (OSet, Bias(..), L)
import qualified Data.Set.Ordered as OrderedSet
import qualified Data.Tuple as Tuple
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Data.Tag as Tag
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Names.Add.Pass0LoadNames
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.CPS (CPS(..), runcps, liftCPS)
import qualified Lamdu.Sugar.Names.Clash as Clash
import           Lamdu.Sugar.Names.Walk (MonadNameWalk(..), Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

data P1Out = P1Out
    { _p1Globals :: Clash.NameSpaces (MMap T.Tag Clash.Collider)
        -- ^ Used in P2 to check against local hole results
    , _p1Locals :: MMap T.Tag Clash.IsClash
        -- ^ Used in P2 to check against global hole results
    , _p1Contexts :: Clash.NameSpaces (MMap T.Tag (Bias L (OSet UUID)))
        -- ^ Needed to generate suffixes
    , _p1TypeVars :: Bias L (OSet UUID)
        -- ^ Type vars met
    , _p1Texts :: Map T.Tag Tag.TextsInLang
    }
    deriving stock (Generic, Show)
    deriving (Semigroup, Monoid) via Generically P1Out
Lens.makeLenses ''P1Out

data P1KindedName
    = P1TagName Annotated.Name Tag.IsOperator Tag.TextsInLang
    | P1AnonName UUID

data P1Name = P1Name
    { p1KindedName :: P1KindedName
    , p1LocalsBelow :: MMap T.Tag Clash.IsClash
        -- ^ Allow checking collisions for names hidden behind monadic
        -- actions.
    , p1IsAutoGen :: Bool
    }
newtype Pass1PropagateUp (im :: Type -> Type) (am :: Type -> Type) a =
    Pass1PropagateUp (Writer P1Out a)
    deriving newtype (Functor, Applicative, Monad, MonadWriter P1Out)
runPass1PropagateUp :: Pass1PropagateUp i o a -> (a, P1Out)
runPass1PropagateUp (Pass1PropagateUp act) = runWriter act

tellSome :: MonadWriter w m => Lens.ASetter' w a -> a -> m ()
tellSome l v = mempty & l .~ v & Writer.tell

instance Monad i => MonadNameWalk (Pass1PropagateUp i o) where
    type OldName (Pass1PropagateUp i o) = P0Name
    type NewName (Pass1PropagateUp i o) = P1Name
    type IM (Pass1PropagateUp i o) = i
    opRun = pure (pure . fst . runPass1PropagateUp)
    opWithName = p1Name Nothing
    opGetName mDisambiguator u nameType p0Name =
        p1Name mDisambiguator u nameType p0Name & runcps
    opWithNewTag _ _ = id

p1Name ::
    Maybe Disambiguator -> Walk.IsUnambiguous -> Walk.NameType -> P0Name ->
    CPS (Pass1PropagateUp i o) P1Name
p1Name mDisambiguator u nameType (P0Name texts isOp internalName) =
    -- NOTE: We depend on the anonTag key in the map
    liftCPS (traverse_ tellCtx ctx) *>
    CPS (\inner ->
        tells
        *> inner
        & Writer.censor addTags -- censor avoids clash-skipping monoid instance
        & Writer.listen
        <&> Tuple.swap
        <&> _1 %~ \innerOut ->
        P1Name
        { p1KindedName =
            if tag == anonTag
            then
                case ctx of
                Nothing -> error "Anon tag with no context"
                Just uuid -> P1AnonName uuid
            else P1TagName aName isOp texts
        , p1LocalsBelow = innerOut ^. p1Locals
        , p1IsAutoGen = internalName ^. inIsAutoName
        }
    )
    where
        tells
            | tag /= anonTag = tag ~~> texts & tellSome p1Texts
            | otherwise = pure ()
        addTags =
            case u of
            Walk.Unambiguous -> id
            Walk.MayBeAmbiguous
                | Walk.isGlobal nameType -> p1Globals <>~ myTags
                | otherwise -> p1Locals . Lens.coerced <>~ myTags ^. Clash.nsLower
            where
                myTags = mempty & Clash.nameTypeSpace nameType .~ (tag ~~> Clash.Collider (Clash.toIsClash aName))
        tellCtx x
            | nameType == Walk.TypeVar =
                tellSome p1TypeVars (Bias (OrderedSet.singleton x))
            | otherwise =
                tellSome p1Contexts
                (mempty & Clash.nameTypeSpace nameType .~ (tag ~~> Bias (OrderedSet.singleton x)))
        InternalName ctx tag _ = internalName
        aName =
            Annotated.Name
            { Annotated._internal = internalName
            , Annotated._disambiguator = mDisambiguator
            , Annotated._nameType = nameType
            }
