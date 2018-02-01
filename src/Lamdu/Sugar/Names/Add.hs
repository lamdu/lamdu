{-# LANGUAGE NoImplicitPrelude, LambdaCase, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, DeriveGeneric, NoMonomorphismRestriction, OverloadedStrings #-}
module Lamdu.Sugar.Names.Add
    ( addToWorkArea
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (Writer, runWriter, MonadWriter)
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.Monad.Trans.Reader (Reader, runReader)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (runState, evalState)
import qualified Data.Char as Char
import           Data.List (partition)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Monoid.Generic (def_mempty, def_mappend)
import qualified Data.Set as Set
import           Data.Set.Ordered (OrderedSet)
import qualified Data.Set.Ordered as OrderedSet
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import           Lamdu.Data.Anchors (assocNameRef)
import           Lamdu.Name
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..), runcps)
import           Lamdu.Sugar.Names.Clash (IsClash(..))
import qualified Lamdu.Sugar.Names.Clash as Clash
import           Lamdu.Sugar.Names.NameGen (NameGen)
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Names.Walk (MonadNaming, Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction
type StoredName = Text

------------------------------
---------- Pass 0 ------------
------------------------------
data P0Name = P0Name
    { _p0StoredName :: Maybe StoredName
    , _p0uuid :: UUID
    }

newtype Pass0LoadNames tm a = Pass0LoadNames { runPass0LoadNames :: T tm a }
    deriving (Functor, Applicative, Monad)

instance Monad tm => MonadNaming (Pass0LoadNames tm) where
    type OldName (Pass0LoadNames tm) = InternalName
    type NewName (Pass0LoadNames tm) = P0Name
    type SM (Pass0LoadNames tm) = tm
    opRun = pure runPass0LoadNames
    opWithParamName _ _ = p0cpsNameConvertor
    opWithLetName _ = p0cpsNameConvertor
    opGetName _ _ = p0nameConvertor

getP0Name :: Monad tm => InternalName -> Pass0LoadNames tm P0Name
getP0Name internalName =
    Pass0LoadNames $ do
        nameStr <- assocNameRef (internalName ^. inUUID) & Transaction.getP
        pure P0Name
            { _p0StoredName = if Text.null nameStr then Nothing else Just nameStr
            , _p0uuid = internalName ^. inUUID
            }

p0nameConvertor :: Monad tm => Walk.NameConvertor (Pass0LoadNames tm)
p0nameConvertor = getP0Name

p0cpsNameConvertor :: Monad tm => Walk.CPSNameConvertor (Pass0LoadNames tm)
p0cpsNameConvertor uuid =
    CPS $ \k -> (,) <$> getP0Name uuid <*> k

------------------------------
---------- Pass 1 ------------
------------------------------

-- Wrap the Map for a more sensible (recursive) Monoid instance
newtype NameUUIDMap = NameUUIDMap { _nameUUIDMap :: Map Text (OrderedSet Clash.AnnotatedName) }
    deriving Show
Lens.makeLenses ''NameUUIDMap

instance Semigroup NameUUIDMap where
    NameUUIDMap x <> NameUUIDMap y = NameUUIDMap (Map.unionWith (flip mappend) x y)

instance Monoid NameUUIDMap where
    mempty = NameUUIDMap Map.empty
    mappend = (<>)

nameUUIDMapSingleton :: Text -> Clash.AnnotatedName -> NameUUIDMap
nameUUIDMapSingleton name nameInstance =
    OrderedSet.singleton nameInstance & Map.singleton name & NameUUIDMap

isLocal :: Walk.NameType -> Bool
isLocal Walk.FieldParamName = True
isLocal Walk.ParamName = True
isLocal _ = False

isLocalName :: Clash.AnnotatedName -> Bool
isLocalName = isLocal . (^. Clash.niNameType)

removeEmpty :: NameUUIDMap -> NameUUIDMap
removeEmpty = nameUUIDMap %~ Map.filter (not . OrderedSet.null)

localNames :: NameUUIDMap -> NameUUIDMap
localNames nameMap =
    nameMap
    & nameUUIDMap . Lens.mapped %~ OrderedSet.filter isLocalName
    & removeEmpty

globalNames :: NameUUIDMap -> NameUUIDMap
globalNames nameMap =
    nameMap
    & nameUUIDMap . Lens.mapped %~ OrderedSet.filter (not . isLocalName)
    & removeEmpty

data P1Out = P1Out
    { _p1Names :: NameUUIDMap
    , _p1Collisions :: Set Text
    } deriving (Generic)
instance Semigroup P1Out where
    (<>) = def_mappend
instance Monoid P1Out where
    mempty = def_mempty
    mappend = (<>)

data P1Name = P1Name
    { p1StoredName :: Maybe StoredName
    , p1Instance :: Clash.AnnotatedName
    , -- | We keep the names below each node so we can check if an
      -- auto-generated name (in pass2) collides with any name in
      -- inner scopes (below)
      p1NamesBelow :: NameUUIDMap
    }
newtype Pass1PropagateUp (tm :: * -> *) a = Pass1PropagateUp (Writer P1Out a)
    deriving (Functor, Applicative, Monad, MonadWriter P1Out)
runPass1PropagateUp :: Pass1PropagateUp tm a -> (a, P1Out)
runPass1PropagateUp (Pass1PropagateUp act) = runWriter act & _2 %~ p1PostProcess

globalCollisions :: NameUUIDMap -> Set Text
globalCollisions (NameUUIDMap names) =
    Map.filter (namesClash . (^.. Lens.folded)) names & Map.keysSet
    where
        namesClash ns =
            case Clash.check globals of
            Clash -> True
            noClash -> any (Clash.isClash . (noClash <>) . Clash.isClashOf) locals
            where
                (locals, globals) = partition isLocalName ns

-- | Compute the global collisions to form ALL collisions and yield
-- the global names only
p1PostProcess :: P1Out -> P1Out
p1PostProcess (P1Out names localCollisions) =
    P1Out names (localCollisions <> globalCollisions names)

p1ListenNames :: Pass1PropagateUp tm a -> Pass1PropagateUp tm (a, NameUUIDMap)
p1ListenNames act = Writer.listen act <&> _2 %~ _p1Names

instance Monad tm => MonadNaming (Pass1PropagateUp tm) where
    type OldName (Pass1PropagateUp tm) = P0Name
    type NewName (Pass1PropagateUp tm) = P1Name
    type SM (Pass1PropagateUp tm) = tm
    opRun = pure (pure . fst . runPass1PropagateUp)
    opWithParamName GetFieldParameter _ = pass1Result Nothing Walk.FieldParamName
    opWithParamName GetParameter _ = pass1Result Nothing Walk.ParamName
    opWithLetName _ = pass1Result Nothing Walk.ParamName
    opGetName mDisambiguator nameType p0Name =
        pass1Result mDisambiguator nameType p0Name & runcps

unnamedStr :: Text
unnamedStr = "Unnamed"

pass1Result ::
    Maybe Disambiguator -> Walk.NameType -> P0Name ->
    CPS (Pass1PropagateUp tm) P1Name
pass1Result mDisambiguator nameType (P0Name mName uuid) =
    CPS $ \inner ->
    do
        (r, namesBelow) <- p1ListenNames inner
        let newNamesBelow = myNameUUIDMap `mappend` namesBelow
        let checkLocalCollision name =
                localNames newNamesBelow ^.. nameUUIDMap . Lens.ix name . Lens.folded
                & Clash.check & Clash.isClash
        let mGivenName =
                -- TODO: Once we ALWAYS have associated tags (except
                -- for local single param lambda) via prevention of
                -- invalid states, just use: `mGivenName = mName`
                case (nameType, mName) of
                (Walk.ParamName, Nothing) -> Nothing
                (_, Just name) -> Just name
                (_, _) -> Just unnamedStr
        let localCollisions =
                case mGivenName of
                Just name
                    | isLocal nameType && checkLocalCollision name -> Set.singleton name
                _ -> mempty
        Writer.tell P1Out { _p1Names = myNameUUIDMap, _p1Collisions = localCollisions }
        pure
            ( P1Name
                { p1StoredName = mName
                , p1Instance = nameInstance
                , p1NamesBelow = newNamesBelow
                }
            , r
            )
    where
        myNameUUIDMap =
            case (nameType, mName) of
            (_, Just name) -> Just name
            (Walk.ParamName, Nothing) -> mempty
            (_, Nothing) -> Just unnamedStr
            & foldMap singleton
        nameInstance =
            Clash.AnnotatedName
            { _niUUID = uuid
            , _niDisambiguator = mDisambiguator
            , _niNameType = nameType
            }
        singleton nameText = nameUUIDMapSingleton nameText nameInstance

------------------------------
---------- Pass 2 ------------
------------------------------

data P2Env = P2Env
    { _p2NameGen :: NameGen UUID
    , _p2NameSuffixes :: Map UUID Int
    , -- | Names used in containing scopes (above) -- used to avoid
      -- generating an automatic name that collides with a name above
      -- and to generate "UnknownCollision" inside hole results
      _p2NamesAbove :: Map Text IsClash
    }
Lens.makeLenses ''P2Env

-- | Textual Name ambiguity
--
-- In the visible grammar, there are different types of names (see
-- Walk.NameType):
-- DefName, TagName, NominalName, ParamName
--
-- Each type can collide with itself. Nominals can only collide with
-- themselves (due to their grammatic context being unique).
--
-- Definitions and tags cannot collide with each other but both
-- can collide with param names.
--
-- Hence, we check collisions in three groups:
-- * NominalIds
-- * Tags+FieldParams
-- * Defs+FieldParams+Vars
--
-- Defs+Params can also be disambiguated if used exclusively in
-- labeled apply contexts, and with differing signatures.

uuidSuffixes :: OrderedSet Clash.AnnotatedName -> Map UUID Int
uuidSuffixes nameInstances =
    nameInstances ^.. Lens.folded . Clash.niUUID
    & List.nub
    & (zip ?? [0..])
    & Map.fromList

isReserved :: Text -> Bool
isReserved name =
    name `Set.member` reservedWords
    || (name ^? Lens.ix 0 <&> Char.isDigit & fromMaybe False)
    where
        reservedWords =
            Set.fromList
            [ "if", "elif", "else"
            , "case", "of"
            , "let"
            , "or"
            , "λ", "«", "»", "Ø", "|", ".", "→", "➾"
            ]


initialP2Env :: P1Out -> P2Env
initialP2Env (P1Out names collisions) =
    P2Env
    { _p2NameGen = NameGen.initial
    , _p2NamesAbove =
        globalNames names ^. nameUUIDMap <&> (^.. Lens.folded)
        <&> Clash.check
    , _p2NameSuffixes = names ^@.. nameUUIDMap . Lens.ifolded <&> f & mconcat
    }
    where
        f (name, insts)
            | name `Set.member` collisions
            || isReserved name = uuidSuffixes insts
            | otherwise = mempty

newtype Pass2MakeNames (tm :: * -> *) a = Pass2MakeNames (Reader P2Env a)
    deriving (Functor, Applicative, Monad)
runPass2MakeNames :: P2Env -> Pass2MakeNames tm a -> a
runPass2MakeNames initial (Pass2MakeNames act) = runReader act initial
p2GetEnv :: Pass2MakeNames tm P2Env
p2GetEnv = Pass2MakeNames Reader.ask
p2WithEnv :: (P2Env -> P2Env) -> Pass2MakeNames tm a -> Pass2MakeNames tm a
p2WithEnv f (Pass2MakeNames act) = Pass2MakeNames $ Reader.local f act

runPass2MakeNamesInitial :: P1Out -> Pass2MakeNames tm a -> a
runPass2MakeNamesInitial = runPass2MakeNames . initialP2Env

setUuidName :: Monad tm => UUID -> StoredName -> T tm ()
setUuidName = Transaction.setP . assocNameRef

getCollision :: Text -> Clash.AnnotatedName -> P2Env -> Collision
getCollision name inst env =
    case env ^. p2NameSuffixes . Lens.at (inst ^. Clash.niUUID) of
    Just suffix -> Collision suffix
    Nothing ->
        case env ^. p2NamesAbove . Lens.ix name <> Clash.isClashOf inst of
        NoClash{} -> NoCollision
        Clash ->
            -- Once a collision, other non-colliding instances
            -- also get a suffix, so we have no idea what suffix
            -- we'll get:
            UnknownCollision

getCollisionEnv :: Text -> Clash.AnnotatedName -> P2Env -> (Collision, P2Env)
getCollisionEnv name inst env =
    ( getCollision name inst env
    , env & p2NamesAbove %~ Map.insertWith mappend name (Clash.isClashOf inst)
    )

instance Monad tm => MonadNaming (Pass2MakeNames tm) where
    type OldName (Pass2MakeNames tm) = P1Name
    type NewName (Pass2MakeNames tm) = Name (T tm)
    type SM (Pass2MakeNames tm) = tm
    opRun = p2GetEnv <&> runPass2MakeNames <&> (pure .)
    opWithParamName GetParameter varInfo = p2cpsNameConvertorLocal varInfo
    opWithParamName GetFieldParameter _ = p2cpsNameConvertorGlobal
    opWithLetName = p2cpsNameConvertorLocal
    opGetName _ nameType =
        case nameType of
        Walk.ParamName -> p2nameConvertorLocal
        _ -> p2nameConvertorGlobal

p2nameConvertorLocal :: Monad m => P1Name -> Pass2MakeNames tm (Name (T m))
p2nameConvertorLocal (P1Name mStoredName inst _) =
    case mStoredName of
        Just storedName ->
            do
                env <- p2GetEnv
                Stored storedName (getCollision storedName inst env) & pure
        Nothing ->
            do
                nameGen <- p2GetEnv <&> (^. p2NameGen)
                let name = evalState (NameGen.existingName uuid) nameGen
                AutoGenerated name & pure
    <&> (`Name` setUuidName uuid)
    where
        uuid = inst ^. Clash.niUUID

p2cpsNameConvertor ::
    Monad tm =>
    P1Name ->
    (P2Env -> (Form, P2Env)) ->
    CPS (Pass2MakeNames tm) (Name (T tm))
p2cpsNameConvertor (P1Name mStoredName inst _) nameMaker =
    CPS $ \k ->
    do
        oldEnv <- p2GetEnv
        let (newName, newEnv) =
                case mStoredName of
                Just storedName ->
                    getCollisionEnv storedName inst oldEnv
                    & _1 %~ Stored storedName
                Nothing -> nameMaker oldEnv
                & _1 %~ (`Name` setUuidName (inst ^. Clash.niUUID))
        res <- p2WithEnv (const newEnv) k
        pure (newName, res)

p2cpsNameConvertorGlobal :: Monad tm => Walk.CPSNameConvertor (Pass2MakeNames tm)
p2cpsNameConvertorGlobal p1name =
    p2cpsNameConvertor p1name $
    \env ->
    getCollisionEnv unnamedStr (p1Instance p1name) env
    & _1 %~ Unnamed

p2cpsNameConvertorLocal ::
    Monad tm => NameGen.VarInfo -> Walk.CPSNameConvertor (Pass2MakeNames tm)
p2cpsNameConvertorLocal isFunction p1name =
    p2cpsNameConvertor p1name $ \p2env ->
    let accept name =
            Lens.hasn't (nameUUIDMap . Lens.ix name) (localNames namesWithin)
            && Lens.hasn't (p2NamesAbove . Lens.ix name) p2env
    in  NameGen.newName accept isFunction (inst ^. Clash.niUUID)
        <&> AutoGenerated
        & Lens.zoom p2NameGen
        & (`runState` p2env)
    where
        P1Name _ inst namesWithin = p1name

p2nameConvertorGlobal :: Monad tm => Walk.NameConvertor (Pass2MakeNames tm)
p2nameConvertorGlobal (P1Name mStoredName inst _) =
    p2GetEnv
    <&> getCollision (fromMaybe unnamedStr mStoredName) inst
    <&> mk
    <&> (`Name` setUuidName (inst ^. Clash.niUUID))
    where
        mk = maybe Unnamed Stored mStoredName

fixVarToTags :: Monad m => VarToTags -> T m ()
fixVarToTags VarToTags {..} =
    Transaction.getP (assocNameRef vttReplacedVar)
    >>= Transaction.setP (assocNameRef (vttReplacedByTag ^. tagVal))

fixParamAddResult :: Monad m => ParamAddResult -> T m ()
fixParamAddResult (ParamAddResultVarToTags v) = fixVarToTags v
fixParamAddResult _ = pure ()

fixParamDelResult :: Monad m => ParamDelResult -> T m ()
fixParamDelResult (ParamDelResultTagsToVar TagsToVar {..}) =
    Transaction.getP (assocNameRef (ttvReplacedTag ^. tagVal))
    >>= Transaction.setP (assocNameRef ttvReplacedByVar)
fixParamDelResult _ = pure ()

fixExtractFloatResult :: Monad m => ExtractFloatResult -> T m ()
fixExtractFloatResult = traverse_ fixVarToTags . efrMVarToTags

postProcessAction :: Monad m => (a -> m ()) -> m a -> m a
postProcessAction f action =
    do
        res <- action
        f res
        pure res

fixNodeActions :: Monad m => NodeActions (T m) -> NodeActions (T m)
fixNodeActions =
    extract %~ postProcessAction fixExtractFloatResult

-- mutual recursion fixBinder<->fixExpr

fixBinder ::
    Monad m =>
    Binder name (T m) (Expression name (T m) a) ->
    Binder name (T m) (Expression name (T m) a)
fixBinder binder =
    binder
    & SugarLens.binderFuncParamAdds %~ postProcessAction fixParamAddResult
    & SugarLens.binderFuncParamDeletes %~ postProcessAction fixParamDelResult
    & bBody . bbContent %~ fixBinderContent
    where
        fixBinderContent x =
            x
            & _BinderExpr %~ fixExpr
            & _BinderLet . lValue %~ fixBinder
            & _BinderLet . lBody . bbContent %~ fixBinderContent
            & _BinderLet . lActions . laNodeActions %~ fixNodeActions

fixExpr :: Monad m => Expression name (T m) a -> Expression name (T m) a
fixExpr expr =
    expr & rBody %~ \case
    BodyLam lam -> lam & lamBinder %~ fixBinder & BodyLam
    body -> body <&> fixExpr

runPasses ::
    Functor tm =>
    (a -> Pass0LoadNames tm b) -> (b -> Pass1PropagateUp tm c) -> (c -> Pass2MakeNames tm d) ->
    a -> T tm d
runPasses f0 f1 f2 =
    fmap (pass2 . pass1) . pass0
    where
        pass0 = runPass0LoadNames . f0
        pass1 = runPass1PropagateUp . f1
        pass2 (x, p1out) =
            f2 x & runPass2MakeNamesInitial p1out

fixDef ::
    Monad tm =>
    Definition name (T tm) (Expression name (T tm) a) ->
    Definition name (T tm) (Expression name (T tm) a)
fixDef = drBody . _DefinitionBodyExpression . deContent %~ fixBinder

addToWorkArea ::
    Monad tm =>
    WorkArea InternalName (T tm) a -> T tm (WorkArea (Name (T tm)) (T tm) a)
addToWorkArea workArea =
    workArea
    & waPanes . traverse . paneDefinition %~ fixDef
    & waRepl %~ fixExpr
    & runPasses f f f
    where
        f = Walk.toWorkArea
