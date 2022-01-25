{-# LANGUAGE TemplateHaskell, TypeApplications, FlexibleInstances, DefaultSignatures, ScopedTypeVariables, DerivingVia #-}

module Lamdu.Sugar.Convert.LightLam
    ( AddLightLams, addLightLambdas
    ) where

import qualified Control.Lens as Lens
import           Data.Monoid (All(..))
import qualified Data.Set as Set
import           Hyper
import qualified Lamdu.Sugar.Config as Config
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Props as Props
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data SubTreeInfo = SubTreeInfo
    { _canUseLightLam :: All
    , _usedVars :: Set InternalName
    } deriving stock (Generic, Show)
    deriving (Semigroup, Monoid) via (Generically SubTreeInfo)
Lens.makeLenses ''SubTreeInfo

addLightLambdas :: _ => m (Ann a # t -> Ann a # t)
addLightLambdas =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.lightLambda) <&>
    \case
    False -> id
    True -> (^. _2) . addLightLambdasH

addLightLambdasH :: forall t a. Recursively AddLightLams t => Ann a # t -> (Const SubTreeInfo :*: Ann a) # t
addLightLambdasH (Ann a b) =
    withDict (recursively (Proxy @(AddLightLams t))) $
    let sub = hmap (Proxy @(Recursively AddLightLams) #> addLightLambdasH) b
        subBody = hmap (const (^. _2)) sub
        subInfo = hfoldMap (const (^. _1 . Lens._Wrapped)) sub
    in
    if subInfo ^. canUseLightLam . Lens._Wrapped
    then
        Const (subInfo <> SubTreeInfo (All (not (Props.isForbiddenInLightLam b))) (foldMap Set.singleton (getVars b)))
        :*: Ann a (lighten (subInfo ^. usedVars) subBody)
    else Const (SubTreeInfo (All False) mempty) :*: Ann a subBody

class Props.SugarExpr t => AddLightLams t where
    markLightParams :: Set InternalName -> t # Ann a -> t # Ann a

    lighten :: Set InternalName -> t # Ann a -> t # Ann a
    lighten _ = id

    getVars :: t f -> Maybe InternalName
    getVars _ = Nothing

    default markLightParams :: HNodesConstraint t AddLightLams => Set InternalName -> t # Ann a -> t # Ann a
    markLightParams = defaultMarkLightParams

defaultMarkLightParams ::
    (HFunctor t, HNodesConstraint t AddLightLams) => Set InternalName -> t # Ann a -> t # Ann a
defaultMarkLightParams paramNames = hmap (Proxy @AddLightLams #> markNodeAddLightLams paramNames)

markNodeAddLightLams :: AddLightLams t => Set InternalName -> Ann a # t -> Ann a # t
markNodeAddLightLams paramNames = hVal %~ markLightParams paramNames

instance AddLightLams (Const (i (TagChoice InternalName o)))
instance AddLightLams (Const (TagRef InternalName i o))
instance AddLightLams (Else v InternalName i o)
instance AddLightLams (Function v InternalName i o)
instance AddLightLams (PostfixFunc v InternalName i o)

instance AddLightLams (Const (GetVar InternalName o)) where
    markLightParams paramNames =
        Lens._Wrapped . Lens.filteredBy (vNameRef . nrName . Lens.filtered f) . vForm .~ GetLightParam
        where
            f n = paramNames ^. Lens.contains n
    getVars = (^? Lens._Wrapped . vNameRef . nrName)

instance AddLightLams (Assignment v InternalName i o) where
    markLightParams ps (BodyPlain x) = x & apBody %~ markLightParams ps & BodyPlain
    markLightParams ps (BodyFunction x) = markLightParams ps x & BodyFunction
    lighten used = _BodyPlain . apBody %~ lighten used
    getVars x = x ^? _BodyPlain . apBody >>= getVars

instance AddLightLams (Binder v InternalName i o) where
    markLightParams ps = bBody %~ markLightParams ps
    lighten used = bBody . _BinderTerm %~ lighten used
    getVars = getVars . (^. bBody)

instance AddLightLams (BinderBody v InternalName i o) where
    markLightParams ps (BinderTerm x) = markLightParams ps x & BinderTerm
    markLightParams ps x = defaultMarkLightParams ps x
    getVars x = x ^? _BinderTerm >>= getVars

instance AddLightLams (Term v InternalName i o) where
    markLightParams paramNames (BodyLeaf (LeafGetVar x)) =
        markLightParams paramNames (Const x) ^. Lens._Wrapped & LeafGetVar & BodyLeaf
    markLightParams paramNames bod = defaultMarkLightParams paramNames bod
    getVars x = x ^? _BodyLeaf . _LeafGetVar <&> Const >>= getVars
    lighten used =
        _BodyLam . Lens.filtered (Lens.nullOf (lamFunc . fParams . _ParamVar . vIsNullParam . Lens.only True)) %~
        \lam ->
        let funcParams =
                lam ^.. lamFunc . fParams .
                ( _ParamsRecord . SugarLens.taggedListItems . tiTag
                    <> _ParamVar . Lens.filteredBy (vIsNullParam . Lens.only False) . vTag . oTag
                ) . tagRefTag . tagName
                & Set.fromList
        in
        if Lens.allOf Lens.folded ((used ^.) . Lens.contains) funcParams
        then lam & lamLightweight .~ True & lamFunc %~ markLightParams funcParams
        else lam
