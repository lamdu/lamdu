{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.ExampleDB
    ( initDB, createPublics
    , withDB
    ) where

import           Control.Lens.Operators
import           Control.Monad (unless, void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as Writer
import           Control.MonadA (MonadA)
import           Data.Foldable (traverse_)
import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
import           Data.Store.Db (Db)
import qualified Data.Store.Db as Db
import           Data.Store.Rev.Branch (Branch)
import qualified Data.Store.Rev.Branch as Branch
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import           Data.Store.Transaction (Transaction, setP)
import qualified Data.Store.Transaction as Transaction
import           Data.String (IsString(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Data.Anchors (assocTagOrder, ParamList, assocFieldParamList, PresentationMode(..))
import qualified Lamdu.Data.DbLayout as Db
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Nominal (Nominal(..))
import           Lamdu.Expr.Pure (($$))
import qualified Lamdu.Expr.Pure as Pure
import           Lamdu.Expr.Scheme (Scheme(..))
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Type (Type, (~>))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TV
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.GUI.WidgetIdIRef as WidgetIdIRef
import qualified System.Directory as Directory
import           System.FilePath ((</>))

type T = Transaction

setName :: (MonadA m, UniqueId.ToGuid a) => a -> String -> T m ()
setName x = setP . Db.assocNameRef $ x

namedId ::
    forall a m. (MonadA m, IsString a, UniqueId.ToGuid a) => String -> T m a
namedId name =
    do
        setName tag name
        return tag
    where
        tag :: a
        tag = fromString name

forAll :: forall a. TV.VarKind a => Int -> ([a] -> Type) -> Scheme
forAll count f =
    Scheme
    { _schemeForAll = mconcat $ map TV.singleton typeVars
    , _schemeConstraints = mempty
    , _schemeType = f $ map TV.lift typeVars
    }
    where
        typeVars :: [T.Var a]
        typeVars = take count $ map (fromString . (:[])) ['a'..'z']

recordType :: [(T.Tag, Type)] -> Type
recordType = T.TRecord . foldr (uncurry T.CExtend) T.CEmpty

sumType :: [(T.Tag, Type)] -> Type
sumType = T.TSum . foldr (uncurry T.CExtend) T.CEmpty

nameTheAnchors :: MonadA m => T m ()
nameTheAnchors = mapM_ (uncurry setName) Builtins.anchorNames

data Public m = Public
    { publicDefs :: [DefI m]
    , publicTags :: [T.Tag]
    , publicTIds :: [T.Id]
    }

instance Monoid (Public m) where
    mempty = Public mempty mempty mempty
    mappend (Public x0 y0 z0) (Public x1 y1 z1) =
        Public (mappend x0 x1) (mappend y0 y1) (mappend z0 z1)

publicize :: (Monad m, Monoid s) => m b -> (b -> s) -> WriterT s m b
publicize act g =
    do
        x <- lift act
        Writer.tell $ g x
        return x

type M m = WriterT (Public m) (T m)

newTag :: MonadA m => Int -> String -> M m T.Tag
newTag order n =
    do
        tag <- publicize (namedId n) $ \x -> mempty { publicTags = [x] }
        lift $ Transaction.setP (assocTagOrder tag) order
        return tag

newTId :: MonadA m => String -> M m T.Id
newTId n = publicize (namedId n) $ \x -> mempty { publicTIds = [x] }

newPublicDef ::
    Monad m => m (DefI n) -> WriterT (Public n) m (DefI n)
newPublicDef act = publicize act $ \x -> mempty { publicDefs = [x] }

type TypeCtor = [Type] -> Type

newNominal ::
    MonadA m => String -> [(T.ParamId, T.TypeVar)] ->
    ({-fixpoint:-}TypeCtor -> Scheme) ->
    M m (T.Id, TypeCtor)
newNominal name params body =
    do
        tid <- newTId name
        let tinst typeParams =
                T.TInst tid $ Map.fromList $ zip (map fst params) typeParams
        let scheme = body tinst
        lift $ Transaction.writeIRef (ExprIRef.nominalI tid) $
            Nominal (Map.fromList params) scheme
        return (tid, tinst)

newPublicDefVal ::
    MonadA m => String -> PresentationMode -> ValI m -> Scheme ->
    WriterT (Public m) (Transaction m) (DefI m)
newPublicDefVal name presentationMode valI typ =
    newPublicDef $
    DataOps.newDefinition name presentationMode .
    Definition.BodyExpr $ Definition.Expr valI $
    Definition.ExportedType $ typ

newPublicDefExpr ::
    MonadA m =>
    String -> PresentationMode -> Val () ->
    Scheme -> M m (DefI m)
newPublicDefExpr name presentationMode expr typ =
    do
        valI <- lift $ ExprIRef.newVal expr
        newPublicDefVal name presentationMode valI typ

data CtorInfo = CtorInfo
    { _ctorName :: String
    , ctorTag :: T.Tag
    , _ctorPresentationMode :: PresentationMode
    , _ctorScheme :: Scheme
    }

createInjectorI ::
    MonadA m =>
    ((Val () -> Val ()) -> Val ()) -> T.Id -> CtorInfo ->
    M m (DefI m)
createInjectorI f typeName (CtorInfo name tag presentationMode scheme) =
    newPublicDefExpr name presentationMode
    (f (Pure.toNom typeName . Pure.inject tag))
    scheme

createInjector ::
    MonadA m => T.Id -> CtorInfo -> M m (DefI m)
createInjector = createInjectorI (Pure.lambda "x")

createNullaryInjector ::
    MonadA m => T.Id -> CtorInfo -> M m (DefI m)
createNullaryInjector = createInjectorI ($ Pure.recEmpty)

data ListNames m = ListNames
    { _lnTid :: T.Id
    , lnNil :: DefI m
    , lnCons :: DefI m
    }

data Ctor
    = Nullary CtorInfo
    | Normal Type CtorInfo

adt ::
    MonadA m => String -> [(T.ParamId, T.TypeVar)] -> (TypeCtor -> [Ctor]) ->
    M m (TypeCtor, T.Id, [DefI m])
adt name params ctors =
    do
        (tid, t) <-
            newNominal name params $ \t ->
            ctors t
            <&> onCtor
            & sumType
            & Scheme.mono
        let mkInjectorFor (Nullary info) = createNullaryInjector tid info
            mkInjectorFor (Normal _ info) = createInjector tid info
        injectors <- mapM mkInjectorFor $ ctors t
        return (t, tid, injectors)
    where
        onCtor (Nullary info) = (ctorTag info, recordType [])
        onCtor (Normal typ info) = (ctorTag info, typ)

createList :: MonadA m => T.ParamId -> M m (TypeCtor, ListNames m)
createList valTParamId =
    do
        (list, tid, [nil, cons]) <-
            adt "List" [(valTParamId, valT)] $ \list ->
            [ Nullary $ CtorInfo "[]" Builtins.nilTag Verbose $ forAll 1 $ \[a] -> list [a]
            , let consType =
                      recordType
                      [ (Builtins.headTag, T.TVar valT)
                      , (Builtins.tailTag, list [T.TVar valT])
                      ]
              in  Normal consType $
                  CtorInfo ":" Builtins.consTag Infix $
                  forAll 1 $ \ [a] -> consType ~> list [a]
            ]

        (list, ListNames tid nil cons) & return
    where
        valT = "a"

createMaybe ::
    MonadA m => T.ParamId -> M m (TypeCtor, T.Id, [DefI m])
createMaybe valTParamId =
    do
        let valT = "a"
        adt "Maybe" [(valTParamId, valT)] $ \maybe_ ->
            [ Nullary $ CtorInfo "Nothing" Builtins.nothingTag Verbose $
              forAll 1 $ \[a] -> maybe_ [a]
            , Normal (T.TVar valT) $ CtorInfo "Just" Builtins.justTag Verbose $
              forAll 1 $ \[a] -> a ~> maybe_ [a]
            ]

data BoolNames m = BoolNames
    { bnTid :: T.Id
    , bnTrue :: DefI m
    , bnFalse :: DefI m
    }

createBool :: MonadA m => M m (Type, BoolNames m)
createBool =
    do
        (tyCon, tid, [injectTrue, injectFalse]) <-
            adt "Bool" [] $ \boolTCons ->
            [ Nullary $ CtorInfo "True" Builtins.trueTag Verbose $
              Scheme.mono $ boolTCons []
            , Nullary $ CtorInfo "False" Builtins.falseTag Verbose $
              Scheme.mono $ boolTCons []
            ]
        return
            ( tyCon []
            , BoolNames
              { bnTid = tid
              , bnTrue = injectTrue
              , bnFalse = injectFalse
              }
            )

caseBool :: BoolNames m -> V.Var -> V.Var -> Val () -> Val () -> Val () -> Val ()
caseBool boolNames v1 v2 cond then_ else_ =
    cases $$ Pure.fromNom (bnTid boolNames) cond
    where
        cases =
            Pure._case Builtins.trueTag (Pure.abs v1 then_) $
            Pure._case Builtins.falseTag (Pure.abs v2 else_) $
            Pure.absurd

setParamList :: MonadA m => ValI m -> ParamList -> M m ()
setParamList lambdaI tags =
    lift $ Transaction.setP (assocFieldParamList lambdaI) $ Just tags

newPublicFunc ::
    MonadA m => String -> PresentationMode -> [T.Tag] ->
    ([Val ()] -> Val ()) -> Scheme -> M m (DefI m)
newPublicFunc name presentationMode tags mkBody scheme =
    do
        v <- lift ExprIRef.newVar
        lambdaI <-
            lift $ ExprIRef.newVal $ Pure.lambdaRecord v tags mkBody
        setParamList lambdaI tags
        newPublicDefVal name presentationMode lambdaI scheme

createIf :: MonadA m => Type -> BoolNames m -> M m (DefI m)
createIf bool boolNames =
    do
        condTag <- newTag 0 "condition"
        thenTag <- newTag 1 "then"
        elseTag <- newTag 2 "else"
        v1 <- lift ExprIRef.newVar
        v2 <- lift ExprIRef.newVar
        newPublicFunc "if" OO
            [condTag, thenTag, elseTag]
            (\[cond, then_ , else_] -> caseBool boolNames v1 v2 cond then_ else_) $
            forAll 1 $ \[a] -> recordType
            [ (condTag, bool)
            , (thenTag, a)
            , (elseTag, a)
            ] ~> a

getDef :: DefI m -> Val ()
getDef = Pure.global . ExprIRef.globalId

createNot :: MonadA m => Type -> BoolNames m -> M m (DefI m)
createNot bool boolNames@BoolNames{..} =
    do
        v0 <- lift ExprIRef.newVar
        v1 <- lift ExprIRef.newVar
        v2 <- lift ExprIRef.newVar
        newPublicDefExpr "not" Verbose
            ( Pure.lambda v0 $ \b ->
              caseBool boolNames v1 v2 b (getDef bnFalse) (getDef bnTrue) ) $
            Scheme.mono $ bool ~> bool

createPublics :: MonadA m => T m (Db.SpecialFunctions m, Public m)
createPublics =
    Writer.runWriterT $
    do
        lift nameTheAnchors

        valTParamId <- lift $ namedId "val"

        (list, listNames) <- createList valTParamId
        _ <- createMaybe valTParamId
        (bool, boolNames) <- createBool

        _ <- createIf bool boolNames
        _ <- createNot bool boolNames

        let infixType lType rType resType =
                recordType [ (Builtins.infixlTag, lType)
                                      , (Builtins.infixrTag, rType)
                                      ] ~> resType

        traverse_ ((`publicBuiltin_` Scheme.mono (infixType bool bool bool)) . ("Prelude."++))
            ["&&", "||"]

        traverse_
            ((`publicBuiltin_` Scheme.mono (infixType T.TInt T.TInt T.TInt)) .
              ("Prelude." ++))
            ["+", "-", "*", "^"]
        publicDef_ "%" Infix ["Prelude"] "mod" $ Scheme.mono $ infixType T.TInt T.TInt T.TInt
        publicDef_ "//" Infix ["Prelude"] "div" $ Scheme.mono $ infixType T.TInt T.TInt T.TInt
        publicBuiltin_ "Prelude.negate" $ Scheme.mono $ T.TInt ~> T.TInt
        publicBuiltin_ "Prelude.sqrt" $ Scheme.mono $ T.TInt ~> T.TInt

        let aToAToBool = forAll 1 $ \[a] -> infixType a a bool
        traverse_ ((`publicBuiltin_` aToAToBool) . ("Prelude." ++))
            ["==", "/=", "<=", ">=", "<", ">"]

        return
            Db.SpecialFunctions
                { Db.sfNil = lnNil listNames
                , Db.sfCons = lnCons listNames
                }
    where
        publicDef_ name presentationMode ffiPath ffiName typ =
            void $ publicDef name presentationMode ffiPath ffiName typ
        publicDef name presentationMode ffiPath ffiName typ =
            newPublicDef $
            DataOps.newDefinition name presentationMode .
            Definition.BodyBuiltin $ Definition.Builtin (Definition.FFIName ffiPath ffiName) typ
        publicBuiltin fullyQualifiedName =
            publicDef name (DataOps.presentationModeOfName name) path name
            where
                path = init fqPath
                name = last fqPath
                fqPath = splitOn "." fullyQualifiedName
        publicBuiltin_ builtinName typ =
            void $ publicBuiltin builtinName typ

newBranch :: MonadA m => String -> Version m -> T m (Branch m)
newBranch name ver =
    do
        branch <- Branch.new ver
        setName (Branch.guid branch) name
        return branch

initDB :: Db -> IO ()
initDB db =
    Db.runDbTransaction db $
    do
        exists <- Transaction.irefExists $ Db.branches Db.revisionIRefs
        unless exists $
            do
                emptyVersion <- Version.makeInitialVersion []
                master <- newBranch "master" emptyVersion
                view <- View.new master
                let writeRevAnchor f = Transaction.writeIRef (f Db.revisionIRefs)
                writeRevAnchor Db.view view
                writeRevAnchor Db.branches [master]
                writeRevAnchor Db.currentBranch master
                writeRevAnchor Db.redos []
                let paneWId = WidgetIdIRef.fromIRef $ Db.panes Db.codeIRefs
                writeRevAnchor Db.cursor paneWId
                Db.runViewTransaction view $
                    do
                        (specialFunctions, public) <- createPublics
                        let writeCodeAnchor f = Transaction.writeIRef (f Db.codeIRefs)
                        writeCodeAnchor Db.specialFunctions specialFunctions
                        writeCodeAnchor Db.globals (publicDefs public)
                        writeCodeAnchor Db.panes []
                        writeCodeAnchor Db.preJumps []
                        writeCodeAnchor Db.preCursor paneWId
                        writeCodeAnchor Db.postCursor paneWId
                        writeCodeAnchor Db.tids (publicTIds public)
                        writeCodeAnchor Db.tags (publicTags public)
                -- Prevent undo into the invalid empty revision
                newVer <- Branch.curVersion master
                Version.preventUndo newVer

withDB :: FilePath -> (Db -> IO a) -> IO a
withDB lamduDir body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        Db.withDb (lamduDir </> "codeedit.db") $ \db ->
            do
                initDB db
                body db
