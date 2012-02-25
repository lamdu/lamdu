{-# LANGUAGE CPP #-}
module Data.AtFieldTH (make) where

import qualified Data.Char as Char
import qualified Data.List as List
import Language.Haskell.TH.Syntax

make :: Name -> Q [Dec]
make typeName = do
    TyConI typeCons <- reify typeName
    (makeAtName, typeVars, ctor) <-
        case typeCons of
            DataD _ _ typeVars constructors _ ->
                case constructors of
                    [ctor] -> return (makeAtNameForDataField, typeVars, ctor)
                    _ -> fail "one constructor expected for Data.AtFieldTH.make"
            NewtypeD _ _ typeVars ctor _ -> return (makeAtNameForNewtype typeName, typeVars, ctor)
            _ -> error $ show typeCons ++ " is not supported!"
    return $ constructorAtFuncs makeAtName typeName typeVars ctor

constructorAtFuncs :: (Name -> Name) -> Name -> [TyVarBndr] -> Con -> [Dec]
constructorAtFuncs makeAtName typeName typeVars constructor =
    concatMap (uncurry (fieldAtFunc makeAtName typeName typeVars isFreeVar)) fields
    where
        fields = constructorFields constructor
        constructorVars = concatMap (List.nub . varsOfType . snd) fields
        isFreeVar v = 1 == countElemRepetitions v constructorVars

constructorFields :: Con -> [(Name, Type)]
constructorFields (NormalC name [(_, t)]) = [(name, t)]
constructorFields (RecC _ fields) =
    map f fields
    where
        f (name, _, t) = (name, t)
constructorFields _ = error "unsupported constructor for type supplied to Data.AtFieldTH.make"

countElemRepetitions :: Eq a => a -> [a] -> Int
countElemRepetitions x = length . filter (== x)

-- When field name is *Typename, such as unTypename, or getTypename,
-- As is common convention for 'conceptual' newtype wrappers, we name the accessor atTypename.
-- Otherwise we name it as we do normally with Data constructors.
makeAtNameForNewtype :: Name -> Name -> Name
makeAtNameForNewtype newTypeName fieldName
    | typeNameStr `List.isSuffixOf` nameBase fieldName = mkName $ "at" ++ typeNameStr
    | otherwise = makeAtNameForDataField fieldName
    where
        typeNameStr = nameBase newTypeName

makeAtNameForDataField :: Name -> Name
makeAtNameForDataField fieldName =
    mkName $ "at" ++ (Char.toUpper fieldNameHead : fieldNameTail)
    where
        fieldNameHead : fieldNameTail = nameBase fieldName

fieldAtFunc :: (Name -> Name) -> Name -> [TyVarBndr] -> (Name -> Bool) -> Name -> Type -> [Dec]
fieldAtFunc makeAtName typeName typeVars isFreeVar fieldName fieldType =
    [ SigD resName . ForallT resultTypeVars [] $ foldr1 arrow
        [ arrow (sideType fieldType "Src") (sideType fieldType "Dst")
        , input
        , output
        ]
    , FunD resName [ Clause [VarP funcName, VarP valName] clause [] ]
    ]
    where
        arrow = AppT . AppT ArrowT
        funcName = mkName "func"
        valName = mkName "val"
        resName = makeAtName fieldName
        clause = NormalB $ RecUpdE (VarE valName) [(fieldName, applyExp)]
        applyExp = AppE (VarE funcName) . AppE (VarE fieldName) $ VarE valName
        valType = foldl AppT (ConT typeName) $ map (VarT . tyVarBndrName) typeVars
        sideType t suffix = mapTypeVarNames (sideTypeVar suffix) t
        fieldVars = List.nub (varsOfType fieldType)
        sideTypeVar suffix name =
            if isFreeVar name && elem name fieldVars
            then mkName (nameBase name ++ suffix) else name
        input = sideType valType "Src"
        output = sideType valType "Dst"
        resultTypeVars = map PlainTV . List.nub $ concatMap varsOfType [input, output]

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name

-- TODO [BP]: can boilerplate be reduced with SYB/uniplate ?
varsOfType :: Type -> [Name]
varsOfType (ForallT _ _ t) = varsOfType t -- TODO: dwa need todo something wrt the predicates?
varsOfType (VarT x) = [x]
varsOfType (ConT _) = []
varsOfType (TupleT _) = []
#if __GLASGOW_HASKELL__ >= 704
varsOfType (UnboxedTupleT _) = []
#endif
varsOfType ArrowT = []
varsOfType ListT = []
varsOfType (AppT x y) = varsOfType x ++ varsOfType y
varsOfType (SigT x _) = varsOfType x

-- TODO BP
mapTypeVarNames :: (Name -> Name) -> Type -> Type
mapTypeVarNames func (ForallT vars cxt t) =
    ForallT vars (map (mapPredVarNames func) cxt) (mapTypeVarNames func t)
mapTypeVarNames func (VarT x) = VarT (func x)
mapTypeVarNames _ (ConT x) = ConT x
mapTypeVarNames _ (TupleT x) = TupleT x
#if __GLASGOW_HASKELL__ >= 704
mapTypeVarNames _ (UnboxedTupleT x) = UnboxedTupleT x
#endif
mapTypeVarNames _ ArrowT = ArrowT
mapTypeVarNames _ ListT = ListT
mapTypeVarNames func (AppT x y) = AppT (mapTypeVarNames func x) (mapTypeVarNames func y)
mapTypeVarNames func (SigT t kind) = SigT (mapTypeVarNames func t) kind

-- TODO BP
mapPredVarNames :: (Name -> Name) -> Pred -> Pred
mapPredVarNames func (ClassP name types) = ClassP name $ map (mapTypeVarNames func) types
mapPredVarNames func (EqualP x y) = EqualP (mapTypeVarNames func x) (mapTypeVarNames func y)
