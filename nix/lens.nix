{ mkDerivation, array, base, base-orphans, bifunctors, bytestring
, Cabal, cabal-doctest, call-stack, comonad, containers
, contravariant, criterion, deepseq, directory, distributive
, doctest, exceptions, filepath, free, generic-deriving, ghc-prim
, hashable, HUnit, kan-extensions, mtl, nats, parallel, profunctors
, QuickCheck, reflection, semigroupoids, semigroups, simple-reflect
, stdenv, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, text, th-abstraction, transformers
, transformers-compat, type-equality, unordered-containers, vector
}:
mkDerivation {
  pname = "lens";
  version = "4.18.1";
  sha256 = "3107a3d5165a9762269d7c9e39205d2c4c2aaebc1900ff44a0066c31a492bdd2";
  setupHaskellDepends = [ base Cabal cabal-doctest filepath ];
  libraryHaskellDepends = [
    array base base-orphans bifunctors bytestring call-stack comonad
    containers contravariant distributive exceptions filepath free
    ghc-prim hashable kan-extensions mtl parallel profunctors
    reflection semigroupoids tagged template-haskell text
    th-abstraction transformers transformers-compat type-equality
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring containers deepseq directory doctest filepath
    generic-deriving HUnit mtl nats parallel QuickCheck semigroups
    simple-reflect test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th text transformers
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring comonad containers criterion deepseq
    generic-deriving transformers unordered-containers vector
  ];
  homepage = "http://github.com/ekmett/lens/";
  description = "Lenses, Folds and Traversals";
  license = stdenv.lib.licenses.bsd2;
}
