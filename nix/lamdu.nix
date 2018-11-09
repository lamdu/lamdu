{ mkDerivation, aeson, aeson-diff, aeson-pretty, AlgoW, base, base-compat
, base-orphans, base16-bytestring, binary, bytestring, Cabal, containers
, cryptohash-sha256, data-default, deepseq, deepseq-generics
, directory, edit-distance, ekg-core, executable-path, filepath
, generic-random, GLFW-b, graphics-drawingcombinators, hashable, lamdu-calculus
, language-ecmascript, lens, lens-aeson, leveldb-haskell, List, mtl, nodejs-exec, OpenGL
, optparse-applicative, pretty, process, random, QuickCheck, safe-exceptions
, split, StateVar, stdenv, stm, temporary, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, time, timeit, transformers, unordered-containers
, uuid, uuid-types, vector, wl-pprint, yaml, zip-archive, HUnit
}:
mkDerivation {
  pname = "Lamdu";
  version = "0.1";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson aeson-diff aeson-pretty AlgoW base base-compat base-orphans
    base16-bytestring binary bytestring Cabal containers cryptohash-sha256
    data-default deepseq deepseq-generics directory edit-distance ekg-core executable-path filepath
    GLFW-b generic-random graphics-drawingcombinators hashable HUnit lamdu-calculus
    language-ecmascript lens lens-aeson leveldb-haskell List mtl nodejs-exec
    OpenGL optparse-applicative pretty process random QuickCheck safe-exceptions
    split StateVar stm temporary template-haskell test-framework test-framework-hunit test-framework-quickcheck2
    text time timeit transformers unordered-containers uuid uuid-types vector wl-pprint
    yaml zip-archive
  ];
  homepage = "http://www.lamdu.org";
  description = "A next generation IDE";
  license = "GPL";
}
