{ mkDerivation, aeson, aeson-diff, aeson-pretty, AlgoW, base, base-compat
, base-orphans, base16-bytestring, binary, bytestring, containers
, cryptohash-sha256, data-default, deepseq, deepseq-generics
, directory, edit-distance, ekg-core, filepath, GLFW-b, graphics-drawingcombinators
, hashable, JuicyPixels, lamdu-calculus, language-ecmascript, lens, lens-aeson
, leveldb-haskell, List, mtl, nodejs-exec, OpenGL
, optparse-applicative, pretty, process, random, QuickCheck, safe-exceptions
, split, StateVar, stdenv, stm, temporary, test-framework, test-framework-hunit
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
    base16-bytestring binary bytestring containers cryptohash-sha256
    data-default deepseq deepseq-generics directory edit-distance ekg-core filepath GLFW-b
    graphics-drawingcombinators hashable HUnit JuicyPixels lamdu-calculus
    language-ecmascript lens lens-aeson leveldb-haskell List mtl nodejs-exec
    OpenGL optparse-applicative pretty process random QuickCheck safe-exceptions
    split StateVar stm temporary test-framework test-framework-hunit test-framework-quickcheck2
    text time timeit transformers unordered-containers uuid uuid-types vector wl-pprint
    yaml zip-archive
  ];
  homepage = "http://www.lamdu.org";
  description = "A next generation IDE";
  license = "GPL";
}
