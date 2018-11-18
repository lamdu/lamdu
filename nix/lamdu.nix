{ mkDerivation, aeson, aeson-diff, aeson-pretty, AlgoW, base
, base-compat, base-orphans, base16-bytestring, binary, bytestring
, Cabal, containers, cryptohash-sha256, data-default, deepseq
, deepseq-generics, directory, edit-distance, ekg-core
, executable-path, filepath, generic-random, GLFW-b
, graphics-drawingcombinators, hashable, HUnit, lamdu-calculus
, language-ecmascript, lens, lens-aeson, leveldb-haskell, List, mtl
, nodejs-exec, OpenGL, optparse-applicative, pretty, process
, QuickCheck, random, safe-exceptions, split, StateVar, stdenv, stm
, template-haskell, temporary, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, time, timeit, transformers
, unordered-containers, uuid, uuid-types, vector, wl-pprint, yaml
, zip-archive, lib, gitMinimal
}:
mkDerivation {
  pname = "Lamdu";
  version = "0.1";
  src =
      let src = ../.;
          myFilter =
              path: type:
              let relPath = lib.removePrefix (toString src + "/") (toString path);
              in
              let res =
              !lib.any (prefix: lib.hasPrefix prefix relPath)
              [ "dist" # cabal
                "dist-newstyle" # cabal-new
                ".stack-work" # stack builds
                "ghci-out" # ghci loads
                "git-cache" # git caches
                "result" # nix output
              ];
              in res;
      in lib.cleanSource (lib.cleanSourceWith { filter = myFilter; inherit src; });
  configureFlags = [ "-f-ekg" ];
  isLibrary = true;
  isExecutable = true;
  doHaddock = false;
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty AlgoW base base-compat base-orphans
    base16-bytestring binary bytestring containers cryptohash-sha256
    data-default deepseq deepseq-generics directory edit-distance
    ekg-core executable-path filepath GLFW-b
    graphics-drawingcombinators hashable lamdu-calculus
    language-ecmascript lens lens-aeson leveldb-haskell List mtl
    nodejs-exec OpenGL optparse-applicative pretty process random
    safe-exceptions split StateVar stm temporary text time timeit
    transformers unordered-containers uuid uuid-types vector wl-pprint
    zip-archive
  ];
  buildDepends = [ gitMinimal ];
  executableHaskellDepends = [
    base base-compat directory process template-haskell time
  ];
  testHaskellDepends = [
    aeson aeson-diff aeson-pretty AlgoW base bytestring Cabal
    containers deepseq deepseq-generics directory filepath
    generic-random GLFW-b HUnit lamdu-calculus lens lens-aeson List mtl
    nodejs-exec pretty process QuickCheck random split test-framework
    test-framework-hunit test-framework-quickcheck2 text uuid-types
    yaml
  ];
  homepage = "http://www.lamdu.org";
  description = "A next generation IDE";
  license = "GPL";
}
