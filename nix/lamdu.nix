{ mkDerivation, aeson, aeson-pretty, AlgoW, base, base-compat
, base-orphans, base16-bytestring, binary, bytestring, containers
, cryptohash-sha256, data-default, deepseq, deepseq-generics
, directory, filepath, GLFW-b, graphics-drawingcombinators
, hashable, JuicyPixels, lamdu-calculus, language-ecmascript, lens
, leveldb-haskell, List, mtl, nodejs-exec, OpenGL
, optparse-applicative, pretty, process, random, safe-exceptions
, StateVar, stdenv, stm, text, time, transformers
, unordered-containers, uuid, uuid-types, vector, wl-pprint
, zip-archive, imagemagick
}:
mkDerivation {
  pname = "Lamdu";
  version = "0.1";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson aeson-pretty AlgoW base base-compat base-orphans
    base16-bytestring binary bytestring containers cryptohash-sha256
    data-default deepseq deepseq-generics directory filepath GLFW-b
    graphics-drawingcombinators hashable JuicyPixels lamdu-calculus
    language-ecmascript lens leveldb-haskell List mtl nodejs-exec
    OpenGL optparse-applicative pretty process random safe-exceptions
    StateVar stm text time transformers unordered-containers uuid
    uuid-types vector wl-pprint zip-archive
  ];
  executablePkgconfigDepends = [ imagemagick ];
  homepage = "http://www.lamdu.org";
  description = "A next generation IDE";
  license = "GPL";
}
