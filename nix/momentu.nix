{ mkDerivation, fetchFromGitHub, stdenv
, GLFW-b, HUnit, OpenGL, QuickCheck, aeson, base, base-compat, binary, bytestring
, containers, deepseq, generic-data, generic-random, graphics-drawingcombinators
, lens, mtl, safe-exceptions, stm, template-haskell
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, time, timeit, unicode-properties
}:
mkDerivation {
  pname = "momentu";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "momentu";
    sha256 = "0441yldjjlp34pywfhf9h97ysim3i4vkic864n81pga3wajbxzg2";
    rev = "f3b2140103f7a3651ad145eb90a9300b7b27c52a";
  };
  libraryHaskellDepends = [
    GLFW-b HUnit OpenGL QuickCheck aeson base base-compat binary bytestring
    containers deepseq generic-data generic-random graphics-drawingcombinators lens
    mtl safe-exceptions stm template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 text time timeit unicode-properties
  ];
  homepage = "https://github.com/lamdu/momentu.git#readme";
  description = "The Momentu purely functional animated GUI framework";
  license = stdenv.lib.licenses.bsd3;
}
