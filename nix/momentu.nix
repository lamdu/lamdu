{ mkDerivation, fetchFromGitHub, lib
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
    sha256 = "1lrshy69fqq694cnili6ia0s7yl130j8x71kmq8vv77cidnrbka1";
    rev = "b54f426e021605594b824acb97b6f08e41b89c78";
  };
  libraryHaskellDepends = [
    GLFW-b HUnit OpenGL QuickCheck aeson base base-compat binary bytestring
    containers deepseq generic-data generic-random graphics-drawingcombinators lens
    mtl safe-exceptions stm template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 text time timeit unicode-properties
  ];
  homepage = "https://github.com/lamdu/momentu.git#readme";
  description = "The Momentu purely functional animated GUI framework";
  license = lib.licenses.bsd3;
}
