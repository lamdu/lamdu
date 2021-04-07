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
    sha256 = "15wvz6yzzkn7yhnqin493pggxrzjsz23wsnkq49kz3rfcrpmjgxk";
    rev = "2df16cf5f3be3552a28c0a42f9c82e8b58a0a93e";
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
