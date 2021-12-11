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
    sha256 = "04hn7zlblxg7bndyc7dlvn0gjpqfvm3vqs1jjk6lyxz81ymfpyjd";
    rev = "536582e9c792833de69b8aa4c118305b3a02414b";
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
