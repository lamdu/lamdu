{ mkDerivation, fetchFromGitHub, lib
, GLFW-b, HUnit, OpenGL, QuickCheck, aeson, base, base-compat, binary, bytestring
, containers, deepseq, generic-data, generic-random, graphics-drawingcombinators
, lens, mtl, safe-exceptions, stm, template-haskell
, tasty, tasty-hunit, tasty-quickcheck
, text, time, timeit, unicode-properties
}:
mkDerivation {
  pname = "momentu";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "momentu";
    sha256 = "1qw24mgkpf5g80j7vbvcfwyxpxz5877yanac9zldybz4b3bqajqp";
    rev = "16835f282d383dfb97893cf82054b3165770255d";
  };
  libraryHaskellDepends = [
    GLFW-b HUnit OpenGL QuickCheck aeson base base-compat binary bytestring
    containers deepseq generic-data generic-random graphics-drawingcombinators lens
    mtl safe-exceptions stm template-haskell text time timeit unicode-properties
    tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/lamdu/momentu.git#readme";
  description = "The Momentu purely functional animated GUI framework";
  license = lib.licenses.bsd3;
}
