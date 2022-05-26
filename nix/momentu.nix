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
    sha256 = "15vnngi48mvz5qdy5wxfb4xd2av6gwmnqmhga92y43wl1cdl9w4y";
    rev = "a83a5978798c355d510d2e1fa7e2d8c73ba81078";
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
