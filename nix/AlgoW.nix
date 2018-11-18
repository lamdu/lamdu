{ mkDerivation, base, base-compat, binary, bytestring, containers
, criterion, deepseq, fetchFromGitHub, lamdu-calculus, lens, mtl, pretty
, QuickCheck, stdenv, test-framework, test-framework-quickcheck2
, transformers
}:
mkDerivation {
  pname = "AlgoW";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "Algorithm-W-Step-By-Step";
    sha256 = "0n1gcfzmqjrax5jhwfznksayhd7z8imknx0l0aypqcbxrx4cgkgy";
    rev = "84151ef8eee8680d8f9597d5f271b8d84ce52714";
  };
  libraryHaskellDepends = [
    base base-compat binary bytestring containers deepseq
    lamdu-calculus lens pretty transformers
  ];
  testHaskellDepends = [
    base base-compat bytestring containers lamdu-calculus lens mtl
    pretty QuickCheck test-framework test-framework-quickcheck2
    transformers
  ];
  benchmarkHaskellDepends = [
    base base-compat bytestring containers criterion deepseq
    lamdu-calculus lens mtl pretty
  ];
  description = "Type inference, extending AlgorithmW step-by-step";
  license = stdenv.lib.licenses.gpl3;
}
