{ mkDerivation, base, base-compat, binary, bytestring, containers
, criterion, deepseq, fetchgit, lamdu-calculus, lens, mtl, pretty
, QuickCheck, stdenv, test-framework, test-framework-quickcheck2
, transformers
}:
mkDerivation {
  pname = "AlgoW";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/Algorithm-W-Step-By-Step";
    sha256 = "15rv9vr0s8q8f502jd5q6268niq23r4paxkfakrkwr9dizf0plnh";
    rev = "3e46ad5fbe555f62771f44eaddc2f5680866027a";
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
