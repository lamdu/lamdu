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
    sha256 = "15zk654b18lckcph5rahb0phrsk4fpbsd3wz6lhqyvnmcrsd3yih";
    rev = "6eb15fb8eefffbeec460c3865da767b72cd7f787";
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
