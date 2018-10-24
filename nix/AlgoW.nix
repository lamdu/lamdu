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
    sha256 = "011kjdffl8qannanf0pxq1mmqd2nfbgqfj1rga5ilyp9gqzca5gj";
    rev = "242cb97d0202e912e275faa10c19b77cd76ec64b";
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
