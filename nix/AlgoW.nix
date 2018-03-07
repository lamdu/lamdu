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
    sha256 = "1133vb9xq2z1yl5a2nlza25kpwfj4r9bx5brabi771ygwn8yc27l";
    rev = "f56f7482594e44a998060a279f4ec017241c6e10";
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
