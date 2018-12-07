{ mkDerivation, base, base-compat, binary, bytestring, containers
, criterion, deepseq, fetchFromGitHub, lamdu-calculus, lens, mtl, pretty
, QuickCheck, stdenv, syntax-tree, test-framework, test-framework-quickcheck2
, transformers
}:
mkDerivation {
  pname = "AlgoW";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "Algorithm-W-Step-By-Step";
    sha256 = "1mq45g4rf61ml6fz2c2jkl9831ydphrj17s3x025cb9b3kxhm0fy";
    rev = "aea6d0d689eec8769fd2a459d726eb2f6f35aa1d";
  };
  libraryHaskellDepends = [
    base base-compat binary bytestring containers deepseq
    lamdu-calculus lens pretty syntax-tree transformers
  ];
  testHaskellDepends = [
    base base-compat bytestring containers lamdu-calculus lens mtl
    pretty QuickCheck test-framework test-framework-quickcheck2
    syntax-tree transformers
  ];
  benchmarkHaskellDepends = [
    base base-compat bytestring containers criterion deepseq
    lamdu-calculus lens mtl pretty
  ];
  description = "Type inference, extending AlgorithmW step-by-step";
  license = stdenv.lib.licenses.gpl3;
}
