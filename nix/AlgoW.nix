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
    sha256 = "021nwj2zrsk83k994pffkgy94rysvfbz3vj3v4xplz7ig1vq3w1q";
    rev = "019f40002ac949a1c7f571178c097fc199ecc4ee";
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
