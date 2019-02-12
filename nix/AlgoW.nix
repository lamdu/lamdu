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
    sha256 = "0bwn69irk3c8khx4rwmnfacip7nn7c8drjwzm8is6wr1gd1l7df7";
    rev = "6ad563cadabf41d1d5c85d1e7b0a473034589fa2";
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
