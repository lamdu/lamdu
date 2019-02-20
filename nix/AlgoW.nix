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
    sha256 = "12lv3ls4hvfdcvp0gqgqp5jrj3s2iqw812cyn5sx6mad2rqwxhza";
    rev = "ac8b851c213a1c2138b42bf6d4d13e15d1b70b1b";
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
