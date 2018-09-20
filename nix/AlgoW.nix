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
    sha256 = "1pyy2r64mldi6583751jsj94qy9hl636f150hmnf2nmgqz06bhvh";
    rev = "eb03220fb5fb7552e120723e2b32bebbfbb1749d";
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
