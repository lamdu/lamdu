{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, containers, deepseq, fetchgit, hashable, lens, pretty
, QuickCheck, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/lamdu-calculus.git";
    sha256 = "0l88si5maq8klqgwc4577mqmh5zwk2dx7d0zcbv5qnqrsz9wzv9d";
    rev = "caebaca0e6b66c079a307608ee6dfea4df660d2b";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring containers
    deepseq hashable lens pretty QuickCheck transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
