{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, containers, deepseq, fetchgit, hashable, lens, pretty
, QuickCheck, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/lamdu-calculus.git";
    sha256 = "1b1s815xj3m31saj8qy2z88s6ibmyfz98hnmy939j75c8w72yrqz";
    rev = "090aeb43105fc79abed3221a8fba26c2fbba046f";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring containers
    deepseq hashable lens pretty QuickCheck transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
