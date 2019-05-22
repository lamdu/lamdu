{ mkDerivation, base, base-compat, base16-bytestring, binary
, constraints, containers, deepseq, fetchFromGitHub, lattices, lens, monad-st
, mtl, pretty, QuickCheck, show-combinators, template-haskell, th-abstraction
, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "07hwzi2c0fsyfcip5y4i4vwbiihz2kpcb5cz8vhg0cs8l9w4k8ck";
    rev = "f30124163e1c65725bfbc95193cc793c5aff1179";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq lattices lens
    monad-st mtl pretty QuickCheck show-combinators template-haskell
    th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
