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
    sha256 = "1r8h6w8lwdj9ap52qj3q3bzybm6662q8il57vh7jdic6c5bfzrb4";
    rev = "314a1edd9a70762946da485cb0cfbe6c0c8d50f6";
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
