{ mkDerivation, base, base-compat, base16-bytestring, binary
, constraints, containers, deepseq, fetchFromGitHub, generic-data, lattices, lens, monad-st
, mtl, one-liner, pretty, QuickCheck, show-combinators, template-haskell, th-abstraction
, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "1mnc8kx462vhgpm9729qgqi4wchnk32v2g2v6288774a526xkw0r";
    rev = "f15bbb743808f17f2600eeafdca3cb9c088e2a36";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq generic-data lattices lens
    monad-st mtl one-liner pretty QuickCheck show-combinators template-haskell
    th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
