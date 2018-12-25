{ mkDerivation, base, base-compat, base16-bytestring, binary
, constraints, containers, deepseq, fetchFromGitHub, lens, monad-st, mtl
, pretty, QuickCheck, show-combinators, template-haskell, th-abstraction
, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "1hja50kp8qwgn9nf16dn7lvq7kgckpin0f8q5927p8q43p23bbcp";
    rev = "8b6e79b37fce6b9288d596c64e4936c660f0e907";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq lens
    monad-st mtl pretty QuickCheck show-combinators template-haskell
    th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
