{ mkDerivation, base, base-compat, base16-bytestring, binary
, constraints, containers, deepseq, fetchFromGitHub, lens, monad-st, mtl
, pretty, QuickCheck, template-haskell, th-abstraction, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "0b6g7i48jki9j2a6805ygv9pmbi34s5dljgriz92k6x76csr6y6x";
    rev = "f0fada5f7fcd765fa66ed4948823b0d10c85e2f1";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq lens
    monad-st mtl pretty QuickCheck template-haskell th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
