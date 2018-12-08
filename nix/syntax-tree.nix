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
    sha256 = "13vwgn4xwh4jb2z10q2fgd7a5dmgzamzpr7fh8c7frkpkz1b2a7m";
    rev = "5a2027192833dce3979f552cede25b238e865673";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq lens
    monad-st mtl pretty QuickCheck template-haskell th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
