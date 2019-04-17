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
    sha256 = "0cjgf127szk74m930f5yi4gg8fcqwi7jkf3as1d8msna04yw2j3j";
    rev = "4e0def63b08b467d52618fd59f20b7020371fbe2";
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
