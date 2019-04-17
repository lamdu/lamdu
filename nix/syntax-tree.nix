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
    sha256 = "06j51c044jvkd3vf13fhyr8vglss710kqdzjfkmn525viba6dm9q";
    rev = "9e4e5e5805bccd28b9254c9a286fe384aee69ce2";
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
