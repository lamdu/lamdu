{ mkDerivation, base, base-compat, binary
, constraints, containers, deepseq, fetchFromGitHub, generic-data, lattices, lens, monad-st
, mtl, one-liner, pretty, QuickCheck, show-combinators, template-haskell, th-abstraction
, transformers, stdenv, generic-constraints
}:
mkDerivation {
  pname = "hypertypes";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "hypertypes";
    sha256 = "0jbnfxqyqfd17bz9c7d50czc88d0dcxcd2pnyj0z9mynm1jnmv6z";
    rev = "b3382e1d233c3aaa74c7845247d1bd533301a58a";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq generic-data lattices lens
    monad-st mtl one-liner pretty QuickCheck show-combinators template-haskell
    th-abstraction transformers generic-constraints
  ];
  homepage = "https://github.com/lamdu/hypertypes.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
