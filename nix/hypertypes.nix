{ mkDerivation, base, base-compat, binary
, constraints, containers, deepseq, fetchFromGitHub, generic-data, lattices, lens, monad-st
, mtl, one-liner, pretty, QuickCheck, show-combinators, template-haskell, th-abstraction
, transformers, lib, generic-constraints
}:
mkDerivation {
  pname = "hypertypes";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "hypertypes";
    sha256 = "18pb6h10r5g8dwamr2zlw1r0yf98ia2nifksg37szvm4rsj4ppxc";
    rev = "9a28e3863b0e2525f38ce7b6efabbc056d3145b2";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq generic-data lattices lens
    monad-st mtl one-liner pretty QuickCheck show-combinators template-haskell
    th-abstraction transformers generic-constraints
  ];
  homepage = "https://github.com/lamdu/hypertypes.git#readme";
  description = "Library for typed ASTs";
  license = lib.licenses.bsd3;
}
