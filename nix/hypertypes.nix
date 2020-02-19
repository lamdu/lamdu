{ mkDerivation, base, base-compat, base16-bytestring, binary
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
    sha256 = "1pv58xh4nl8y9vxg66k1r21qkqiah616cmckbvjabwasg87l5yia";
    rev = "95925a374169870b09b8d1268e79416fac5f41ab";
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
